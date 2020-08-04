{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JParse where

import qualified Conduit as C
import           Conduit ((.|))
import           Control.Monad (mzero)
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A (isSpace_w8)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as D
import           Data.ByteString.Builder (Builder)
import           System.IO (stdout)

import Data.Function (fix)
import Data.Void (Void)

import Parse hiding (fail)
import qualified Parse.ReadAlt as R

import qualified Parse.MatchZepto as Zep
import qualified Parse.ReadZepto as Zep
import qualified Parse.Parser.Zepto as Z
import qualified Parse.Parser as Z

import qualified Parse.MatchStream as ZepS
import qualified Parse.ReadStream as ZepS
import qualified Parse.Parser.ZeptoStream as ZS
import qualified Parse.Parser.Stream as ZS


-- | query key function: extracts a query bytestring from command line argument list
--   default if no arguments found is "name" for historical reasons
qkey :: [String] -> ByteString
qkey = \case
    a:_ -> T.encodeUtf8 . T.pack $ a
    _   -> "name"
{-# INLINE qkey #-}

-- | print a Builder to stdout with a trailing newline
putLnBuilder :: MonadIO m => Maybe Builder -> m ()
putLnBuilder Nothing = pure ()
putLnBuilder (Just b) = liftIO $ D.hPutBuilder stdout (b <> D.word8 0xa)
{-# INLINE putLnBuilder #-}

putLnBuilderC :: MonadIO m => C.ConduitT (Maybe Builder) Void m ()
putLnBuilderC = C.mapM_C putLnBuilder
{-# INLINE putLnBuilderC #-}

-- | Strip leading whitespace from a ByteString
trim :: ByteString -> ByteString
trim !bs = B.dropWhile A.isSpace_w8 bs
{-# INLINE trim #-}

-- | Run 'parseC' using a given parser over arbitrary upstream
-- and output the results using 'putLnBuilderC'
runParse :: (ByteString -> A.Result (Maybe Builder))
          -> C.ConduitT () ByteString IO ()
          -> IO ()
runParse = runParseWithC putLnBuilderC
{-# INLINE runParse #-}

-- | Run 'parseC' using a given parser over arbitrary upstream
-- and output the results using arbitrary function
runParseWithC :: (MonadIO m, MonadFail m)
              => C.ConduitT (Maybe Builder) Void m a -- ^ sink on Maybe Builder values
              -> (ByteString -> A.Result (Maybe Builder)) -- ^ parse function
              -> C.ConduitT () ByteString m () -- ^ input conduit
              -> m a
runParseWithC mc parser source = C.runConduit $ source .| runParseC .| mc
  where
    {-# INLINE runParseC #-}
    runParseC = C.await >>= \case
      Just bs ->
        let bs' = trim bs
            streamEmpty = False
            cleanState = True
         in if B.null bs'
               then runParseC
               else parseC streamEmpty cleanState parser (parser bs')
      _       -> pure ()
{-# INLINE runParseWithC #-}


-- | Conduit that feeds upstream ByteStrings into a Parser and yields Maybe Builders from successful parses
--
--   Recurses until end of output is reached and retrieves additonal ByteString
--   output from upstream each pass, until parser yields Done or Fail result.
--
--   Designed around 'seekInObj', which has the property that as soon as a positive or
--   negative result has been decided for each JSON object encountered, the remainder of
--   that JSON object is skipped.
parseC :: (MonadIO m, MonadFail m)
       => Bool -- ^ has upstream been fully consumed
       -> Bool -- ^ is the parse-state clean (not mid-parse)
       -> (ByteString -> A.Result (Maybe Builder)) -- ^ primary parser
       -> A.Result (Maybe Builder) -- ^ most recent parse result
       -> C.ConduitT ByteString (Maybe Builder) m ()
parseC atEnd clean parser res =
  case res of
    A.Done leftover result -> do
      let clean' = not atEnd -- we are mid-parse if upstream is exhausted
      C.yield result
      if | more <- trim leftover
         , not $ B.null more -> parseC atEnd clean' parser $! parser more -- recurse on non-empty leftover bytestring
         | atEnd -> pure () -- upstream fully consumed and no leftover input
         | otherwise -> C.await >>= \case
            Nothing -> pure ()
            Just bs | !bs' <- trim bs
                    -> parseC False (B.null bs') parser $! (parser $! bs') -- recurse over upstream
    A.Partial cont
      | atEnd && clean -> pure ()
      | atEnd -> parseC atEnd clean parser $! cont B.empty -- continue on empty bytestring when mid-parse
      | otherwise
      -> C.await >>= \case
            Nothing -> parseC True clean parser $! res -- mark end-of-input and retry
            Just bs | more <- trim bs
                    , not $ B.null more
                    -> parseC False False parser $! cont more -- continue on upstream ByteString
                    | otherwise
                    -> parseC False clean parser $! res -- retry upstream when output only whitespace
    A.Fail i ctx e -> fail $ show (i, ctx, e)

-- | Extract bytestring-valued key from a JSON object
--
--   Argument is a list of ParseClass values corresponding to
--   query key, obtained most likely through `mapClass`
seekInObj :: [ParseClass] -> A.Parser (Maybe Builder)
seekInObj cs = do
    symbol LBrace
    A.anyWord8 >>= \case
        RBrace -> pure Nothing
        Quote -> getStringValue cs
        _    -> mzero
{-# INLINE seekInObj #-}

-- | version using 'getStringValue\''
seekInObj' :: [ParseClass] -> A.Parser (Maybe Builder)
seekInObj' cs = do
    symbol LBrace
    A.anyWord8 >>= \case
        RBrace -> pure Nothing
        Quote -> getStringValue' cs
        _    -> mzero
{-# INLINE seekInObj' #-}

-- | version using 'getStringValueZepto'
seekInObjZepto :: [ParseClass] -> Z.Parser (Maybe Builder)
seekInObjZepto cs = do
    Z.symbol LBrace
    Z.pop >>= \case
        RBrace -> pure Nothing
        Quote -> getStringValueZepto cs
        _    -> mzero
{-# INLINE seekInObjZepto #-}

-- | version using 'getStringValueZeptoStream'
seekInObjZeptoStream :: [ParseClass] -> ZS.Parser (Maybe Builder)
seekInObjZeptoStream cs = do
    ZS.symbol LBrace
    ZS.pop >>= \case
        RBrace -> pure Nothing
        Quote -> getStringValueZeptoStream cs
        _    -> mzero
{-# INLINE seekInObjZeptoStream #-}

-- | Extract bytestring-valued key from a sequence of key-value pairs inside a
-- JSON object, then consume and discard the tail of the object throug the
-- closing brace.  The starting position is immediately after the initial open
-- quote character.
--
getStringValue :: [ParseClass] -> A.Parser (Maybe Builder)
getStringValue ckey = do
    this <- parseMatch ckey
    if this
       then do symbol Colon <* A.word8 Quote
               Just <$> parseToEndQ <* skipRestObj
       else do symbol Colon *> skipValue
               token >>= \case
                  Comma -> A.word8 Quote *> getStringValue ckey
                  RBrace -> pure Nothing
                  _ -> mzero
{-# INLINE getStringValue #-}

-- | version using 'parseMatchAlt' and Parse.ReadAlt variant functions
getStringValue' :: [ParseClass] -> A.Parser (Maybe Builder)
getStringValue' ckey = do
    this <- parseMatchAlt ckey
    if this
       then do symbol Colon <* A.word8 Quote
               Just <$> R.parseToEndQ <* R.skipRestObj
       else do symbol Colon *> R.skipValue
               token >>= \case
                  Comma -> A.word8 Quote *> getStringValue' ckey
                  RBrace -> pure Nothing
                  _ -> mzero
{-# INLINE getStringValue' #-}

-- | version using 'Zep.parseMatch' and Parse.ReadZepto variant functions
getStringValueZepto :: [ParseClass] -> Z.Parser (Maybe Builder)
getStringValueZepto ckey = do
    this <- Zep.parseMatch ckey
    if this
       then do Z.symbol Colon <* Z.word8 Quote
               Just <$> Zep.parseToEndQ <* Zep.skipRestObj
       else do Z.symbol Colon *> Zep.skipValue
               Z.token >>= \case
                  Comma -> Z.word8 Quote *> getStringValueZepto ckey
                  RBrace -> pure Nothing
                  _ -> mzero
{-# INLINE getStringValueZepto #-}

-- | version using 'ZepS.parseMatch' and Parse.ReadStream variant functions
getStringValueZeptoStream :: [ParseClass] -> ZS.Parser (Maybe Builder)
getStringValueZeptoStream ckey = do
    this <- ZepS.parseMatch ckey
    if this
       then do ZS.symbol Colon <* ZS.word8 Quote
               Just <$> ZepS.parseToEndQ <* ZepS.skipRestObj
       else do ZS.symbol Colon *> ZepS.skipValue
               ZS.token >>= \case
                  Comma -> ZS.word8 Quote *> getStringValueZeptoStream ckey
                  RBrace -> pure Nothing
                  _ -> mzero
{-# INLINE getStringValueZeptoStream #-}
