{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Conduit as C
import           Conduit ((.|))
import           Control.Applicative ((<|>))
import           Control.Monad (mzero, void, when)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A (isDigit_w8, isSpace_w8, skipSpace)
import qualified Data.Attoparsec.Text as T (char)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as D
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Char8 as S8
import           Data.Word (Word8)
import qualified System.Environment as Sys
import           System.IO (stdout)

import Parse

fix :: (a -> a) -> a
fix f = let x = f x in x

main :: IO ()
main = do
   key <- qkey <$> Sys.getArgs
   C.runConduit $ C.stdinC .| fix \retry -> do
       fmap trim <$> C.await >>= \case
           Just bs | B.null bs -> retry
                   | otherwise -> go key bs
           Nothing             -> pure ()
  where
    qkey args = case args of
        a:_ -> S8.pack a
        _   -> "name"

    go key = loop False True . parser
      where
        ckey   = mapClass key
        parser = A.parse (seekInObj ckey)

        -- | Incrementally parse a stream of JSON values, extracting a single
        -- key from each, and restarting the parser at the end of each object.
        loop False _ (A.Done leftover result) = do
            C.liftIO $ mapM_ (D.hPutBuilder stdout . (<> D.word8 0xa)) result
            if | more <- trim leftover
               , not $ B.null more -> loop False True $! parser more
               | otherwise -> fmap trim <$> C.await >>= \case
                   Nothing -> pure ()
                   Just bs -> loop False (B.null bs) $! parser $! bs
        loop False clean (A.Partial c) = C.await >>= \case
            Nothing -> loop True clean $! A.Partial c
            Just bs | more <- trim bs
                    , not $ B.null more -> loop False False $! c more
                    | otherwise -> loop False clean $! A.Partial c
        loop True _ (A.Done leftover result) = do
            C.liftIO $ mapM_ (D.hPutBuilder stdout . (<> D.word8 0xa)) result
            if | more <- trim leftover
               , not $ B.null more -> loop True False $! parser more
               | otherwise -> pure ()
        loop True True (A.Partial _) = pure ()
        loop True False (A.Partial c) = loop True False $! c mempty
        loop _ _ (A.Fail i ctx e) = fail $ show (i, ctx, e)

    trim ! bs = B.dropWhile A.isSpace_w8 bs

-- | Extract bytestring-valued key from a JSON object
--
seekInObj :: [ParseClass] -> A.Parser (Maybe Builder)
seekInObj cs = do
    symbol 0x7b
    A.anyWord8 >>= \case
        0x7d -> pure Nothing
        0x22 -> getStringValue cs
        _    -> mzero

-- | Extract bytestring-valued key from a sequence of key-value pairs inside a
-- JSON object, then consume and discard the tail of the object throug the
-- closing brace.  The starting position is immediately after the initial open
-- quote character.
--
getStringValue :: [ParseClass] -> A.Parser (Maybe Builder)
getStringValue ckey = do
    this <- parseMatch ckey
    if this
        then do symbol 0x3a <* A.word8 0x22
                Just <$> parseToEndQ <* skipQuick 0x7d
        else do symbol 0x3a *> skipValue
                A.eitherP (symbol 0x2c) (symbol 0x7d) >>= \case
                    Left  _ -> (A.word8 0x22) *> getStringValue ckey
                    Right _ -> pure Nothing
