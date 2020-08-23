{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Options where

import Data.ByteString (ByteString)

import Data.List (stripPrefix)
import Data.Maybe (isJust, fromJust, mapMaybe)

import JParse (qkey)

import Options.Applicative
import Data.Semigroup ((<>))


type Qstring = ByteString

data Options =
  Options { query  :: Qstring
          , mode   :: Mode
          , zipped :: Bool
          , http   :: Maybe String
          }

getOptions :: Parser Options
getOptions = do
  query <- queryParse
  mode  <-  modeParse
  zipped <- zippedParse
  http <- httpParse
  return Options{..}

queryParse :: Parser Qstring
queryParse = do
  s <- argument str ( metavar "QUERY KEY"
                    <> showDefault
                    <> value "name"
                    )
  return $ qkey s

modeParse :: Parser Mode
modeParse =
  option (maybeReader readMode)
    ( long "mode"
    <> help "mode in which parser pipeline operates, conditional on input data format: (line|block|debug)"
    <> showDefault
    <> value BlockMode
    <> metavar "MODE"
    )

zippedParse :: Parser Bool
zippedParse = switch (long "zipped" <> short 'z' <> help "Whether input is gzip-compressed")

httpParse :: Parser (Maybe String)
httpParse = option (Just <$> str) (long "http-url" <> short 'u' <> value Nothing <> metavar "URL" <> help "url from which input is read instead of stdin")

-- * Command-line specification of input processing mode

-- | Algebraic data type encapsulating the two primary modes of operation:
--
-- * @BlockMode@ for data that is either formatted with multiple objects per line, multiple lines per object, or arbitrarily long lines that are not safe to read into memory as a unit.
--
-- * @LineMode@ for (theoretically) more efficient parsing of data where every line contains exactly one complete JSON object, and lines are trusted to be sufficiently short to permit reading entire lines into memory as a unit.
--
-- * Additionally supports @DebugMode@ for human-readable printing of the raw return values of line-mode parser, rather than printing them in a machine-readable way after filtering and eliminating extraneous constructors around the actual result value.
data Mode = BlockMode -- ^ Attoparsec inside Conduit
          | LineMode -- ^ Zepto inside Streaming
          | DebugMode -- ^ Zepto inside Streaming (verbose)
          deriving (Eq)

instance Show Mode where
  show BlockMode = "block"
  show LineMode = "line"
  show DebugMode = "debug"

readMode :: String -> Maybe Mode
readMode x =
  if | x == "line"  -> Just LineMode
     | x == "block" -> Just BlockMode
     | x == "debug" -> Just DebugMode
     | otherwise   -> Nothing