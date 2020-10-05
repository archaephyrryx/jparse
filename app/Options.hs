{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Options where

import Data.ByteString (ByteString)
import Options.Applicative


type Qstring = String

data Options =
  Options { query  :: Qstring
          , mode   :: Mode
          , zipped :: Bool
          , gated  :: Bool
          , http   :: Maybe String
          }

getOptions :: Parser Options
getOptions = do
  query <- queryParse
  mode  <-  modeParse
  zipped <- zippedParse
  gated <- gatedParse
  http <- httpParse
  return Options{..}

queryParse :: Parser Qstring
queryParse =
  argument str
    ( metavar "QUERY KEY"
    <> showDefault
    <> value "name"
    )

modeParse :: Parser Mode
modeParse =
  option (maybeReader readMode)
    ( long "mode"
    <> help "mode in which parser pipeline operates, conditional on input data format: (line|block)"
    <> showDefault
    <> value defaultMode
    <> metavar "MODE"
    )

zippedParse :: Parser Bool
zippedParse = switch (long "zipped" <> short 'z' <> help "Whether input is gzip-compressed [line-mode only]")

gatedParse :: Parser Bool
gatedParse = switch (long "gated" <> short 'g' <> help "Whether to 'gate' input pre-processing (for http and/or gzip) [line-mode only]")

httpParse :: Parser (Maybe String)
httpParse = option (Just <$> str) (long "http-url" <> short 'u' <> value Nothing <> metavar "URL" <> help "url from which input is read instead of stdin [line-mode only]")

-- * Command-line specification of input processing mode

-- | Algebraic data type encapsulating the two primary modes of operation:
--
-- * @BlockMode@ for data that is either formatted with multiple objects per line, multiple lines per object, or arbitrarily long lines that are not safe to read into memory as a unit.
--
-- * @LineMode@ for more efficient parsing of data where every line contains exactly one complete JSON object, and lines are trusted to be sufficiently short to permit reading entire lines into memory as a unit.
--
-- Various options and associated functionality of the executable (i.e. gzip decompression and reading data over HTTP) are only supported in LineMode
data Mode = BlockMode -- ^ Attoparsec-based Streaming pipeline
          | LineMode -- ^ Zepto-based Streaming pipeline
          deriving (Eq)

defaultMode :: Mode
defaultMode = LineMode
{-# INLINE defaultMode #-}

instance Show Mode where
  show BlockMode = "block"
  show LineMode = "line"

readMode :: String -> Maybe Mode
readMode x =
  if | x == "line"  -> Just LineMode
     | x == "block" -> Just BlockMode
     | otherwise   -> Nothing
