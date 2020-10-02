{-|
Module      : JParse
Description : Efficient parser library for single-key queries of JSON streams
Copyright   : (c) Peter Duchovni, 2020
License     : BSD-3
Maintainer  : caufeminecraft+github@gmail.com

= jparse

@jparse@ is a JSON-parsing library optimized for performing a single-key
string-value lookup over every object in a stream of JSON data. This
specificity obviates the need for construction of in-memory representations of
JSON objects, instead performing a linear scan over each object as efficiently
as possible, extracting only the contents of the string associated with a
fixed __query-key__ and skipping all non-matching key-value pairs.

"JParse" and its submodules define high-level functions to be used as
components in a larger "Streaming"-based pipeline. There are currently three
primary sub-modules:

* "JParse.Zepto" defines high-level functions for applying a parser to a
monadic ByteString (from "Data.ByteString.Streaming") and returning the
results of each successful parse as a Stream (from "Streaming") of values.

* "JParse.Driver" defines a high-level function for applying a parser to a
monadic ByteString and directly outputting the successful parse results to
stdout, one-per-line

* "JParse.Attoparsec" defines high-level functions for applying a parser to a
monadic ByteString and either returning the successful parse results as a
Stream, or outputting them to stdout one-per-line


== Line-mode ("JParse.Zepto") and Block-mode ("JParse.Attoparsec")

The key differences between "JParse.Zepto" and "JParse.Attoparsec" are the
actual parser-combinator library they are implemented with respect to, and the
assumptions made about the format and structure of the incoming JSON data.
Furthermore, the different parser-combinator libraries allow for different
computation strategies, and can have vastly different performance over the
same input.

"JParse.Zepto" is implemented using the customized "Parse.Parser.Zepto"
module, which allows for high-performance parsing without backtracking or
continuation passing. In order to achieve this performance, the input to the
top-level parser combinator (viz. 'seekInObjZepto') must be a complete JSON
object. This parser sacrifices the majority of JSON validation in order to be
as efficient as possible when processing valid JSON data, and may fail to
recognize malformed JSON. In order to obtain complete JSON objects, every
literal newline character (@\\n@) in the input JSON stream is treated as an
end-of-object marker, as newlines can only ever occur outside of JSON keys and
values. This means, however, that the Zepto-based \"Line-mode\" parser (as it
is referred to throughout this library) is only suitable for input in which
each JSON object occurs on a separate line, and newlines are only used to
seperate top-level objects and do not occur inside of objects themselves. It
also means that for parsing to occur, each line must be read into memory as a
single unit, which may be impractical or unsafe if the input is malicious or
may contain excessively long lines.

When line-mode parsing is possible, however, it allows for concurrent
processing of large batches of lines taken from the input stream, and vastly
outperforms equivalent operations using other JSON-stream parsers.

A variant mode of JSON stream-parsing, namely Block-mode parsing implemented
in terms of "Parse.Parser.Attoparsec", is suitable in cases where the JSON
input is not formatted one-object-per-line, as it uses continuation-passing to
parse the JSON-stream in chunks rather than lines. As newline characters are
not used as end-of-object oracles, this mode of parsing does not operate in
parallel, as the boundaries between objects cannot be determined in advance.
While this yields sub-optimal performance, block-mode parsing is able to more
strictly validate the JSON data it processes, and can be used in cases where
Line-mode parsing is unsuitable.

While Line-mode and Block-mode parsing are nominally used only for JSON-streams,
if they are provided suitable parsers implemented in terms of the appropriate library,
it is possible to parse arbitrary content using "JParse.Zepto" and "JParse.Attoparsec".

-}
module JParse
  ( module JParse.Internal
  , module JParse.Attoparsec
  ) where

import JParse.Internal
import JParse.Attoparsec
