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
monadic 'Data.ByteString.Streaming.ByteString' containing one complete JSON
object per line, returning the results of each successful parse as a Stream
(from "Streaming") of values.

* "JParse.Driver" defines a high-level function for applying a parser to a
monadic 'Data.ByteString.Streaming.ByteString' containing one complete JSON
object per line, directly outputting the successful parse results to stdout,
one-per-line

* "JParse.Attoparsec" defines high-level functions for applying a parser to a
monadic 'Data.ByteString.Streaming.ByteString' containg a stream of JSON objects with more flexible
formatting constraings, either returning the successful parse results as a
Stream, or outputting them to stdout one-per-line


== Line-mode ("JParse.Zepto") and Block-mode ("JParse.Attoparsec")

While "JParse.Zepto" and "JParse.Attoparsec" offer similar utility, they
are suitable for JSON data conforming to different format constraints, and
offer different performance.

When the input JSON stream is known to contain exactly one complete JSON
object per line, it is possible to use the "JParse.Zepto" (or "JParse.Driver")
pipeline components, which operate in \"Line-Mode\", using parallel threads to process multiple
batches of JSON objects independently; the parser-library these functions use
is the highly optimized "JParse.Parser.Zepto" library, which achieves its efficiency
by relying on the guarantee that newline characters are consistent end-of-object markers
for the input in question. For data that is formatted in this way, Line-Mode parsing
is by far the fastest parsing strategy.

If the input stream is formatted differently, such as if the JSON objects are \'prettified\'
by using object-internal newlines, or if more than one object can occur per line, Line-Mode
parsing is unsuitable. For such cases, \"Block-Mode\" parsing using "JParse.Attoparsec" is
still possible, but is unable to use parallel computation strategies due to the lack of an
end-of-object marker.

-}
module JParse
  ( module JParse.Internal
  , module JParse.Attoparsec
  ) where

import JParse.Internal
import JParse.Attoparsec
