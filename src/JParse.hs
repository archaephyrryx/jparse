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
components in a larger "Streaming"-based pipeline. There are currently two
primary sub-modules:

* "JParse.Zepto" defines high-level functions for applying a parser to a
monadic 'Data.ByteString.Streaming.ByteString' containing one complete JSON
object per line, returning the results of each successful parse as a 'Streaming.Stream'
of values.

* "JParse.Attoparsec" defines high-level functions for applying a parser to a
monadic 'Data.ByteString.Streaming.ByteString' containg a stream of JSON objects with more flexible
formatting constraints, either returning the successful parse results as a
Stream, or outputting them to stdout one-per-line


== Line-mode ("JParse.Zepto") and Block-mode ("JParse.Attoparsec")

While "JParse.Zepto" and "JParse.Attoparsec" offer similar utility, they
are suitable for JSON data conforming to different format constraints, and
offer different performance.

When the input JSON stream is known to contain exactly one complete JSON
object per line, the Line-Mode module JParse.Zepto" can be used to process
multiple batches of JSON objects independently  using parallel threads. For
data that is formatted in this way, Line-Mode parsing is by far the fastest
parsing strategy.

Alternatively, \"Block-Mode\" parsing using "JParse.Attoparsec" is able to
perform the same task with reduced efficiency but much higher flexibility
in the input format; the input JSON data stream can contain arbitrary valid
whitespace sequences within and between JSON objects without issue.

If \"Line-Mode\" parsing is suitable for any given input, it out-performs \"Block-Mode\"
noticeably, but its format requirements may be too restrictive for some use cases that \"Block-Mode\"
is nevertheless able to operate within.

-}
module JParse
  ( module JParse.Internal
  , module JParse.Attoparsec
  ) where

import JParse.Internal
import JParse.Attoparsec
