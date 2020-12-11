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
formatting constraints, returning the successful parse results as a
Stream or as a single value computed by folding over a Stream.


== Line-mode ("JParse.Zepto") and Block-mode ("JParse.Attoparsec")

\"Line-Mode\" is a JSON parsing strategy designed and optmized for input streams
known to contain exactly one complete JSON
object per line. The module "JParse.Zepto", which uses this strategy, is able to process
multiple batches of JSON objects in parallel.

\"Block-Mode\" is an alternate JSON parsing strategy that is more flexible with regard to input formatting.
The module "JParse.Attoparsec", which uses this strategy, is able to process JSON data streams containing arbitrary valid
whitespace sequences within and between JSON objects without issue.

If \"Line-Mode\" parsing is suitable for any given input, it out-performs \"Block-Mode\"
noticeably, but its format requirements may be too restrictive for some use cases that \"Block-Mode\"
is nevertheless able to operate within.

-}
module JParse
  ( module JParse.Internal
  , module JParse.Attoparsec
  , module JParse.Zepto
  ) where

import JParse.Internal
import JParse.Attoparsec
import JParse.Zepto
