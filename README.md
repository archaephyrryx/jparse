# jparse

**jparse** is an experimental package consisting of a specialized library designed to parse a large volume of JSON objects in a consistent fashion. Unlike other
libraries that provide JSON parsers, the use-pattern this project was designed to facilitate is specific enough to obviate any need for storing Haskell-based
representations of JSON objects or values.

The original and current scope of this library corresponds to a single use-case, in which the desired operation is bulk-extraction of the values associated with a single (constant) **query-key** from an input stream consisting of many individual JSON objects. As such, this project does not support extracting more than one value from any object, and is currently limited to extracting string-valued elements.

Approach
========

Each JSON object is parsed in a single pass with minimal[1] backtracking, matching every key encountered against the constant **query-key** and efficiently skipping to the next key upon non-match, repeating this process until the object terminates or a match is found. Upon encountering a matching key[2], the associated value is parsed and returned, and the trailing remainder of the JSON object is skipped as efficiently as possible. This process is repeated for each JSON object, transforming a stream of unparsed JSON into a stream of extracted values. 

The division of functionality between the library and executable modules has not yet been fully tuned, and therefore any project importing the respective library may find some desired features missing if they happen to be defined in modules specific to the executable. Please contact the maintainer of this project regarding any desired features you wish to be exposed in the library rather than the executable.

[1]: The library defines several variant approaches to the task of parsing a JSON object, which variously balance sanity-checking and efficiency. The most efficient versions only ensure the structural sanity of the JSON object in terms of properly delimited nested objects and arrays, while sacrificing the majority of integrity-checking of string contents and scalar values in order to optimize performance. Currently, the executable included with this package uses non-backtracking variants exclusively.

[2]: Even if multiple keys exist in the same object that would theoretically match against the given **query-key** (which would constitute malformed JSON in any case), at most one match is considered per object, and therefore the sequentially earliest match is the only one properly detected; any later hypothetical matches are skipped.

Character Encoding
==================

As JSON itself allows UTF-16 codepoints within the BMP range (including ASCII-range codepoints) to be represnted either directly as glyphs or as hexadecimal tetragraphs of the form `\u0000`--`\uffff` (case-insensitively), as well as UTF-16 Surrogate Pairs, comparing a **query-key** against an arbitrary key inside of each JSON object is not a byte-for-byte equality test. In order to support non-ASCII characters in *query-key*s, as well as indirect representations of ASCII-range codepoints even in
ASCII-only JSON objects, each **query-key** is preemptively converted into a sequence of canonical code-point representations (specifically values of type `ParseClass` as defined in the library module [Parse.Match.Internal](/src/Parse/Match/Internal.hs)) that are either transparent in their possible representations in highly specific cases, or carry values indicating their alternative representations (i.e. as glyphs or as hexadecimal tetragraphs).

The process of matching a given JSON key against this canonicalized **query-key** then consists of iteratively attempting to parse one of the byte-sequences that corresponds to a valid encoding of a canonicalized character, short-circuiting after the first failed attempt.

While it would theoretically be possible to attain a marginal performance improvement by specializing for cases where it is known in advance how the queried key is represented inside each JSON object, such a specialization is unplanned at this time, as the process of matching against a given key
is a cheap operation when compared to the overhead for I/O operations and the process of parsing (or skipping) the contents of the JSON object as a whole.

Executable Usage
================

The current implementation of the executable included in this project reads JSON data from standard input and prints extracted values to standard output. The standard input is expected to be formatted as a stream of JSON objects, separated only by whitespace or newlines. This input may either be provided raw, or compressed using the zlib codec (e.g. using gzip or similar) provided that the `--zipped` (shortened to `-z`) flag is used.

The only non-option argument to the executable is the **query-key**, which defaults to "name" (for historical reasons).

Option parsing is done using the `optparse-applicative` library, meaning that most of the details regarding executable usage are more reliably obtained via the `--help` flag of the executable, which reflects ongoing changes.
