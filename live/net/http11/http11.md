# SRFI nnn: HTTP 1.1

by Amirouche Boubekki, Another Person, Third Person

# Status

Early Draft

# Abstract

HTTP/1.1 is a widespread protocol to communicate on the Internet, and
the basis of the web.  This library offers the routine necessary, to
read and write HTTP/1.1 and HTTP/1.0 without assuming a particular
implementation of network sockets.

# Issues

- The generators / accumulators might raise connection close or in the
  case of the generator return eof. Is that really a problem?

# Rationale

HTTP is an essential protocol to communicate on the Internet. This
library expose the necessary procedures to build HTTP clients and
servers.

# Specification

## `(http-error? obj)`

Returns `#t` if `OBJ` is an HTTP error. Otherwise, it returns `#f`.

## `(http-error-message OBJ)`

Return a human readable string describing the error.

## `(http-error-irritants OBJ)`

Return an object that can help understand the error or possibly
workaround it.

## `(http-request-read generator) procedure? → string? string? pair? pair? procedure?`

Read a HTTP request from `GENERATOR` that produces bytevectors, and
returns five values: method, uri, version, headers and body.

The only argument `GENERATOR` must yield bytevectors. It may raise
errors, but those are unspecified, and must not be silenced by
`http-request-read`.

The five returned values are the following:

- method: a string describing the HTTP method, it should not be
  checked, in other words according to this library any method
  is valid;

- uri: a string describing the uri from the request line;

- version: a pair of numbers describing the HTTP version;

- headers: an association list of string key and string value pairs;

- body: a generator that yields pairs, the `car` is a string that is
  the chunk extension that may be the empty string, and the `cdr` is a
  bytevector as a scheme representation of the chunk. The last
  returned pair before end-of-file may be trailer, in that case the
  `car` is false, and the `cdr` is an association list representing
  the trailing headers. When the HTTP request is not chunked, on the
  first call `BODY` will return a pair made of the empty string, and a
  bytevector, the next call to `BODY` will return the eof-object. That
  is the `car` is always a string, except when the HTTP request is
  chunked, and there is a trailer. If the HTTP request is chunked and
  has not trailer, `BODY` must return an end-of-file after the last
  chunk.

## `(http-request-read* generator)`

Read a HTTP request from `GENERATOR` that produces bytevectors, and
returns five values: `method`, `uri`, `version`, `headers`, and
`body`. Those `method`, `uri`, and `version` are the same as
`http-request-read`. `headers` will contain both headers, and if any
trailers. `body` must be a bytevector. If the request is chunked,
chunk extensions are ignored, and `body` is chunks bodies that are
appended.

## `(http-request-write accumulator method uri version headers body)`

Write a HTTP request to `ACCUMULATOR` based on `METHOD`, `URI`,
`VERSION`, `HEADERS`, and `BODY`.

The argument `ACCUMULATOR` must accept a bytevector as argument. The
other arguments are specified as follows:

- `METHOD` a string;

- `URI` a string;

- `VERSION` a pair of positive integers;

- `HEADERS` an association list of string key and value pairs;

- `BODY` a generator that yields pairs and mirror the behavior of
  `body` in `http-request-read` or a bytevector in case the request
  is not chunked.

If `BODY` is a generator and the chunked headers are not present it is
an error. If `BODY` is a bytevector, and the chunked headers is
present it is an error.

## `(http-response-read generator) procedure? → pair? positive-integer? string? pair? procedure?`

Read an HTTP request from `GENERATOR`, it returns version, code,
reason, headers and the body.

The only argument `GENERATOR` must produce bytevectors. The
returned values are specified as follows:

- version: a pair of positive integers;

- code: a positive integer;

- reason: a string;

- headers: an association list of string key and value pairs;

- body: a generator that yields a byte at a time. Whether the reponse
  body is chunked or not must not make a difference, the generator
  body will yield bytes of the underlying payload;

## `(http-response-write accumulator version code reason headers body)`

Write a HTTP response to `ACCUMULATOR` based on `VERSION`, `CODE`,
`REASON`, `HEADERS`, and `BODY`.

The argument `ACCUMULATOR` must accept one byte at a time as
argument. The other arguments are specified as follows:

- `VERSION` a pair of positive integer;

- `CODE` a positive integer;

- `REASON` a string;

- `HEADERS` an association list of string key and value pairs;

- `BODY` a generator that yields pairs;

## `(http-response-write* accumulator version code reason headers body)`

# Implementation

??? explanation of how it meets the sample implementation requirement
(see process), and the code, if possible, or a link to it Source for
the sample implementation.

# Acknowledgements

??? Give credits where credits is due.

# References

??? Optional section with links to web pages, books and papers that
helped design the SRFI.

# Copyright

Copyright (C) Firstname Lastname (20XY).

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
