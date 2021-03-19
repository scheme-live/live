# `(import (okvslite datacab))`

![Fast taxi with popeye graffiti in the background.](carl-joseph-jASSGNBObNY-unsplash.jpg)

## Status

**Early draft.**

## Issues

## Introduction

### Abstract

### Rational

There is no embedded okvs that covers exactly the following use-cases:

- ACID transactions,
- Minimal memory usage,
- Estimated key count,
- Estimated bytes count,

SQLite LSM is the nearest, even if it does not support Estimated key
and bytes count.  Outside the fact that SQLite LSM extension support
multiple POSIX processus which has little interest when multiple POSIX
threads are available, there is the problem that there is not much
interest to maintain it (and there are infelicities).

ref: https://sqlite.org/src4/doc/trunk/www/lsm.wiki

### Goals

- Implement okvslite interface,
- Easily support multiple Scheme implementations (With a C FFI
  implementation, it requires `open`, `mmap`, `flush`, and `close`).

### Non-goals

- No multiple POSIX processus support.

## Reference
