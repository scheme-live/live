# `(import (okvslite))`

![The abstraction of architecture of the Abu Dhabi Louvre Museum ceiling it’s a piece of art on it’s own.](alvaro-pinot-czDvRp5V2b0-unsplash.jpg)

## Status

**Rework draft.**

## Issues

- Missing record types for cursor and transaction
- No facility to implement
  [HightContentionAllocator](https://activesphere.com/blog/2018/08/05/high-contention-allocator),
  see also FDB's [Directory layer](https://git.io/JqCFP)
- Add page read and write middlewares to support compression and
  cryptography.
- Transaction begin, rollback, before and after commit hooks
- No builtin support for hooks / watches / triggers (may be
  unnecessary if transaction hooks exists).
- Transaction retry?
- In `make-okvslite` replace `args` with `make-okvslite-options`. Max
  key and value size would be default values for their related option.
- create a super predicate for db, transaction or cursor?

## Introduction

### Abstract

TODO

### Background

This project began in 2011 when @amirouche started looking for the
perfect database to build the web application that would make Google a
relic of the past.

Along the road they built several projects, in particular the [SRFI
167]() and [SRFI 168](). The current work build and hopefully improve
upon that. Together with the extensions, it will replace the need for
most of SQL database features and extend them to support text search.

### Rational

> For the projects I build I want to avoid changing syntax every now
> and then. "Now and then" in backend development can be minutes to
> seconds when you deal with an SQL database, or JSON wrapped in wanna
> be *human human-machine* interface. Even more so with full-stack
> development.  Ordered Key-Value Store offer that opportunity to
> write everything in your favorite programming language. Remain a
> question: What about performance?
>
> @amirouche

### Goals

- Support multiple backend storage
- Benchmark several backend storage
- Benchmark against other database systems

### Non-goals

- No support for FoundationDB and other distributed OKVS.

### Related

#### Extensions

- [counter](counter/#readme)
- [bag](bag/#readme)
- [set](set/#readme)
- [mapping-multi](mapping-multi/#readme)
- [bstore](bstore/#readme): Binary object store
- [ustore](ustore/#readme): uid allocator
- [eavstore](eavstore/#readme): Entity-Attribute-Value
- [nstore2](nstore2/#readme): Generic tuple store 2
- [vnstore2](vnstore2/#readme): Versioned generic tuple store
- kstore: Ranked set / leaderboard / priority queue
- fstore: Approximate bytes lookup
- xzstore: XZ-ordered curve
- [scstore](scstore/#readme): Semantic text search

#### Others

In memory storage:

- The R7RS library `(scheme mapping)` aka
  [SRFI-146](https://srfi.schemers.org/srfi-146/)
- Log-bassed well-balanced binary-tree

Packing and unpacking:

- exact match `bytesomatic`
- natural order `lexicographic`

## Reference

### `okvslite-key-max-size` parameter

> *alpha* in theory sqlite-lsm-ext and wiredtiger do not have a direct
> key or value limit. In the case of wiredtiger, there is a
> transaction max size. In both cases it is necessarly bound by
> available memory. In practice with sqlite-lsm-ext the database will
> crash some time when the key is too big. It might be a bug with
> sqlite lsm extension. FDB has a limit on both. All that leads me to
> think that it might be better to document a max size for
> both. Possibly make it the default value, and allow the user to
> override it in `make-okvslite`.

### `okvslite-value-max-size` parameter

> *alpha* see above

### `(make-okvslite . args) (every any?) → okvslite?` generic

- keyword: constructor

Return a handle of the configured backend storage.

### `(okvslite-estimate-bytes-size db [key [other]]) okvslite? bytevector? bytevector? → integer?` generic

> **alpha**
>
> Does it need to be fast? Not sure what is the use-case outside
> reporting the end-user the size of a "folder". In the case of
> foundationdb it allows (among other things?) to predict whether a
> transaction needs to be split into multiple transaction (because
> transaction max size would be reached, without resorting or reducing
> the need for error handling inside the transaction.

Return the size of key-value pairs of the specified subspace in bytes.

### `(okvslite-estimate-key-count db [key other]) okvslite? bytevector? bytevector? → integer` generic

> **alpha**
>
> To be able to implement a query optimizer, some statistic are
> required, those might be implemented on top okvslite, like mongodb
> does on its own without help from wiredtiger. It seems like it would
> be better to implement that at the storage layer.
>
> That procedure would be useful to implement query optimizers.

### `(okvslite-set! db key value) okvslite? bytevector? bytevector?` generic

Associate to the bytevector `KEY`, with the bytevector `VALUE`.

### `(okvslite-remove! db key) okvslite? bytevector?` generic

Removes the bytevector `KEY`, and its associated value.

### `(okvslite-close! db) okvslite?` generic

Close database.

### `(okvslite-in-transaction! db proc [failure [success]]) okvslite? procedure? procedure? procedure? → (values (every any?))` generic

Begin a transaction against `DB`, execute `PROC`. In case of error,
rollback the transaction and execute `FAILURE` with no
arguments. Otherwise, executes `SUCCESS` with the returned values of
`PROC`.

### `(okvslite-transaction-timestamp transaction) okvslite-transaction? → bytevector?` generic

Return the timestamp of the given `TRANSACTION` as bytevector of 16
bytes using big-endian. This is not necessarly monotonically
increasing timestamp (there might be gaps) and does not necessarly
relate to world clock, but it increments across database open and
close.

### `(okvslite-call-with-cursor db proc) okvslite? procedure? → (values (every any?))` generic

Open a cursor against `DB` and call `PROC` with the cursor
handle. When `PROC` returns, the cursor is closed.

### `(okvslite-cursor-seek cursor strategy key) okvslite-cursor? symbol? bytevector? → symbol?` generic

Position the `CURSOR` using `STRATEGY`. `STRATEGY` can be one of the
following symbol:

- `less-than-or-equal`
- `equal`
- `greater-than-or-equal`

The strategy `less-than-or-equal` will first seek for the biggest key
that is less than `KEY`, if there is one, it returns the symbol `less`.
Otherwise, if there is a key that is equal to `KEY` it will return the
symbol `equal`. If there is no valid position for the given `KEY`, it
fallback to the symbol `not-found`.

The strategy `equal` will seek a key that is equal to `KEY`. If there
is one it will return the symbol `equal`. Otherwise, it returns the
symbol `not-found`.

The strategy `greater-than-equal` will first seek the smallest key that
is greater than `KEY`, if there is one, it returns the symbol
`greater`.  Otherwise, if there is a key that is equal to `KEY` it
will return the symbol `equal`. If there is no valid position for the
given `KEY`, it fallback the symbol `not-found`.

### `(okvslite-cursor-next? cursor) okvslite-cursor? → boolean?` generic

Move the `CURSOR` to the next key if any. Return `#t` if there is such
a key. Otherwise returns `#f`. `#f` means the cursor reached the end
of the key space.

### `(okvslite-cursor-previous? cursor) okvslite-cursor? → boolean?` generic

Move the `CURSOR` to the previous key if any. Return `#t` if there is
such a key. Otherwise returns `#f`. `#f` means the cursor reached the
begining of the key space.

### `(okvslite-query db key [other [offset [limit]]]) okvslite? bytevector? bytevector? integer? integer? → procedure?` generic

Merge of `okvs-ref`, `okvs-range`, and `okvs-range-reverse` from SRFI-167.
