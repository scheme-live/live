# `(import (okvslite ustore))`

![TODO](TODO)

## Status

**Early draft.**

## Introduction

## Reference

### `(make-ustore prefix)`

### `(ustore-allocate! ustore db key) → bytevector?`

This may use a lock and a counter in cases where there is multiple
writer and one processus.

### `(ustore-ref ustore bytevector) → bytevector?`
