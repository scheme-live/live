# `(import (okvslite bag))`

![TODO](TODO)

## Status

**Early draft.**

## Issues

- How to handle concurrency?

## Introduction

## Reference

### `(make-bag prefix) → bag?`

### `(bag? obj) any? → boolean?`

### `(bag-add! bag db key value) bag? okvslite? bytevector integer? → integer?`

### `(bag-delete! bag db key)`

### `(bag-delete-all! bag db keys)`

### `(bag-size bag db) → integer?`

### `(bag-total bag db) → integer?`

### `(bag-key-generator bag db) → procedure?`

### `(bag-key-count bag db key) → integer?`

### `(bag-generator bag db) → procedure?`

### `(bag->alist bag db) → list?`
