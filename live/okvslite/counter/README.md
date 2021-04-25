# `(import (okvslite counter))`

![TODO](TODO)

## Status

**Early draft.**

## Issues

## Introduction

Simple extension that allows to represent a counter with two
procedures `counter-add!` and `counter-ref`.

## Reference

### `(counter-add! db key integer) okvslite? bytevector? integer? → integer?`

In `DB`, add `INTEGER` to value associated to `KEY`. If `KEY`
associcated with no value, set it to `VALUE`. Return the new value
associated with `KEY`.

### `(counter-ref db key) okvslite? bytevector? → integer?`

In `DB`, retrieve the value associated with `KEY` as an integer.
