# `(import (okvslite scstore))`

![TODO](TODO)

## Status

**Early draft.**

## Issues

## Introduction

The semantic store is a search engine that extends boolean-keyword
search engine with semantic features leaning toward concept search.
It is primarly meant for text search of semi-structured documents.

In particular, it is not meant to be a general purpose search
framework or a distributed index.

Semantic store is fine-tuned for finding text documents that match the
verbatim query of the user or their intent. It can help explore and
navigate a more or less large quantity of textual data. Documents may
be linked, annotated with discovery date, and keywords. The latter may
be used to create facets.

Here is the list of supported features:

- boolean-keyword text search with different level of
  fuzziness/relatedness
- sorting by discovery date
- typographic fix suggestion
- query suggestion to implement autocomplete, and related queries
- result extracts highlighting
- similar documents query
- network exploration
- entity recognition and linking
- keyword facets
- full scan

## Reference

### `(make-scstore prefix)`

### `(scstore-entity-link! scstore db entity other)`

### `(scstore-entity-set! scstore db entity description)`

### `(scstore-entity-ref scstore db entity description)`

### `(scstore-outgoing scstore db url)`

### `(scstore-incoming scstore db url)`

### `(scstore-index! scstore db uri document)`

### `(scstore-query scstore db query)`
