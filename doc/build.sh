#!/bin/sh
set -eu
cd "$(dirname "$0")"
generate() {
    dst="$1"
    src="$2"
    makeinfo \
        --html \
        --css-ref "/style.css" \
        --internal-links "$dst/index.tsv" \
        --output "$dst" \
        "$src.texi"
}
generate head head
generate 2021 live2021
gsi index.scm >index.conf
