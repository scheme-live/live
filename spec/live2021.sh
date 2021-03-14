#!/bin/sh
set -eu
cd "$(dirname "$0")"
makeinfo --html --no-split live2021.texi
