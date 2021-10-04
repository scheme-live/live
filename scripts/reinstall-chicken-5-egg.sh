#!/bin/sh
set -eu
cd "$(dirname "$0")/.."
set -x
chicken-uninstall -force live
rm -f *.build.sh *.import.scm *.install.sh *.link *.o *.so
chicken-install
