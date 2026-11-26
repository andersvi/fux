#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

find examples -maxdepth 1 -type f \
    \( -name '*.ogg' \
    -o -name '*.pdf' \
    -o -name '*.eps' \
    -o -name '*.mscx' \
    -o -name '*.mscz' \) \
    -print0 |
    xargs -0 -r gh release upload examples --clobber
