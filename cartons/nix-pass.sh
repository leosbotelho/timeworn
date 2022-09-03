#!/usr/bin/env bash

set -euo pipefail

f=$(mktemp)
trap "srm -f $f" EXIT
pass show "$1" | head -c -1 > $f
nix-instantiate --eval -E "builtins.readFile $f"
