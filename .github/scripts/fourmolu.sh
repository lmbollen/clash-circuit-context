#!/usr/bin/env bash
# Format the Haskell files it is given, in place.
#
# This is the entry point the pre-commit hook set calls (hook id `format-hs`).
# It is the per-file counterpart of ./format.sh, which formats the whole tree;
# both run fourmolu twice, because fourmolu is not single-pass idempotent on
# some constructs and a single pass can leave a file that the next run would
# change again.
#
# Unlike the script of the same name in bittide-hardware, this one does not
# shuffle SPDX headers to the top: clash-circuit-context declares copyright and
# licensing in REUSE.toml rather than in file headers, so there is none to move.
set -euf -o pipefail

if ! command -v fourmolu >/dev/null; then
  echo "fourmolu.sh: fourmolu is not on PATH." >&2
  echo "  It comes from the devshell this repo builds in; enter that shell" >&2
  echo "  (see check.sh) or install fourmolu 0.19 before committing." >&2
  exit 1
fi

for file in "$@"; do
  fourmolu --quiet --mode inplace "$file"
  fourmolu --quiet --mode inplace "$file"
done
