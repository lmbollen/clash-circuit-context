#!/usr/bin/env bash
# Format all Haskell sources with fourmolu (config: fourmolu.yaml).
# Run twice: fourmolu is not single-pass idempotent on some constructs.
set -euo pipefail
cd "$(dirname "$0")"
fourmolu -i src tests
fourmolu -i src tests
