#!/usr/bin/env bash

set -euo pipefail

echo "Running benchmarks for tvix/eval..."
cd tvix/eval
docker run --rm -v "$(pwd):/app" -w /app rust cargo bench
windtunnel-cli report -f criterion-rust .
