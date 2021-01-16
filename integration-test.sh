#!/bin/bash

set -euo pipefail

diff -Nur \
  <(find . -name "*.hs" | sort) \
  <(stack run -- find . -name "*.hs" | sort)
