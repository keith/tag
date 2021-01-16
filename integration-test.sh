#!/bin/bash

set -euo pipefail

diff -Nur \
  <(find . -name "*.s" | sort) \
  <(stack run -- find . -name "*.hs" | sort)
