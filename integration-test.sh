#!/bin/bash

set -euo pipefail

diff -Nur <(rg --sort path foo) <(stack run -- rg --sort path foo | cat)
