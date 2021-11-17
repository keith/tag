#!/bin/bash

set -euo pipefail

diff -Nur \
  <(find . | sort) \
  <(./build/tag find . | sort)

tmpfile=$(mktemp)
expected=$(mktemp)
cat <<EOF > "$expected"
alias e1="eval '\$EDITOR \\"./README.md\\"'"
alias -g f1="./README.md"
EOF

SHELL=zsh ./build/tag --alias-file "$tmpfile" find . -name "*.md"
diff -Nur "$expected" "$tmpfile"
SHELL=zsh ./build/tag find . -name "*.md"
diff -Nur "$expected" /tmp/tag_aliases

cat <<EOF > "$expected"
alias e1="eval '\$EDITOR \"README.md\" \"+call cursor(10, 6)\"'"
alias -g f1="README.md"
alias e2="eval '\$EDITOR \"README.md\" \"+call cursor(12, 75)\"'"
alias -g f2="README.md"
alias e3="eval '\$EDITOR \"README.md\" \"+call cursor(13, 26)\"'"
alias -g f3="README.md"
alias e4="eval '\$EDITOR \"README.md\" \"+call cursor(16, 63)\"'"
alias -g f4="README.md"
alias e5="eval '\$EDITOR \"README.md\" \"+call cursor(17, 26)\"'"
alias -g f5="README.md"
alias e6="eval '\$EDITOR \"integration-test.sh\" \"+call cursor(37, 28)\"'"
alias -g f6="integration-test.sh"
EOF

if command -v rg; then
  SHELL=zsh ./build/tag rg foo --sort path
  diff -Nur "$expected" /tmp/tag_aliases

  output=$(./build/tag rg tag README.md)
  if [[ -z "$output" ]]; then
    echo "error: missing output" >&2
    exit 1
  fi

  # Hack to make this not be caught by the grep
  expected_file=README.md
  expected_string="$expected_file:1:3:"
  output=$(./build/tag rg tag)
  if [[ "$output" != *"$expected_string"* ]]; then
    echo "error: piped rg output not correctly formatted: $output" >&2
    exit 1
  fi
else
  echo "warning: rg isn't installed" >&2
fi

if output=$(./build/tag find "baz qux" 2>&1); then
  echo "error: find should have failed" >&2
  exit 1
fi

if [[ "$output" != *"baz qux"* ]]; then
  echo "error: unexpected quoted find output: $output" >&2
  exit 1
fi
