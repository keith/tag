#!/bin/bash

set -euo pipefail

diff -Nur \
  <(find . | sort) \
  <(./build/tag find . | sort)

if ./build/tag </dev/null 2>/dev/null; then
  echo "error: tag without arguments should fail without piped input" >&2
  exit 1
fi

tmpfile=$(mktemp)
expected=$(mktemp)
cat <<EOF > "$expected"
alias eall="eval '\$EDITOR -q \\"/tmp/tag_qf\\"'"
alias e1="eval '\$EDITOR \\"./README.md\\"'"
alias -g f1="./README.md"
alias -g d1="."
EOF

SKIP_PIPE_FILTERING=true SHELL=zsh ./build/tag --alias-file "$tmpfile" find . -name "*.md"
diff -Nur "$expected" "$tmpfile"
SKIP_PIPE_FILTERING=true SHELL=zsh ./build/tag find . -name "*.md"
diff -Nur "$expected" /tmp/tag_aliases

cat <<'EOF' > "$expected"
alias eall="eval '$EDITOR -q \"/tmp/tag_qf\"'"
alias e1="eval '$EDITOR \"README.md\"'"
alias -g f1="README.md"
alias -g d1="."
alias e2="eval '$EDITOR \"tag.cpp\"'"
alias -g f2="tag.cpp"
alias -g d2="."
EOF

printf 'README.md\ntag.cpp\n' | SHELL=zsh ./build/tag --alias-file "$tmpfile" > /dev/null
diff -Nur "$expected" "$tmpfile"

SKIP_PIPE_FILTERING=true SHELL=zsh ./build/tag --alias-file "$tmpfile" ls LICENSE README.md
cat <<EOF > "$expected"
alias eall="eval '\$EDITOR -q \"/tmp/tag_qf\"'"
alias e1="eval '\$EDITOR \"LICENSE\"'"
alias -g f1="LICENSE"
alias -g d1="."
alias e2="eval '\$EDITOR \"README.md\"'"
alias -g f2="README.md"
alias -g d2="."
EOF

diff -Nur "$expected" "$tmpfile"
SKIP_PIPE_FILTERING=true SHELL=zsh ./build/tag --alias-file "$tmpfile" ls -l LICENSE README.md
diff -Nur "$expected" "$tmpfile"

SKIP_PIPE_FILTERING=true SHELL=zsh ./build/tag --alias-file "$tmpfile" find .github -name build.yml
cat <<EOF > "$expected"
alias eall="eval '\$EDITOR -q \"/tmp/tag_qf\"'"
alias e1="eval '\$EDITOR \".github/workflows/build.yml\"'"
alias -g f1=".github/workflows/build.yml"
alias -g d1=".github/workflows"
EOF
diff -Nur "$expected" "$tmpfile"

cat <<EOF > "$expected"
alias eall="eval '\$EDITOR -q \\"/tmp/tag_qf\\"'"
alias e1="eval '\$EDITOR \"README.md\" \"+call cursor(10, 6)\"'"
alias -g f1="README.md"
alias -g d1="."
alias e2="eval '\$EDITOR \"README.md\" \"+call cursor(12, 75)\"'"
alias -g f2="README.md"
alias -g d2="."
alias e3="eval '\$EDITOR \"README.md\" \"+call cursor(13, 26)\"'"
alias -g f3="README.md"
alias -g d3="."
alias e4="eval '\$EDITOR \"README.md\" \"+call cursor(16, 63)\"'"
alias -g f4="README.md"
alias -g d4="."
alias e5="eval '\$EDITOR \"README.md\" \"+call cursor(17, 26)\"'"
alias -g f5="README.md"
alias -g d5="."
alias e6="eval '\$EDITOR \"integration-test.sh\" \"+call cursor(88, 53)\"'"
alias -g f6="integration-test.sh"
alias -g d6="."
EOF

if command -v rg; then
  SKIP_PIPE_FILTERING=true SHELL=zsh ./build/tag rg foo --sort path
  diff -Nur "$expected" /tmp/tag_aliases

  output=$(./build/tag rg tag README.md)
  if [[ -z "$output" ]]; then
    echo "error: missing output" >&2
    exit 1
  fi

  SKIP_PIPE_FILTERING=true SHELL=zsh ./build/tag --alias-file "$tmpfile" rg tag README.md > /dev/null
  if ! grep -Fq 'README.md\" \"+call cursor(1, 3)' "$tmpfile"; then
    echo "error: single-file rg output did not create a README.md alias" >&2
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

  count=$(./build/tag rg '"rg"' | wc -l | xargs)
  if [[ "$count" != 3 ]]; then
    echo "error: unexpected result count: $count" >&2
    exit 1
  fi

  count=$(./build/tag rg "\"rg\"" | wc -l | xargs)
  if [[ "$count" != 3 ]]; then
    echo "error: unexpected backslashed result count: $count" >&2
    exit 1
  fi

  count=$(./build/tag rg \"rg\" | wc -l | xargs)
  if [[ "$count" != 3 ]]; then
    echo "error: unexpected non-quoted backslashed result count: $count" >&2
    exit 1
  fi
else
  echo "warning: rg isn't installed" >&2
fi

repo_root=$(pwd)

if output=$(./build/tag find "baz qux" 2>&1); then
  echo "error: find should have failed" >&2
  exit 1
fi

if [[ "$output" != *"baz qux"* ]]; then
  echo "error: unexpected quoted find output: $output" >&2
  exit 1
fi

SKIP_PIPE_FILTERING=true ./build/tag find . -name README.md > /dev/null
if [[ "$(cat /tmp/tag_qf)" != *"README.md:1:1:"* ]]; then
  exit 1
fi

if command -v zsh >/dev/null; then
  space_tmp=$(mktemp -d)
  (
    cd "$space_tmp"
    touch "qg s"
    SKIP_PIPE_FILTERING=true SHELL=zsh "$repo_root/build/tag" --alias-file "$tmpfile" find . -name "qg s" > /dev/null
  )

  cat <<'EOF' > "$expected"
alias eall="eval '$EDITOR -q \"/tmp/tag_qf\"'"
alias e1="eval '$EDITOR \"./qg s\"'"
alias -g f1="./qg\\ s"
alias -g d1="."
EOF

  diff -Nur "$expected" "$tmpfile"

  alias_output=$(zsh -fc 'source "$1"; print_args(){ for arg in "$@"; do print -r -- "[$arg]"; done; }; eval "print_args f1"' zsh "$tmpfile")
  if [[ "$alias_output" != "[./qg s]" ]]; then
    echo "error: zsh global alias did not quote path with spaces: $alias_output" >&2
    exit 1
  fi
fi

if command -v zsh >/dev/null; then
  git_space_tmp=$(mktemp -d)
  (
    cd "$git_space_tmp"
    git init -q
    touch "qg s"
    SKIP_PIPE_FILTERING=true SHELL=zsh "$repo_root/build/tag" --alias-file "$tmpfile" git-status > /dev/null
  )

  cat <<'EOF' > "$expected"
alias eall="eval '$EDITOR -q \"/tmp/tag_qf\"'"
alias e1="eval '$EDITOR \"qg s\"'"
alias -g f1="qg\\ s"
alias -g d1="."
EOF

  diff -Nur "$expected" "$tmpfile"

  if ! editor_output=$(EDITOR=print_args zsh -fc 'print_args(){ for arg in "$@"; do print -r -- "[$arg]"; done; }; source "$1"; eval "e1"' zsh "$tmpfile"); then
    echo "error: zsh edit alias for git-status path with spaces failed" >&2
    exit 1
  fi
  if [[ "$editor_output" != "[qg s]" ]]; then
    echo "error: zsh edit alias did not quote git-status path with spaces: $editor_output" >&2
    exit 1
  fi

  alias_output=$(zsh -fc 'source "$1"; print_args(){ for arg in "$@"; do print -r -- "[$arg]"; done; }; eval "print_args f1"' zsh "$tmpfile")
  if [[ "$alias_output" != "[qg s]" ]]; then
    echo "error: zsh global alias did not quote git-status path with spaces: $alias_output" >&2
    exit 1
  fi
fi

git_tmp=$(mktemp -d)
(
  cd "$git_tmp"
  git init -q
  git config user.email tag@example.com
  git config user.name tag
  git config commit.gpgsign false
  old_path=$(printf 'f%s' oo)
  printf content > "$old_path"
  git add "$old_path"
  git commit -qm init 2>/dev/null
  git mv "$old_path" bar

  SKIP_PIPE_FILTERING=true SHELL=zsh "$repo_root/build/tag" --alias-file "$tmpfile" git-status --renames > /dev/null 2>/dev/null
)

cat <<'EOF' > "$expected"
alias eall="eval '$EDITOR -q \"/tmp/tag_qf\"'"
alias e1="eval '$EDITOR \"bar\"'"
alias -g f1="bar"
alias -g d1="."
EOF

diff -Nur "$expected" "$tmpfile"
