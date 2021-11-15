# tag

Tag is a wrapper CLI for [`ag`][ag], [`rg`][rg], `find`, and [`fd`][fd].
It parses the output and creates shell aliases to open vim at the
locations of the searches.

## Examples

```bash
% rg foo
test/RgTests.hs
[1] 11:65:  (Command "rg" ["--heading", "--color", "always", "--column", "foo"])
[2] 12:16:  (rgCommand ["foo"])

test/AgTests.hs
[3] 11:53:  (Command "ag" ["--group", "--color", "--column", "foo"])
[4] 12:16:  (agCommand ["foo"])

% e1 # This opens test/RgTests.hs in vim at line 11 column 65
```

```bash
% fd yaml
[1] stack.yaml
[2] stack.yaml.lock

% e1 # This opens stack.yaml in vim
```

## Installation

On macOS:

```sh
$ brew install keith/formulae/tag
```

Manually:

```sh
cmake -B build
cmake --build build
cmake --install build
```

## Setup

Tag is meant to be a transparent wrapper around the underlying tools it
calls, in order to make this work, you need to add a bit of
configuration to your shell to auto-source the aliases after running a
search. Here's some example configurations for common shells.

- `bash - ~/.bashrc`

```bash
if hash tag 2>/dev/null; then
  tag() {
    trap 'source /tmp/tag_aliases 2>/dev/null' SIGINT
    command tag "$@" && source /tmp/tag_aliases 2>/dev/null
    trap - SIGINT
  }
  alias ag="tag ag"
  alias fd="tag fd"
  alias find="tag find"
  alias rg="tag rg"
fi
```

- `zsh - ~/.zshrc`

```zsh
if (( $+commands[tag] )); then
  tag() {
    trap 'source /tmp/tag_aliases 2>/dev/null' SIGINT
    command tag "$@" && source /tmp/tag_aliases 2>/dev/null
    trap - SIGINT
  }
  alias ag="tag ag"
  alias fd="tag fd"
  alias find="tag find"
  alias rg="tag rg"
fi
```

NOTE: With zsh tag also adds `fN` global aliases so you can use `cat f1`
to print the file containing the first match.

- `fish - ~/.config/fish/functions/tag.fish`

```fish
function tag
  command tag $argv; and source /tmp/tag_aliases ^/dev/null
  alias ag "tag ag"
  alias fd "tag fd"
  alias find "tag find"
  alias rg "tag rg"
end
```

## Configuration

You can customize the path tag writes the alias file to by passing
`--alias-file /custom/path` before the tool.

## Credits

This is inspired by [this project](https://github.com/aykamko/tag), I
wanted to expand on it.

[ag]: https://github.com/ggreer/the_silver_searcher
[fd]: https://github.com/sharkdp/fd
[rg]: https://github.com/BurntSushi/ripgrep
