# tag

Tag is a wrapper CLI for [`ag`][ag] and [`rg`][rg]. It parses the output
and creates shell aliases to open vim at the locations of the searches.

## Example

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

## Installation

On macOS:

```sh
$ brew install keith/formulae/tag
```

Building manually:

Using [stack](https://docs.haskellstack.org/en/stable/README/):

```sh
$ stack install --local-bin-path /usr/local/bin
```

## Setup

Tag is meant to be a transparent wrapper around `ag` or `rg`, in order
to make this work, you need to add a bit of configuration to your shell
to auto-source the aliases after running a search. Here's some example
configurations for common shells, replace `ag` with `rg` in these
examples if you'd prefer.

- `bash - ~/.bashrc`

```bash
if hash ag 2>/dev/null; then
  tag() { command tag "$@" && source "/tmp/tag_aliases" 2>/dev/null; }
  alias ag="tag ag"
fi
```

- `zsh - ~/.zshrc`

```zsh
if (( $+commands[tag] )); then
  tag() { command tag "$@" && source "/tmp/tag_aliases" 2>/dev/null }
  alias ag="tag ag"
fi
```

NOTE: With zsh tag also adds `fN` global aliases so you can use `cat f1`
to print the file containing the first match.

- `fish - ~/.config/fish/functions/tag.fish`

```fish
function tag
  command tag $argv; and source /tmp/tag_aliases ^/dev/null
  alias ag "tag ag"
end
```

- manual

In order for tag to work, you just need to call it passing the
underlying tool to call and then pass any other arguments you'd like.
Then tag will create a file at `/tmp/tag_aliases` that you need to
source from your shell.

## Configuration

You can customize the path tag writes the alias file to by passing
`--alias-file /custom/path` before the tool.

## Credits

This is inspired by [this project](https://github.com/aykamko/tag), I
plan to expand it to work with more tools, such as find.

[ag]: https://github.com/ggreer/the_silver_searcher/
[rg]: https://github.com/BurntSushi/ripgrep
