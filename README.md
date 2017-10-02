# tag

Tag is a wrapper CLI for [`ag`][ag]. It parses the output and creates
shell aliases to open vim at the locations of the searches.

## Usage

Tag is meant to be a transparent wrapper around `ag`, in order to make
this work, you need to add a bit of configuration to your shell to
auto-source the aliases after running a search. Here's some example
configurations for common shells:

- `bash - ~/.bashrc`

```bash
if hash ag 2>/dev/null; then
  tag() { command tag "$@" && source /tmp/tag_aliases 2>/dev/null; }
  alias ag="tag ag"
fi
```

- `zsh - ~/.zshrc`

```zsh
if (( $+commands[tag] )); then
  tag() { command tag "$@" && source /tmp/tag_aliases 2>/dev/null }
  alias ag="tag ag"
fi
```

- `fish - ~/.config/fish/functions/tag.fish`

```fish
function tag
  command tag $argv; and source /tmp/tag_aliases ^/dev/null
  alias ag "tag ag"
end
```

- manual

In order for tag to work, you just need to call it passing the
underlying tool to call, currently only `ag` is supported, and then pass
any other arguments you'd like. Then tag will create a file at
`/tmp/tag_aliases` that you need to source from your shell.

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

## Credits

This is inspired by [this project](https://github.com/aykamko/tag), I
plan to expand it to work with more tools, such as find.

[ag]: https://github.com/ggreer/the_silver_searcher/
