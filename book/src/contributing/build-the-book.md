# How to build this book

This guide shows how to build the book from the directory `book/`, with all
diagrams and all checked examples.

## Install the tools

You need these programs on your `PATH`:

| Program | Source | Does |
|---|---|---|
| `mdbook` | [mdBook](https://github.com/rust-lang/mdBook) | builds the book |
| `mdbook-drawio` | [QBayLogic/mdbook-drawio](https://github.com/QBayLogic/mdbook-drawio) | exports the diagrams in `diagrams/` to SVG |
| `drawio` | [draw.io desktop](https://www.drawio.com/) | the exporter that `mdbook-drawio` runs |
| `mdbook-clash`, `mdbook-clash-doctest` | [clash-lang/mdbook-clash](https://github.com/clash-lang/mdbook-clash) | checks the Clash examples |
| `cabal`, `ghc-9.10.3` | [GHCup](https://www.haskell.org/ghcup/) | builds the `clash` that checks the examples |

With Nix, you can build each of the first four:

```bash
nix build nixpkgs#mdbook -o result-mdbook
nix build nixpkgs#drawio -o result-drawio
nix build github:QBayLogic/mdbook-drawio -o result-mdbook-drawio
nix build github:clash-lang/mdbook-clash -o result-mdbook-clash
```

The binaries are then in `result-*/bin`.

## Build the Clash of the book

The examples use clash-circuit-context and its plugin, so the `clash` that
checks them must be built with this package. `book/cabal.project` does that:
it builds clash-circuit-context from `..`, and `clash-ghc` from the
clash-prelude commit that the package uses.

```bash
cd book
cabal --project-file="$PWD/cabal.project" build clash-ghc:exe:clash clash-circuit-context
```

The first build takes a long time. `scripts/clash` then runs this `clash`
from each directory, with the flags that the examples need.

## Build the book

```bash
cd book
mdbook build
```

The book is in `_build/book/` at the root of the repository. An example
that does not compile, or that gives other output, stops the build with the
name of the page and the line.

## Write a checked example

A fenced block with the language `haskell,clash` is checked:

- The blocks with the same `group=<name>` form one module. Start the group
  with a `hidden` block that holds the module header and the imports.
- A line with `>>>` runs in a Clash session, and the lines after it are the
  expected output. `...` matches each text.
- `topEntity=<name>` also generates Verilog for that entity.

The module `examples/BookSupport.hs` is on the search path of each example.
It has `printWaveform`, which simulates a design and prints the scopes and
signals of its VCD. Use it to show a waveform in a page.

Examples that write files write them in `book/`, for example
`book/waveforms/`. These files are not part of the repository.
