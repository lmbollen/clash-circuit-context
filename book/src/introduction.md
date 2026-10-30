# Introduction

clash-circuit-context records the signals of a
[Clash](https://clash-lang.org/) design while you simulate it, and writes
them as a waveform. The waveform has one scope for each component of the
design, so it has the same hierarchy as your source.

You do not call a trace function for each signal. You add two annotations to
a function:

- a `HasCircuitContext` constraint in its signature
- an `{-# OPAQUE #-}` pragma

A GHC plugin then makes the function a component, and records each named
local binding under its own name. A simulation that you run as usual gives a
VCD file and a JSON file. The JSON file tells a waveform viewer the type of
each signal: its constructors and fields, not only its bits.

{{#drawio path="diagrams/pipeline.drawio" steps="layers" alt="The plugin rewrites the design. A simulation records it. The dump writes a VCD file and a JSON file for a viewer."}}

The instrumentation has no effect on hardware. When Clash generates HDL, each
recording function is the identity, and the HDL is the same as the HDL of the
design without instrumentation.

## How this book is organised

The book follows [Diátaxis](https://diataxis.fr/). Each page has one purpose:

| Part | Read it when you want to |
|---|---|
| [Tutorials](tutorials/first-waveform.md) | learn the package, step by step |
| How-to guides | do a specific task, for example [keep waveforms from a test suite](how-to/test-suite.md) |
| Reference | look up a fact, for example the [warning categories](reference/warnings.md) |
| Explanation | understand why the package works as it does, for example [the oracle](explanation/oracle.md) |

If you are new to the package, start with
[Your first waveform](tutorials/first-waveform.md).

## The examples are checked

Each Clash example in this book is compiled with the plugin when the book is
built, and each `>>>` line is run. An example that does not compile, or that
gives other output, stops the build. See
[How to build this book](contributing/build-the-book.md).
