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
waveform file (FST or VCD) and a JSON file. The JSON file tells a waveform
viewer the type of each signal: its constructors and fields, not only its
bits.

{{#drawio path="diagrams/pipeline.drawio" steps="layers" alt="The plugin rewrites the design. A simulation records it. The dump writes a waveform file and a JSON file for a viewer."}}

## What the plugin does to your code

You write a function with the two annotations:

```haskell
counter :: (HasCircuitContext) => Signal System Bool -> Signal System (Unsigned 8)
counter enable = count
 where
  count = register clockGen resetGen enableGen 0 next
  next = mux enable (count + 1) count
{-# OPAQUE counter #-}
```

Before type checking, the plugin rewrites it to approximately this:

```haskell
counter enable =
  component "counter"
    ( let count = autoTrace "count" (register clockGen resetGen enableGen 0 next)
          next = autoTrace "next" (mux enable (count + 1) count)
       in count
    )
```

- `component "counter"` opens a scope for each instance of `counter`.
- `autoTrace "count"` records the signal `count` in that scope. If a binding
  cannot be recorded (a function, a list, a type without `BitPack`),
  `autoTrace` does nothing, and the plugin can give a warning.
- `HasCircuitContext` carries the current scope from the caller to the
  callee. A function that simulates the design, such as `writeWaveform`,
  supplies the top of it.

The rest of the book uses these words: a *component* is a function with a
scope of its own, a *trace* records a signal, and a *probe* records a value
inside the step function of a mealy machine. For all the rules, see [What
the plugin instruments](reference/instrumentation.md).

The instrumentation adds no hardware. When Clash generates HDL, each
recording function is the identity. The HDL does the same function as the
HDL of the design without instrumentation, but its text is different. See
[How to generate HDL from an instrumented design](how-to/hdl.md).

## How this book is organised

The book follows [Diátaxis](https://diataxis.fr/). Each page has one purpose:

| Part | Read it when you want to |
|---|---|
| [Tutorials](tutorials/first-waveform.md) | learn the package, step by step |
| How-to guides | do a specific task, for example [keep waveforms from a test suite](how-to/test-suite.md) |
| Reference | look up a fact, for example the [warning categories](reference/warnings.md) |
| Explanation | understand why the package works as it does, for example [the oracle](explanation/oracle.md) |

If you are new to the package, start with
[Your first waveform](tutorials/first-waveform.md). It also sets up the
project. To add the package to a project that you already have, see [How to
add clash-circuit-context to a project](how-to/add-to-project.md).

## The examples are checked

Each Clash example in this book is compiled with the plugin when the book is
built, and each `>>>` line is run. An example that does not compile, or that
gives other output, stops the build. See
[How to build this book](contributing/build-the-book.md).

Many examples show a waveform with `printWaveform n design`. **This function
is a helper of the book, not part of the package.** It simulates `n` cycles
of the design, writes the waveform to a temporary file, and prints its
scopes and signals as an indented tree. To see the same in your project,
write the waveform with `writeWaveform`, and open the waveform file in a viewer
(or print its declarations, as the tutorial does).
