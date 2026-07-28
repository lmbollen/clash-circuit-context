<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Automatic generation of memory maps for Clash `Circuit`s

Hello, welcome to `clash-protocols-memmap`!

### What problem does this package solve?

When you build circuits (using [`clash-protocols`][protocols]) and include a CPU to run software programs, you might have hardware components that expose memory-mapped registers.

You could keep a list of addresses of different hardware components and offsets of registers and try to keep it updated with every change. And then build wrappers in your programming language of choice in which you want to write the firmware/software, and also keep those in sync.

**Ooooor** you could use  `clash-protocols-memmap`!

```haskell
someCircuit' ::
  (HasCallStack, HiddenClockResetEnable dom) =>
  Circuit
    (ToConstBwd Mm, Wishbone dom 'Standard 32 (BitVector 32))
    ()
someCircuit' = withName "top" $ circuit $ \(mm, master) -> do
  [a, b] <- interconnectImplicit -< (mm, master)
  withName "A" magicUart -< a
  withName "B" magicUart -< b
```

With [`clash-protocols`][protocols] you can build your circuits using a predefined shape. `clash-protocols-memmap` adds memory map information on a simulation-only backwards channel to each circuit! These memory maps for all those circuits can compose.

Your Clash source code then becomes the source of truth, no need to manually update other documents!

### What components does this repository contain?

 - [`clash-protocols-memmap`](./clash-protocols-memmap)
   
   This is the package that contains the structures for representing and building memory maps. Includes a validator and JSON output.
   Best used with a byte-addressible protocol, like Wishbone.
 
- [`clash-bitpackc`](./clash-bitpackc)

   This package contains a Haskell typeclass `BitPackC` which allows Haskell/Clash types to have a C compatible representation. Useful for making sure that software accessing data knows exactly how to de-/encode datatypes.

- [`rust/memorymap-compiler`](./rust/memorymap-compiler)

   Base structures for reading and manipulationg memory maps in Rust. Uses the JSON output from `clash-protocols-memmap` as the input and contains common transformations, such as monomorphisation or deduplication of multiple memory maps.

- [`rust/memorymap-compiler-c`](./rust/memorymap-compiler-c)

   Functions for generating C code based on a memory map. Uses `memorymap-compiler` as the base and only adds C output functions.

- [`rust/memorymap-compiler-rust`](./rust/memorymap-compiler-rust)

   Functions for generating Rust code based on a memory map. Also based on `memorymap-compiler`.

## License

Most of the code is released under [Apache-2.0](./LICENSES/Apache-2.0) and non-copyrightable/trivial material is licensed under [CC0-1.0](./LICENSES/CC0-1.0.txt).

This project uses the [REUSE tool][reuse] to perform machine checking of licenses in the code in this repository.

[protocols]: https://github.com/clash-lang/clash-protocols
[reuse]: https://reuse.software/
