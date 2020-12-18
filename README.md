# Overview

This is a working sample included in the FFI chapter of Haskell Wikibooks.

You can read the original article at: https://en.wikibooks.org/wiki/Haskell/FFI

# How to build (Ubuntu)

## Example: import C into Haskell

This example contains following files:

- main.hs
- Bessel.hsc
- Qag.hsc
- GSLWorkspace.hs

```
sudo apt install libgsl-dev
hsc2hs Bessel.hsc
hsc2hs Qag.hsc
ghc main.hs -lgsl
```

## Example: export Haskell to C

This example contains following files:

- fibonacci.hs
- fib.c

```
ghc -c fibonacci.hs
ghc -no-hs-main fib.c fibonacci.o -o fib
```
