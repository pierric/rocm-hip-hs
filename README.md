[![Haskell CI](https://github.com/pierric/rocm-hip-hs/actions/workflows/haskell.yml/badge.svg?branch=main)](https://github.com/pierric/rocm-hip-hs/actions/workflows/haskell.yml)

# Haskell bindings to ROCm HIP Runtime
This repository provides a Haskell library binding the key interfaces to HIP runtimes.

## setup
ROCm is pre-installed in /opt/rocm. If not, edit the cabal file to specify the correct path.

Run `ghcup run --cabal 3.14 --ghc 9.12 cabal -- build` to build the library.

## Example
[test/saxy.hip](test/saxy.hip) is an example of HIP kernel.

Compile it with the command `hipcc -O3 --genco --offload-arch=gfx1100 test/saxpy.hip -o test/saxpy.hsaco`. You may need to adjust the arch to the right one for your GPU at hand.

[app/Main.hs](app/Main.hs) is an example Haskell program that call the kernel.
