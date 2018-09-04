# The WebAssembly Binary Toolkit in Clojure

The WebAssembly Binary Toolkit, or WABT, is a collection of C++ programs for
working with WebAssembly. This partial port provides mainly a "wat2wasm" equivilent
where WebAssembly text format (WAT) programs are encoded in Edn. Since WAT is already
an s-expr language, WAT programs are mostly valid Edn. It should be possible to
port a WAT program to Wat-In-Edn (WIE) with minimal local text fixups.

Primarily, this project aims to provide a pleasant interface for compilers written
in Clojure to target WebAssembly as a backend.

## Status

Very much a work-in-progress. I'll make an announcement if/when things stablize.

Right now, most of the interesting bits of parsing, id resolution, and encoding
work. Not all section types or instructions are fully-implemented.

Practically nothing is tested.

It's not yet clear what subset of WAT code is reasonably WIE code.
Investigation required.

## Usage

See `wabt-clj.core`. There will be two modes: Compile an entire module, or
compile modulefield-by-modulefield.

## License

Copyright © 2018 Brandon Bloom

Distributed under the Eclipse Public License 1.0.
