# WAET: The WebAssembly Edn Toolkit

WAET (prounced "weight") is the Clojure-equivilent of [WABT, the WebAssembly
Binary Toolkit](https://github.com/WebAssembly/wabt).

The WebAssembly Edn Toolkit provides a collection of Babashka scripts in the
style of WABT's C++ programs for manipulating various WebAssembly file formats.

**Provided Tools:**

- `wat2wie` -- Converts WebAssembly Text into WebAssembly In Edn.
- `wie2wasm` -- Converts WebAssembly In Edn to binary WebAssembly. Accepts the
  same options as `wat2wasm`.
- `wat2wie2wasm` -- Behaves as `wat2wasm`, but detours through WIE for testing
  purposes.

Additionally -- and perhaps most importantly -- WAET provides the above
functionality as a Clojure library for the purpose of being a pleasant
compilation toolkit for compilers targeting WebAssembly.

## WIE: WebAssembly In Edn

WAT (WebAssembly Text) is already an sexpression language and so has a great
deal of common syntax with EDN (Clojure's Extensible Data Notation).

Where possible, WAT syntax is used directly. Where not possible, WAT is translated to WAET
as described here.

### Strings and Data

Clojure on the JVM uses UTF-16 strings, but WAT data strings specify UTF-8 data.
This means that binary data can not safely be encoded into strings. Instead,
strings are treated abstractly and encoded into UTF-8 when written into Wasm binary.
To exactly encode binary data, use `#waet/data[ ... ]` tagged literals. Each element of the
vector is a data "chunk" which may be a string or number. Big integers are encoded big-endian,
other integers are treated as individual bytes.

### Attributes

WAT attributes are encoded with the `#waet/attribute(NAME & DATA...)` tagged literal.

### Other Differences

- TODO: Document more differences.

## Status

Very much a work-in-progress. I'll make an announcement if/when things stablize.

Right now, most of the interesting bits of parsing, id resolution, and encoding
work. Not all section types or instructions are fully-implemented.

Practically nothing is tested, but will eventually use the full wabt test suite.

It's not yet clear what subset of WAT code is reasonably WIE code.
Investigation required.

## Usage

See `waet.core`. There will be two modes: Compile an entire module, or
compile modulefield-by-modulefield.

## Testing

Use `./test.sh`, which delegates to `$WABT_HOME/run-tests.sh` and injects Waet's toolchain.

## License

Copyright © 2024 Brandon Bloom

Distributed under the Eclipse Public License 1.0.
