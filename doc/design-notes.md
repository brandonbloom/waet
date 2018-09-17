# Design Notes

## Sexpr Format Differences

Since both WAT and EDN are s-expr text formats, a lot of WAT is syntactically
valid WIE. However, there are some differences, which are documented here.

### Block Comments

WAT supports blockcomments `(; like this ;)` which discard any content
and may be nested. EDN only supports line comments that discard any content,
but does have the discard reader macro `#_` which discards the next lexically
valid form and can be nested `#_(like #_this)`.

### Strings & Byte Arrays

EDN uses Java-style escape sequences, which include most of WAT's basic
escape sequences, but differs for an important kind of escape sequence:
hexnums. WAT matches `/\[0-9a-f]/i` for each of two hexdigits in a byte, but
EDN expects two-byte unicode character sequences matching `/\u[0-9a-f]{4}/i`.

All WIE strings are treated as utf-8.

To address the common need for encoding binary data, EDN's vector syntax
and standard integer literals may be used to represent a string of bytes.
For example: `[ 0x00 0x01 ]`.
