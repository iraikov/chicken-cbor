# cbor

[![Chicken Scheme](https://img.shields.io/badge/Chicken-Scheme-orange.svg)](https://call-cc.org/)

Concise Binary Object Representation (CBOR) for CHICKEN Scheme.

CBOR ([RFC 8949](https://www.rfc-editor.org/rfc/rfc8949)) is a compact
binary data format with a JSON-like data model. The `cbor` library
provides routines to read and write CBOR data on binary ports. It maps
Scheme values to CBOR data items and back, and it writes SRFI-4 numeric
vectors as typed arrays ([RFC 8746](https://www.rfc-editor.org/rfc/rfc8746)):
one block of raw bytes, with no per-element conversion. 

The design follows the Haskell [cborg](https://github.com/well-typed/cborg)
library: a low-level token layer, a generic term tree, and a mapping for
native values on top.

## Contents

- [Modules](#modules)
- [Module cbor](#module-cbor)
  - [Reading and writing](#reading-and-writing)
  - [The mapping](#the-mapping)
  - [Codecs](#codecs)
  - [Limits and errors](#limits-and-errors)
- [Module cbor-core](#module-cbor-core)
- [Module cbor-term](#module-cbor-term)
- [Module cbor-token](#module-cbor-token)
- [Module cbor-deflate](#module-cbor-deflate)
- [Examples](#examples)
- [Author](#author)
- [License](#license)
- [Version history](#version-history)

## Modules

| Module         | Purpose |
|----------------|---------|
| `cbor`         | Scheme values to and from CBOR. Most programs need only this module. |
| `cbor-core`    | Low-level encoding and decoding of individual tokens. |
| `cbor-term`    | A generic tree that mirrors a CBOR data item exactly, and diagnostic notation. |
| `cbor-token`   | CBOR as a flat list of tokens, for testing and inspection. |
| `cbor-deflate` | Compressed data items, using the bundled [miniz](https://github.com/richgel999/miniz). |

## Module cbor

### Reading and writing

#### `(write-cbor OBJ [PORT])`

Writes `OBJ` to `PORT` as one CBOR data item. `PORT` must accept binary
output.

#### `(read-cbor [PORT])`

Reads one data item from `PORT`. Returns an eof object if `PORT` is
already at its end.

#### `(cbor->bytevector OBJ)`<br>`(bytevector->cbor BYTEVECTOR)`

Encode to, and decode from, a bytevector. `bytevector->cbor` signals an
error if the bytevector holds anything after the first data item.

#### `(write-cbor-file PATH OBJ)`<br>`(read-cbor-file PATH)`

Write and read a file holding one data item. The file starts with the
self-describe tag 55799 (the bytes `d9 d9 f7`), which marks it as CBOR.

#### `(call-with-cbor-output-file PATH PROC)`<br>`(call-with-cbor-input-file PATH PROC)`

Open `PATH` as a binary port and call `PROC` with the port. The output
version writes the self-describe tag first; `PROC` should then write
exactly one data item.

#### `(write-cbor-sequence LIST [PORT])`<br>`(read-cbor-sequence [PORT])`

Write each element of `LIST` as a separate data item, and read all items
up to the end of input, as a CBOR sequence
([RFC 8742](https://www.rfc-editor.org/rfc/rfc8742)).

#### `(encode-value OBJ PORT)`<br>`(decode-value DECODER)`

The same mapping as `write-cbor` and `read-cbor`, for use together with
the token-level procedures of `cbor-core`.

#### Parameter `cbor-preferred-floats`

When true, flonums are written in the shortest float width that holds
them exactly (half, single or double). The default, `#f`, always writes
doubles.

### The mapping

| Scheme | CBOR |
|--------|------|
| exact integer | major type 0 or 1; beyond 64 bits, bignum tags 2 and 3 |
| flonum | double (or shortest exact width, see above) |
| exact rational | tag 30, `[numerator, denominator]` |
| `#t`, `#f` | true, false |
| the void value | undefined |
| `cbor-null` | null |
| string | text string |
| symbol | tag 39 (identifier) with a text string |
| proper list | array |
| improper list or pair | tag `tag-scheme-pair`, `[items..., tail]` |
| vector | tag `tag-scheme-vector` with an array |
| bytevector (also blob, u8vector) | byte string |
| s8, u16, s16, u32, s32, u64, s64, f32 and f64 vectors | RFC 8746 typed array, in host byte order |
| character | tag `tag-scheme-char` with its code point |
| keyword | tag `tag-scheme-keyword` with a text string |
| SRFI-69 hash table | map (read back with `equal?` hashing) |
| `cbor-tagged` record | the given tag and value |
| `cbor-simple` record | the given simple value |

When reading, maps become SRFI-69 hash tables, typed arrays in either
byte order become SRFI-4 vectors (half-precision arrays become
f32vectors), the self-describe tag is skipped, and tags without a codec
become `cbor-tagged` records. Circular lists and cyclic vectors cannot
be written.

The private tags `tag-scheme-pair`, `tag-scheme-vector`,
`tag-scheme-char` and `tag-scheme-keyword` lie in the
first-come-first-served range of the IANA tag registry.

#### Constant `cbor-null`<br>`(cbor-null? OBJ)`

The CBOR null value.

#### `(make-cbor-tagged TAG VALUE)`<br>`(cbor-tagged? OBJ)`<br>`(cbor-tagged-tag TAGGED)`<br>`(cbor-tagged-value TAGGED)`

A tagged item whose tag has no codec.

#### `(make-cbor-simple N)`<br>`(cbor-simple? OBJ)`<br>`(cbor-simple-value SIMPLE)`

A simple value other than false, true, null and undefined.

### Codecs

A codec adds a Scheme type, tied to a CBOR tag.

#### `(make-cbor-codec TAG PREDICATE ENCODER DECODER)`

`PREDICATE` recognizes values of the type. `ENCODER` is called with
three arguments: the value, a procedure that writes a nested Scheme
value, and the output port (for writing tokens directly with
`cbor-core`). It must write exactly one data item, the content of the
tag; the tag itself is written before it. `DECODER` is called with the
decoded content and returns the value.

#### `(register-cbor-codec! CODEC)`

Adds `CODEC` to the global registry, ahead of earlier codecs.

#### Parameter `cbor-codecs`

A list of codecs that take priority over the global registry, for use
with `parameterize`. Built-in types are always handled first.

```scheme
(define-record-type point (make-point x y) point? (x point-x) (y point-y))

(register-cbor-codec!
 (make-cbor-codec 1000001 point?
                  (lambda (p emit port) (emit (list (point-x p) (point-y p))))
                  (lambda (content) (apply make-point content))))
```

### Limits and errors

#### Parameter `cbor-max-depth`

Deepest nesting of arrays, maps and tags accepted when reading, and
written when encoding (default 1024).

#### Parameter `cbor-max-length`

Largest string (in bytes) or array or map (in items) accepted when
reading, or `#f` for no limit (the default). Very long byte strings are
read in two steps, so a forged length in a small input cannot force a
large allocation.

#### Errors

Decoding errors signal a condition of kinds `exn` and `cbor`. The `cbor`
part has the properties `reason` (one of `eof`, `malformed`, `type`,
`limit`, `utf8`) and `offset` (the byte position in the input).

## Module cbor-core

Encoders take the value first and an optional output port:
`encode-head`, `encode-uint`, `encode-nint`, `encode-int`,
`encode-bytes` (with optional start and end), `encode-bytes-begin`,
`encode-string`, `encode-string-begin`, `encode-list-len`,
`encode-list-begin`, `encode-map-len`, `encode-map-begin`,
`encode-tag`, `encode-bool`, `encode-null`, `encode-undefined`,
`encode-simple`, `encode-float16`, `encode-float32`,
`encode-float64`, `encode-float` (shortest exact width),
`encode-break` and `encode-encoded` (pre-encoded bytes).

Decoders take a decoder, made with `(make-decoder [PORT])`:
`peek-token-type`, `decode-uint`, `decode-nint`, `decode-int`,
`decode-integer` (also bignums), `decode-bytes`, `decode-string`,
`decode-bytes-indef`, `decode-string-indef`, `decode-list-len`,
`decode-list-len-indef`, `decode-list-len-or-indef`,
`decode-map-len`, `decode-map-len-indef`,
`decode-map-len-or-indef`, `decode-tag`, `decode-bool`,
`decode-null`, `decode-undefined`, `decode-simple`,
`decode-float`, `decode-float-token`, `decode-break`,
`decode-break-or` and `decode-skip`.

`peek-token-type` returns one of the symbols `uint`, `nint`, `bytes`,
`bytes-indef`, `string`, `string-indef`, `list-len`, `list-len-indef`,
`map-len`, `map-len-indef`, `tag`, `bool`, `null`, `undefined`,
`simple`, `float16`, `float32`, `float64`, `break`, `invalid` or `eof`.

Byte and text strings are decoded whole, whether definite or indefinite.
Decoders that build nested items call `decoder-enter!` and
`decoder-leave!` around each level so that `cbor-max-depth` applies.

## Module cbor-term

#### `(decode-term DECODER)`<br>`(encode-term TERM [PORT])`

Terms are a `datatype` with the variants `term-int`, `term-bytes`,
`term-bytes-indef`, `term-string`, `term-string-indef`, `term-list`,
`term-list-indef`, `term-map`, `term-map-indef` (pairs as
`(key . value)`), `term-tagged`, `term-bool`, `term-null`,
`term-undefined`, `term-simple`, `term-half`, `term-float` and
`term-double`. Terms keep float widths and indefinite lengths, so
re-encoding a decoded term gives back the original bytes whenever they
were in preferred form.

#### `(term->diagnostic TERM)`

Returns the diagnostic notation of RFC 8949 section 8, for example
`"[1, [2, 3]]"`.

#### `(term=? TERM1 TERM2)`

Structural equality that treats NaN as equal to itself.

## Module cbor-token

A `datatype` of tokens (`tk-uint`, `tk-nint`, `tk-bytes`,
`tk-bytes-begin`, `tk-string`, `tk-string-begin`, `tk-list-len`,
`tk-list-begin`, `tk-map-len`, `tk-map-begin`, `tk-tag`, `tk-bool`,
`tk-null`, `tk-undefined`, `tk-simple`, `tk-float16`, `tk-float32`,
`tk-float64`, `tk-break`), with `write-token`, `write-tokens`,
`read-token`, `read-tokens` and `token->list`.

## Module cbor-deflate

Loading this module registers a compressed-item tag, `tag-compressed`.
Its content is the array `[1, length, data]`: 1 names zlib (RFC 1950),
`length` is the size of the uncompressed encoding, and `data` is a byte
string holding the compressed encoding of one data item. Writers emit
the data as an indefinite-length byte string of 1 MB chunks, so
compression runs in bounded memory. `read-cbor` expands compressed items
transparently.

#### `(cbor-deflated OBJ)`

Wraps `OBJ` so that it is written compressed. The wrapper may appear
anywhere inside a larger value.

#### `(write-cbor/compressed OBJ [PORT])`

Writes `OBJ` as one compressed data item.

#### Parameter `cbor-deflate-level`

Compression level from 0 to 10 (default 6; 1 is fastest).

#### `(deflate-bytevector BYTEVECTOR)`<br>`(inflate-bytevector BYTEVECTOR LENGTH)`

Compress to, and decompress from, a zlib stream.

## Examples

```scheme
(import cbor (srfi 4))

(write-cbor-file "weights.cbor"
                 `((name . "layer1")
                   (shape 256 784)
                   (data . ,(make-f32vector (* 256 784) 0.5))))

(define checkpoint (read-cbor-file "weights.cbor"))
(cdr (assq 'shape checkpoint))            ; => (256 784)
```

Python can read the same file with `cbor2`; the typed array comes back
as a tagged byte string that `numpy.frombuffer` turns into an array.

## Author

Ivan Raikov

## License

MIT. See [LICENSE](LICENSE).

## Version history

- **1.0**: Initial release
