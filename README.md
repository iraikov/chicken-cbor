# cbor

Concise Binary Object Representation (RFC 8949) for CHICKEN 6, with
RFC 8746 typed arrays for SRFI-4 vectors and optional zlib compression.
See `cbor.wiki` for documentation.

Build and test:

    chicken-install -test

Benchmark (writes temporary files to `$TMPDIR`):

    CSC=csc tests/bench-large.sh 100000000
