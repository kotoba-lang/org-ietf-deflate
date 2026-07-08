# kotoba-lang/org-ietf-deflate

Zero-dep portable `.cljc` implementation of DEFLATE (IETF RFC 1951) and the
zlib wrapper format (IETF RFC 1950) — Huffman + LZ77 decompression, no
`java.util.zip`, no external library. Named `org-ietf-deflate` (RFC-numbered
IETF spec, same pattern as `org-ietf-turn`/`org-ietf-oauth2`/`org-ietf-cbor`).

Extracted from `kotoba-lang/kasane` (ADR-2606272100), where it was the single
codec backing PSD ZIP channels and PDF FlateDecode streams. Also consumed by
`org-w3-png` (IDAT), `org-pkware-zip` (raw deflate), `org-w3-woff` (per-table
compression), and `org-iso-pdf` (FlateDecode) — DEFLATE is byte-identical
across all of these containers, so this is the one shared leaf dependency in
the kotoba-lang media/graphics standards-substrate split (see
`com-junkawasaki/root` 90-docs/adr/ for the batch ADR).

## Usage

```clojure
(require '[deflate.core :as deflate])

(deflate/inflate zlib-bytes)      ; RFC 1950 zlib-wrapped stream → vector of unsigned bytes
(deflate/inflate-raw raw-bytes)   ; RFC 1951 raw deflate stream (no zlib header, e.g. ZIP/WOFF)
```

## Test

```sh
clojure -M:test
```
