# cl-ipp

An Internet Printing Protocol (RFC 8010 encoding, RFC 8011 model) client library for Common Lisp (SBCL). Status: 0.1.0, early; tested against one printer (a Brother MFC-J4335DW).

- Binary codec: `encode-message` / `decode-message`, including collections (RFC 3382) and every RFC 8010 value type.
- Request builders: Get-Printer-Attributes, Get-Jobs, Print-Job.
- Response digestion: `printer-summary` (printer-state, marker-names/levels/colors/types zipped into per-marker plists), `job-summaries`, `simplify-message` (plists).
- Transport: HTTP/1.1 POST with `Content-Type: application/ipp` over `usocket` (only dependency). Handles Content-Length, chunked and close-delimited bodies.

## Quick start

```lisp
(ql:quickload :cl-ipp)
(cl-ipp:printer-info "ipp://BRW000000000000.local:631/ipp/print")
;; => (:STATE :IDLE :STATE-REASONS "none"
;;     :MARKERS ((:NAME "M" :COLOR "#FF00FF" :TYPE "ink-cartridge" :LEVEL 24 ...) ...)
;;     :ATTRIBUTES (...all printer attributes as a plist...))

(cl-ipp:get-jobs uri :which-jobs "completed" :limit 5)   ; response message
(cl-ipp:print-job uri octets :document-format "application/pdf") ; SENDS A JOB
```

Data model: message = struct (`message-code`, `message-request-id`, `message-groups`, `message-document`); group = `(:printer . attributes)`; attribute = `(name . values)`; value = `(tag . lisp-value)`. Booleans are `T`/`NIL`. `simplify-attributes` turns attributes into keyword plists (single values unwrapped, collections become plists).

## Prior art (searched 2026-10-22)

I found no existing Common Lisp IPP library. Checked: GitHub repository search (`ipp language:"Common Lisp"`, `"internet printing protocol" lisp`), Quicklisp `system-apropos` for "ipp" and "cups" (dist 2026-01-01), and general web searches. The only Lisp hit was [emarsden/ipp-el](https://github.com/emarsden/ipp-el), which is Emacs Lisp, not Common Lisp. I did not exhaustively search Ultralisp or CLiki, so absence there is not confirmed. Other-language implementations (jipp, python-ipp, ipp-nio, williamkapke/ipp) and CUPS itself exist and were not evaluated. Not finding something is weak evidence; treat "first CL IPP library" as unverified.

## What is tested (and what is not)

`./run-tests.sh` runs 14 FiveAM tests / 38 checks, all offline, all passing on SBCL 2.6.9.

Tested:
- Encoding checked against bytes written out by hand from the RFC (not just roundtrip).
- Roundtrip of every value type, multi-valued attributes, nested collections, repeated groups, Print-Job document appended after the end tag.
- Decode error paths (short message, missing end tag, value length past end).
- A genuine response captured from a Brother MFC-J4335DW on the LAN (`test/fixtures/get-printer-attributes-response.bin`, 9235 bytes, plain `curl --data-binary`, see `examples/capture-fixture.sh`). Asserts printer-state idle and marker-levels 24/22/16/19 (M, C, Y, BK). Our generated request is byte-identical to the one sent to the printer.
- HTTP transport against an in-process loopback mock server (Content-Length and chunked responses), replaying the real fixture.

Verified manually, not in the suite: `examples/printer-status.lisp` against the live printer (Get-Printer-Attributes and Get-Jobs both returned correctly; read-only).

Not tested / not done:
- Print-Job has never been sent to any real printer. It is only tested as encoded bytes. No print job was sent during development.
- Only one printer model's responses are fixtured. Other vendors' quirks are unknown.
- No TLS (`ipps://`), no authentication, no HTTP keep-alive, no redirects, no `Expect: 100-continue`.
- No CUPS-specific operations, no Create-Job/Send-Document, no validation of attribute syntax against the IPP registry.
- Tests compare decoded structures with `equalp`, which is case-insensitive on strings.
- SBCL only (`sb-ext` string conversion, `sb-thread` in tests). Not run on other implementations.
- Not published, not submitted to Quicklisp or Ultralisp; awaiting owner approval.

## Layout

`cl-ipp.asd`, `src/` (tags, codec, requests, simplify, http, client), `test/`, `examples/`, `run-tests.sh`.

## License

MIT.
