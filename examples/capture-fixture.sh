#!/bin/bash
# Capture a genuine Get-Printer-Attributes response with plain curl (read-only).
# usage: capture-fixture.sh [host] [outdir]
set -eu
HOST=${1:-BRW000000000000.local}
OUT=${2:-"$(dirname "$0")/../test/fixtures"}
U="ipp://$HOST:631/ipp/print"
{
  printf '\x02\x00\x00\x0b\x00\x00\x00\x01\x01'
  printf '\x47\x00\x12attributes-charset\x00\x05utf-8'
  printf '\x48\x00\x1battributes-natural-language\x00\x02en'
  printf '\x45\x00\x0bprinter-uri'
  printf "\\x00\\x$(printf %02x ${#U})%s" "$U"
  printf '\x03'
} > "$OUT/get-printer-attributes-request.bin"
curl -s -m 20 -H 'Content-Type: application/ipp' \
  --data-binary @"$OUT/get-printer-attributes-request.bin" \
  "http://$HOST:631/ipp/print" -o "$OUT/get-printer-attributes-response.bin" -w '%{http_code}\n'
