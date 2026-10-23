#!/bin/sh
# Run the offline test suite: ./run-tests.sh   (needs sbcl + quicklisp in ~/quicklisp)
cd "$(dirname "$0")" || exit 1
exec sbcl --noinform --non-interactive \
  --load "${QUICKLISP_SETUP:-$HOME/quicklisp/setup.lisp}" \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(ql:quickload :cl-ipp/test :silent t)' \
  --eval '(asdf:test-system :cl-ipp)'
