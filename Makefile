.PHONY: test

# Offline FiveAM suite (decodes the captured printer fixtures; no network).
test:
	sbcl --noinform --non-interactive \
	  --eval '(load "~/quicklisp/setup.lisp")' \
	  --eval '(push (truename ".") asdf:*central-registry*)' \
	  --eval '(ql:quickload :cl-ipp/test :silent t)' \
	  --eval '(asdf:test-system :cl-ipp)'
