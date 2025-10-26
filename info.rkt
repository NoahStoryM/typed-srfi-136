#lang info

(define license 'MIT)
(define collection "typed")
(define version "0.0")

(define pkg-desc "Typed Record")

(define deps '("base" "typed-racket-lib"))
(define build-deps '("rackunit-typed"))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))
