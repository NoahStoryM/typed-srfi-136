#lang info

(define license 'MIT)
(define collection "typed")
(define version "0.0")

(define pkg-desc "Typed Record")

(define deps '("base" "typed-racket-lib"))
(define build-deps '("scribble-lib" "rackunit-typed" "racket-doc" "typed-racket-doc"))

(define scribblings '(("srfi/136/scribblings/typed-srfi-136.scrbl")))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))
