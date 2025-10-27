#lang typed/racket/base

(require "../../9.rkt" typed/rackunit)

(begin
  (define-record-type Point
    (make-point [x : Integer] [y : Integer])
    point?
    [x get-x]
    [y get-y])

  (define pt (make-point 1 2))
  (check-eqv? (get-x pt) 1)
  (check-eqv? (get-y pt) 2))
