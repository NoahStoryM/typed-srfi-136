#lang typed/racket/base

(require "../../9.rkt" typed/rackunit)

(begin
  (define-record-type <parent>
    (make-parent)
    parent?)
  (check-pred parent? (make-parent))

  (define-record-type (<child> <parent>)
    (make-child)
    child?)
  (check-pred parent? (make-child))
  (check-pred child?  (make-child)))
