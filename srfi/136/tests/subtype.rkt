#lang typed/racket/base

(require "../../136.rkt" typed/rackunit)

(begin
  (define-record-type <parent>
    (make-parent)
    parent?)
  (check-pred record? (make-parent))
  (check-pred parent? (make-parent))

  (define-record-type (<child> <parent>)
    (make-child)
    child?)
  (check-pred record? (make-child))
  (check-pred parent? (make-child))
  (check-pred child?  (make-child)))
