#lang typed/racket/base

(require "../../136.rkt" typed/rackunit)

(begin
  (define-record-type (-t1 +t1) Mutable-Box
    (box [v : -t1 +t1])
    box?
    [v unbox set-box!])

  (: b (Mutable-Box Natural Integer))
  (define b (box -111))
  (check-pred box? b)
  (check-pred box? (ann b (Mutable-Box Byte Number)))

  (check-eqv? (unbox b) -111)
  (set-box! b 0)
  (check-eqv? (unbox b) 0))

(begin
  (define-record-type (-t1 +t1 -t2 +t2) Mutable-Pair
    (make-mpair [b1 : (Mutable-Box -t1 +t1)]
                [b2 : (Mutable-Box -t2 +t2)])
    mpair?
    [b2 get-b2]
    [b1 get-b1])

  (: mcons (∀ (t1 t2) (→ t1 t2 (Mutable-Pair t1 t1 t2 t2))))
  (define (mcons v1 v2) (make-mpair (box v1) (box v2)))

  (: mcar (∀ (+t1) (→ (Mutable-Pair Nothing +t1 Nothing Any) +t1)))
  (: mcdr (∀ (+t2) (→ (Mutable-Pair Nothing Any Nothing +t2) +t2)))
  (define (mcar p) (unbox (get-b1 p)))
  (define (mcdr p) (unbox (get-b2 p)))

  (: set-mcar! (∀ (-t1) (→ (Mutable-Pair -t1 Any Nothing Any) -t1 Void)))
  (: set-mcdr! (∀ (-t2) (→ (Mutable-Pair Nothing Any -t2 Any) -t2 Void)))
  (define (set-mcar! p v1) (set-box! (get-b1 p) v1))
  (define (set-mcdr! p v2) (set-box! (get-b2 p) v2))

  (: p (Mutable-Pair Natural Integer Zero Byte))
  (define p (mcons -1 1))
  (check-pred mpair? p)
  (check-pred mpair? (ann p (Mutable-Pair Byte Number Nothing Natural)))

  (check-eqv? (mcar p) -1)
  (set-mcar! p 1)
  (check-eqv? (mcar p) 1)

  (check-eqv? (mcdr p) 1)
  (set-mcdr! p 0)
  (check-eqv? (mcdr p) 0))
