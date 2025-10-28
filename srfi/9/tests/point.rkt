#lang typed/racket/base

(require "../../9.rkt" typed/rackunit)

(begin
  (define-record-type Point (#f) #f))

(begin
  (define-record-type () (Point-0 Point)
    (make-point-0)
    point-0?)
  (define point-0 (make-point-0))
  (check-pred point-0? point-0))

(begin
  (define-record-type (-t1 +t1) (Point-1 Point-0)
    (make-point-1 [p1 : -t1 +t1])
    point-1?
    [p1 get-p1 set-p1!])
  (define point-1 (make-point-1 1))
  (check-pred point-0? point-1)
  (check-pred point-1? point-1)

  (check-eqv? (get-p1 point-1) 1)
  (set-p1! point-1 -1)
  (check-eqv? (get-p1 point-1) -1))

(begin
  (define-record-type (-t1 +t1 -t2 +t2) (Point-2 Point-1)
    (make-point-2 [p1 : -t1 +t1] [p2 : -t2 +t2])
    point-2?
    [p2 get-p2 set-p2!])
  (define point-2 (make-point-2 1 2))
  (check-pred point-0? point-2)
  (check-pred point-1? point-2)
  (check-pred point-2? point-2)

  (check-eqv? (get-p1 point-2) 1)
  (set-p1! point-2 -1)
  (check-eqv? (get-p1 point-2) -1)

  (check-eqv? (get-p2 point-2) 2)
  (set-p2! point-2 -2)
  (check-eqv? (get-p2 point-2) -2))
