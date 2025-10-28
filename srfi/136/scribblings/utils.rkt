#lang racket/base

(require scribble/manual
         scribble/example
         (for-syntax racket/base
                     syntax/parse))

(provide (all-defined-out))

(define (make-typed-srfi-136-eval) (make-base-eval #:lang 'typed/racket/base '(require typed/srfi/136)))
(define-syntax-rule (typed-srfi-136-examples body ...) (examples #:eval (make-typed-srfi-136-eval) body ...))

(define-syntax (define-tech stx)
  (syntax-parse stx
    [(_ tech/name module-path)
     (syntax/loc stx
       (define (tech/name
                #:key          [key        #f]
                #:normalize?   [normalize? #t]
                #:tag-prefixes [prefixes   #f]
                #:indirect?    [indirect?  #f]
                .
                pre-content*)
         (apply tech
                #:key          key
                #:normalize?   normalize?
                #:doc          module-path
                #:tag-prefixes prefixes
                #:indirect?    indirect?
                pre-content*)))]))

(define-tech tech/guide '(lib "scribblings/guide/guide.scrbl"))
(define-tech tech/refer '(lib "scribblings/reference/reference.scrbl"))
