#lang scribble/manual

@(require (for-label typed/racket/base
                     typed/srfi/136)
          "utils.rkt")

@title{Typed SRFI 136: Extensible Record Types with Variance Annotations}
@defmodule[typed/srfi/136 #:packages ("typed-srfi-136")]
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

@section{Introduction}

This library provides a typed version of SRFI 136 (Extensible Record Types) for
Typed Racket, with support for variance annotations on type parameters. It extends
SRFI 9 by adding @deftech{inheritance}, allowing you to define @tech{record types}
that extend existing @tech{record types}, with precise control over the
@deftech{variance} of mutable and immutable fields.

@tech{Variance} determines how subtyping relationships between type parameters
affect subtyping of the parameterized type:
@itemlist[
  @item{@deftech{Contravariant} positions reverse subtyping order: if @racket[a] is
        a subtype of @racket[b], then @racket[(F b)] is a subtype of @racket[(F a)].}
  @item{@deftech{Covariant} positions preserve subtyping order: if @racket[a] is
        a subtype of @racket[b], then @racket[(F a)] is a subtype of @racket[(F b)].}
  ]

For a @tech{record type} @racket[(F w r)], if @racket[w] is a @tech{contravariant}
position and @racket[r] is a @tech{covariant} position, the type @racket[(F w2 r2)]
is a subtype of @racket[(F w1 r1)] if and only if:
@itemlist[
  @item{@racket[w1] is a subtype of @racket[w2]}
  @item{@racket[r2] is a subtype of @racket[r1]}
  ]

@section{API Reference}

@defform[#:literals (:)
         (define-record-type maybe-type-vars type-spec
           constructor-spec
           predicate-spec
           field-spec ...)
         #:grammar
         ([maybe-type-vars (code:line)
                           (v ...)]
          [type-spec type-name
                     (type-name #f)
                     (type-name parent)]
          [constructor-spec #f
                            (constructor-name field-tag ...)]
          [predicate-spec #f
                          predicate-name]
          [field-tag [field-name : read-type]
                     [field-name : read-type write-type]]
          [field-spec [field-name accessor-name]
                      [field-name accessor-name mutator-name]])]{
Defines a new @deftech{record type} with optional type parameters and
@tech{variance} annotations.

@itemlist[
  @item{@racket[v] is a type parameter. It uses prefix notation to indicate
        @tech{variance}:
        @itemlist[
          @item{@litchar{-} prefix (e.g., @racket[-t]): @tech{Contravariant}
                parameter. Top type is @racket[Nothing], bottom type is @racket[Any].}
          @item{@litchar{+} prefix or no prefix (e.g., @racket[+t] or @racket[t]):
                @tech{Covariant} parameter. Top type is @racket[Any], bottom type
                is @racket[Nothing].}
          ]}
  @item{@racket[type-spec] specifies the @tech{record type} name and optional
        @racket[parent]:
        @itemlist[
          @item{@racket[type-name] or @racket[(type-name #f)]: defines a new
                base @tech{record type}.}
          @item{@racket[(type-name parent)]: defines a @tech{record type} that
                @deftech{inherit}s from @racket[parent], including all its fields}
          ]}
  @item{@racket[constructor-name] is the name of the constructor procedure.
        If @racket[#f], no constructor is generated.}
  @item{A @racket[field-tag] specifies a field.
        @itemlist[
          @item{An immutable field has the form @racket[[field-name : read-type]].}
          @item{A mutable field has the form @racket[[field-name : read-type write-type]].}
          @item{When inheriting, you must specify @italic{all} fields, including
                the inherited fields.}
          ]}
  @item{@racket[predicate-name] is the name of the predicate procedure.
        If @racket[#f], no predicate is generated.}
  @item{A @racket[field-spec] specifies the accessor and optional mutator:
        @itemlist[
          @item{An immutable field has only an accessor.}
          @item{A mutable field has both an accessor and a mutator.}
          @item{When inheriting, you only specify the @italic{new} fields added
                by this @tech{record type}, not the inherited fields.}
          ]}
  ]

This form defines the following:

@itemlist[
  @item{A struct type named @racket[type-name] that optionally extends
        @racket[parent].}
  @item{Type aliases (if the type parameters are not @racket[()]) :
        @itemlist[
          @item{@racket[type-name]@racketidfont{Top}: all parameters at their top bound
                (e.g., @racket[Any] for @tech{covariant}, @racket[Nothing] for @tech{contravariant}).}
          @item{@racket[type-name]@racketidfont{Bot}: all parameters at their bottom bound
                (e.g., @racket[Nothing] for @tech{covariant}, @racket[Any] for @tech{contravariant}).}
          ]}
  @item{A @racket[constructor-name] procedure (if not @racket[#f]) to create instances.
        For mutable fields, the @racket[write-type] must be a subtype of the
        @racket[read-type]. To satisfy this subtyping constraint from the very
        beginning in the simplest way, the @tech{variance} prefixes
        (@litchar{-} and @litchar{+}) are stripped from the type parameters
        when generating the constructor's type.}
  @item{A @racket[predicate-name] procedure (if not @racket[#f]) that tests for
        @racket[type-name]@racketidfont{Top}.}
  @item{An @racket[accessor-name] for each specified field.}
  @item{A @racket[mutator-name] for each specified mutable field.}
  ]
}

@defproc[(record? [v Any]) Boolean]{
Returns @racket[#t] if @racket[v] is an instance of a record,
@racket[#f] otherwise.
}

@typed-srfi-136-examples[
(:print-type record?)
]

@section{Examples}

@subsection{Mutable Boxes and Pairs}

A simple mutable box demonstrating @tech{variance}, and a mutable pair composed
from mutable boxes, illustrating how @tech{variance} propagates through nested
data structures. The type @racket[(Mutable-Box Natural Integer)] means:

@itemlist[
  @item{You can @italic{write} values of type @racket[Natural] (or any subtype)
        into the box, demonstrating @deftech{contravariance} of the write type.}
  @item{You can @italic{read} values of type @racket[Integer] (or any supertype)
        from the box, demonstrating @deftech{covariance} of the read type.}
  ]

@typed-srfi-136-examples[
(define-record-type (-t1 +t1) Mutable-Box
  (BOX [v : +t1 -t1])
  BOX?
  [v UNBOX SET-BOX!])

(:print-type BOX)
(:print-type BOX?)
(:print-type UNBOX)
(:print-type SET-BOX!)

(: b (Mutable-Box Natural Integer))
(define b (BOX -111))
(BOX? (ann b (Mutable-Box Byte Number)))

(UNBOX b)
(SET-BOX! b 0)
(UNBOX b)

(define-record-type (-t1 +t1 -t2 +t2) Mutable-Pair
  (make-mpair [b1 : (Mutable-Box -t1 +t1)]
              [b2 : (Mutable-Box -t2 +t2)])
  MPAIR?
  [b2 get-b2]
  [b1 get-b1])

(:print-type make-mpair)
(:print-type MPAIR?)
(:print-type get-b1)
(:print-type get-b2)

(: MCONS (∀ (t1 t2) (→ t1 t2 (Mutable-Pair t1 t1 t2 t2))))
(define (MCONS v1 v2) (make-mpair (BOX v1) (BOX v2)))

(: MCAR (∀ (+t1) (→ (Mutable-Pair Nothing +t1 Nothing Any) +t1)))
(: MCDR (∀ (+t2) (→ (Mutable-Pair Nothing Any Nothing +t2) +t2)))
(define (MCAR p) (UNBOX (get-b1 p)))
(define (MCDR p) (UNBOX (get-b2 p)))

(: SET-MCAR! (∀ (-t1) (→ (Mutable-Pair -t1 Any Nothing Any) -t1 Void)))
(: SET-MCDR! (∀ (-t2) (→ (Mutable-Pair Nothing Any -t2 Any) -t2 Void)))
(define (SET-MCAR! p v1) (SET-BOX! (get-b1 p) v1))
(define (SET-MCDR! p v2) (SET-BOX! (get-b2 p) v2))

(: p (Mutable-Pair Natural Integer Zero Byte))
(define p (MCONS -1 1))
(MPAIR? (ann p (Mutable-Pair Byte Number Nothing Natural)))

(MCAR p)
(SET-MCAR! p 1)
(MCAR p)

(MCDR p)
(SET-MCDR! p 0)
(MCDR p)
]
@subsection{Inheritance}

A hierarchy of point types with increasingly dimensions:

@typed-srfi-136-examples[
(define-record-type Point #f #f)

(define-record-type () (Point-0 Point)
  (make-point-0)
  point-0?)

(:print-type make-point-0)
(:print-type point-0?)

(define p0 (make-point-0))
(point-0? p0)

(define-record-type (-t1 +t1) (Point-1 Point-0)
  (make-point-1 [p1 : +t1 -t1])
  point-1?
  [p1 get-p1 set-p1!])

(:print-type make-point-1)
(:print-type point-1?)
(:print-type get-p1)
(:print-type set-p1!)

(define p1 (make-point-1 1))
(point-0? p1)
(point-1? p1)

(get-p1 p1)
(set-p1! p1 -1)
(get-p1 p1)

(define-record-type (-t1 +t1 -t2 +t2) (Point-2 Point-1)
  (make-point-2 [p1 : +t1 -t1] [p2 : +t2 -t2])
  point-2?
  [p2 get-p2 set-p2!])

(:print-type make-point-2)
(:print-type point-2?)
(:print-type get-p2)
(:print-type set-p2!)

(define p2 (make-point-2 1 2))
(point-0? p2)
(point-1? p2)
(point-2? p2)

(get-p1 p2)
(set-p1! p2 -1)
(get-p1 p2)

(get-p2 p2)
(set-p2! p2 -2)
(get-p2 p2)
]
