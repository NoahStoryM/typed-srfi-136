#lang scribble/manual

@(require (for-label typed/racket/base
                     typed/srfi/9)
          "utils.rkt")

@title{Typed SRFI 9: Defining Record Types with Variance Annotations}
@defmodule[typed/srfi/9 #:packages ("typed-srfi-9")]
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

@section{Introduction}

This library provides a typed version of SRFI 9 (Defining Record Types) for Typed
Racket, with support for variance annotations on type parameters. It allows you
to @racket[define] @tech{record types} with precise control over the
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

@section{Syntax}

@margin-note{
@racket[write-type] must be a subtype of @racket[read-type].
This ensures type safety: since reading returns values of type
@racket[read-type], any value written must be compatible with
this type. Violating this constraint would allow writing values
that couldn't be safely read back.
}

@defform[(define-record-type maybe-type-vars type-name
           (constructor-name field-tag ...)
           predicate-name
           field-spec ...)
         #:grammar
         ([maybe-type-vars (v ...)]
          [field-tag [field-name : write-type read-type]
                     [field-name : read-type]]
          [field-spec [field-name accessor-name mutator-name]
                      [field-name accessor-name]])]{
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
                is @racket[Nothing].}]}
  @item{@racket[type-name] is the name of the new @tech{record type}.}
  @item{@racket[constructor-name] is the name of the constructor procedure.}
  @item{A @racket[field-tag] specifies a field.
        @itemlist[
          @item{An immutable field has the form @racket[[field-name : read-type]].}
          @item{A mutable field has the form @racket[[field-name : write-type read-type]].}]}
  @item{@racket[predicate-name] is the name of the predicate procedure.}
  @item{A @racket[field-spec] specifies the accessor and optional mutator.
        An immutable field has an accessor, while a mutable field has both an
        accessor and a mutator.}
]

This form defines the following:
@itemlist[
  @item{A struct type named @racket[type-name].}
  @item{Type aliases:
        @itemlist[
          @item{@racket[type-name]@racketidfont{Top}: all parameters at their top bound
                (e.g., @racket[Any] for @tech{covariant}, @racket[Nothing] for @tech{contravariant}).}
          @item{@racket[type-name]@racketidfont{Bot}: all parameters at their bottom bound
                (e.g., @racket[Nothing] for @tech{covariant}, @racket[Any] for @tech{contravariant}).}]}
  @item{A @racket[constructor-name] procedure to create instances of the record.}
  @item{A @racket[predicate-name] procedure (predicate for @racket[type-name]@racketidfont{Top}).}
  @item{An @racket[accessor-name] for each field.}
  @item{A @racket[mutator-name] for each mutable field.}
]
}

@section{Examples}

@subsection{Mutable Box}

A simple mutable box demonstrating @tech{variance}. The type
@racket[(Mutable-Boxof Natural Integer)] means:

@itemlist[
  @item{You can @italic{write} values of type @racket[Natural] (or any subtype)
        into the box, demonstrating @deftech{contravariance} of the write type.}
  @item{You can @italic{read} values of type @racket[Integer] (or any supertype)
        from the box, demonstrating @deftech{covariance} of the read type.}
]

@racketblock[
(define-record-type (-t1 +t1) Mutable-Boxof
  (box [v : -t1 +t1])
  box?
  [v unbox set-box!])

(: b (Mutable-Boxof Natural Integer))
(define b (box -111))
(box? (ann b (Mutable-Boxof Byte Number)))

(unbox b)
(set-box! b 0)
(unbox b)
]

@subsection{Mutable Pair}

A mutable pair composed from mutable boxes, illustrating how @tech{variance}
propagates through nested data structures.

@racketblock[
(define-record-type (-t1 +t1 -t2 +t2) Mutable-Pairof
  (make-mpair [b1 : (Mutable-Boxof -t1 +t1)]
              [b2 : (Mutable-Boxof -t2 +t2)])
  mpair?
  [b1 get-b1]
  [b2 get-b2])

(: mcons (∀ (t1 t2) (→ t1 t2 (Mutable-Pairof t1 t1 t2 t2))))
(define (mcons v1 v2) (make-mpair (box v1) (box v2)))

(: mcar (∀ (+t1) (→ (Mutable-Pairof Nothing +t1 Nothing Any) +t1)))
(: mcdr (∀ (+t2) (→ (Mutable-Pairof Nothing Any Nothing +t2) +t2)))
(define (mcar p) (unbox (get-b1 p)))
(define (mcdr p) (unbox (get-b2 p)))

(: set-mcar! (∀ (-t1) (→ (Mutable-Pairof -t1 Any Nothing Any) -t1 Void)))
(: set-mcdr! (∀ (-t2) (→ (Mutable-Pairof Nothing Any -t2 Any) -t2 Void)))
(define (set-mcar! p v1) (set-box! (get-b1 p) v1))
(define (set-mcdr! p v2) (set-box! (get-b2 p) v2))

(: p (Mutable-Pairof Natural Integer Zero Byte))
(define p (mcons -1 1))
(mpair? (ann p (Mutable-Pairof Byte Number Nothing Natural)))

(mcar p)
(set-mcar! p 1)
(mcar p)

(mcdr p)
(set-mcdr! p 0)
(mcdr p)
]
