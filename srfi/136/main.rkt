#lang typed/racket/base/optional

(require (for-syntax racket/base
                     racket/case
                     racket/list
                     racket/pretty
                     racket/syntax
                     syntax/parse)
         "private/types.rkt")

(provide <record> record? define-record-type)


(begin-for-syntax
  (define-values (variance->top-type variance->bot-type)
    ;; Determine the Top type and Bottom Type based on variance annotation
    ;; If type parameter starts with '-', it's contravariant
    ;; (Top = Nothing and Bot = Any)
    ;; Otherwise, it's covariant
    ;; (Top = Any and Bot = Nothing)
    (let ()
      (define ((variance->type bot top) type-stx)
        (define type-str (symbol->string (syntax-e type-stx)))
        (if (and (positive? (string-length type-str))
                 (char=? #\- (string-ref type-str 0)))
            bot top))
      (values (variance->type #'Nothing #'Any)
              (variance->type #'Any #'Nothing))))

  (define (strip-variance-prefix stx)
    ;; Strip variance prefix (- or +) from type parameter
    ;; Returns the base type parameter name
    (define e (syntax-e stx))
    (cond
      [(symbol? e)
       (define s (symbol->string e))
       (if (positive? (string-length s))
           (case/eq (string-ref s 0)
             [(#\- #\+) (datum->syntax stx (string->symbol (substring s 1)))]
             [else stx])
           stx)]
      [(list? e)
       (datum->syntax stx (map strip-variance-prefix e))]
      [else stx]))

  (define-syntax-class type-spec
    ;; Syntax class for type specification
    [pattern (~or* name:id (name:id #f))
             #:with this #'name
             #:with super #'#f]
    [pattern (name:id parent:id)
             #:with this #'name
             #:with super #'parent])

  (define-syntax-class type-para
    ;; Syntax class for type parameters with variance annotations
    [pattern t:id
             #:with base (strip-variance-prefix #'t)
             #:with Top (variance->top-type #'t)
             #:with Bot (variance->bot-type #'t)])

  (define-syntax-class type
    ;; Syntax class for general types
    [pattern t #:with base (strip-variance-prefix #'t)]))

(begin-for-syntax
  (define-syntax-class tag
    ;; Syntax class for field tags in constructor
    [pattern [name:id (~datum :) r:type]
             #:with id #'name
             #:with r0 #'r.base
             #:with spec #'[name : (→ r)]
             #:with op
             (let ([op (format-id #'name "current-~a" #'name)])
               #`(let ([#,op (λ () name)])
                   #,op))]
    [pattern [name:id (~datum :) r:type w:type]
             #:with id #'name
             #:with r0 #'r.base
             #:with spec #'[name : (case→ (→ r) (→ w Void))]
             #:with op
             (let ([op (format-id #'name "current-~a" #'name)])
               #`(let ([#,op (case-λ [() name] [([v : w.base]) (set! name v)])])
                   #,op))])

  (define-syntax-class spec
    ;; Syntax class for field specifications (getter/setter names)
    [pattern [name:id get:id]]
    [pattern [name:id get:id set:id]]))

(begin-for-syntax
  (define (generate-field-definitions stx Type TypeTop t* field*)
    ;; Generate accessor/mutator definitions for record fields
    ;; Returns a list of definitions for getters and setters
    (define (parse-type T)
      ;; Parse type expression and collect relevant type parameters
      ;; Returns (type-vars . top-type) where type-vars are the parameters
      (define tvar*
        (remove-duplicates
         (let collect ([T T])
           (let ([T (syntax-e T)])
             (cond
               [(null? T) '()]
               [(pair? T)
                (append* (collect (car T)) (map collect (cdr T)))]
               [else
                (for/list ([t (in-list t*)] #:when (eq? T (syntax-e t))) t)])))))
      (datum->syntax stx
       (cons tvar*
             (if (null? tvar*)
                 TypeTop
                 (cons Type
                       (for/list ([t (in-list t*)])
                         (if (memq t tvar*)
                             t
                             (variance->top-type t))))))))
    (datum->syntax stx
     (for/list ([field (in-list field*)])
       (syntax-parse field
         #:datum-literals (:)
         [([name:id : r:type] . [_ get:id])
          #:with Type-name:id (format-id #f "~a-~a" Type #'name)
          #:with ((+t:id ...) . +Top) (parse-type #'r)
          (syntax/loc stx
            (begin
              (: get (∀ (+t ...) (→ +Top r)))
              (define (get record) ((Type-name record)))))]
         [([name:id : r:type w:type] . [_ get:id set:id])
          #:with Type-name:id (format-id #f "~a-~a" Type #'name)
          #:with ((+t:id ...) . +Top) (parse-type #'r)
          #:with ((-t:id ...) . -Top) (parse-type #'w)
          (syntax/loc stx
            (begin
              (: get (∀ (+t ...) (→ +Top r)))
              (: set (∀ (-t ...) (→ -Top w Void)))
              (define (get record) ((Type-name record)))
              (define (set record name) ((Type-name record) name))))])))))

(define-syntax (define-record-type stx)
  (syntax-parse stx
    [(_ (~or* (~seq) () (ts:id ...+)) T:type-spec
        (~or* #f (make-record:id . field-tags))
        (~or* #f record?:id)
        field-spec*:spec ...)
     #:with (t:type-para ...) (if (attribute ts) #'(ts ...) #'())
     #:with (field-tag*:tag ...) (if (attribute field-tags) #'field-tags #'())
     #:with This:id #'T.this
     #:with <Super>:id (if (syntax-e #'T.super) #'T.super #'<record>)
     #:with <This>:id   (format-id #f "~a"  #'This)
     #:with This?:id    (format-id #f "~a?" #'This)
     #:with makeThis:id (format-id #f "make~a" #'This)
     #:with ThisTop:id (format-id #'This "~aTop" #'This)
     #:with ThisBot:id (format-id #'This "~aBot" #'This)
     #:with (t0:id ...) (datum->syntax #'This (remove-duplicates (syntax->datum #'(t.base ...))))
     #:with ((field-tag:tag . field-spec:spec) ...)
     ;; Match field specifications with field tags
     (let ([data-hash
            (for/hasheq ([field-spec (in-list (syntax->list #'(field-spec* ...)))])
              (syntax-parse field-spec
                [[name:id . _]
                 (values (syntax-e #'name) field-spec)]))])
       (datum->syntax #'(field-spec* ...)
        (for/list ([field-tag (in-list (syntax->list #'(field-tag* ...)))]
                   #:do
                   [(define field-spec
                      (syntax-parse field-tag
                        [[name:id . _]
                         (hash-ref data-hash (syntax-e #'name) #f)]))]
                   #:when field-spec)
          (cons field-tag field-spec))))
     #:with field-def*
     (generate-field-definitions
      #'This
      (syntax-e #'This)
      (syntax-e #'ThisTop)
      (syntax-e #'(t ...))
      (syntax-e #'((field-tag . field-spec) ...)))
     (quasisyntax/loc stx
       (begin
         (struct (t ...) <This> <Super>
           (field-tag.spec ...)
           #:constructor-name makeType
           #:type-name This)
         #,@(if (attribute ts)
                ;; Type definitions for polymorphic case
                #`((define-type ThisTop (This t.Top ...))
                   (define-type ThisBot (This t.Bot ...))
                   #,@(if (attribute make-record)
                          #'((: make-record (∀ (t0 ...) (→ field-tag*.r0 ... (This t.base ...))))
                             (define (make-record field-tag*.id ...) (makeType field-tag*.op ...)))
                          #'())
                   .
                   #,(if (attribute record?)
                         #'((define record? (cast This? (pred ThisTop))))
                         #'()))
                ;; Type definitions for non-polymorphic case
                #`((define-type ThisTop This)
                   (define-type ThisBot This)
                   #,@(if (attribute make-record)
                          #'((: make-record (→ field-tag*.r0 ... This))
                             (define (make-record field-tag*.id ...) (makeType field-tag*.op ...)))
                          #'())
                   .
                   #,(if (attribute record?)
                         #'((define record? This?))
                         #'())))
         .
         field-def*))]))
