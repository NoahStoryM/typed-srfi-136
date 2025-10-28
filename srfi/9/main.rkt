#lang typed/racket/base/optional

(require (for-syntax racket/base
                     racket/case
                     racket/list
                     racket/syntax
                     syntax/parse))

(provide define-record-type)


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
             #:with base (strip-variance-prefix  #'t)
             #:with Top (variance->top-type #'t)
             #:with Bot (variance->bot-type #'t)])

  (define-syntax-class type
    ;; Syntax class for general types
    [pattern t #:with base (strip-variance-prefix #'t)]))

(begin-for-syntax
  (define-syntax-class tag
    ;; Syntax class for field tags in constructor
    [pattern [name:id (~datum :) w:type r:type]
             #:with id #'name
             #:with r0 #'r.base
             #:with spec #'[name : (case→ (→ r) (→ w Void))]
             #:with op #'(case-λ [() name] [([v : w.base]) (set! name v)])]
    [pattern [name:id (~datum :) t:type]
             #:with id #'name
             #:with r0 #'t.base
             #:with spec #'[name : (→ t)]
             #:with op #'(λ () : t.base name)])

  (define-syntax-class spec
    ;; Syntax class for field specifications (getter/setter names)
    [pattern [name:id get:id set:id]]
    [pattern [name:id get:id]]))

(begin-for-syntax
  (define (generate-field-accessors stx Type TypeTop t* field*)
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
         [([name:id : w:type r:type] . [_ get:id set:id])
          #:with Typeof-name:id (format-id #f "~aof-~a" Type #'name)
          #:with ((-t:id ...) . -Top) (parse-type #'w)
          #:with ((+t:id ...) . +Top) (parse-type #'r)
          (syntax/loc stx
            (begin
              (: set (∀ (-t ...) (→ -Top w Void)))
              (: get (∀ (+t ...) (→ +Top r)))
              (define (set record name) ((Typeof-name record) name))
              (define (get record) ((Typeof-name record)))))]
         [([name:id : t:type] . [_ get:id])
          #:with Typeof-name:id (format-id #f "~aof-~a" Type #'name)
          #:with ((+t:id ...) . +Top) (parse-type #'t)
          (syntax/loc stx
            (begin
              (: get (∀ (+t ...) (→ +Top t)))
              (define (get record) ((Typeof-name record)))))])))))

(define-syntax (define-record-type stx)
  (syntax-parse stx
    [(_ (~or* (~seq) () (t*:id ...+)) Type:type-spec
        ((~or* #f make-record:id) field-tag*:tag ...)
        (~or* #f record?:id)
        field-spec*:spec ...)
     #:with (t:type-para ...) (if (attribute t*) #'(t* ...) #'())
     #:with Typeof:id   (format-id #f "~aof"   (syntax-e #'Type.this))
     #:with Typeof?:id  (format-id #f "~aof?"  (syntax-e #'Type.this))
     #:with makeType:id (format-id #f "make~a" (syntax-e #'Type.this))
     #:with TypeTop:id (format-id #'Type "~aTop" (syntax-e #'Type.this))
     #:with TypeBot:id (format-id #'Type "~aBot" (syntax-e #'Type.this))
     #:with (name-spec ...) (if (syntax-e #'Type.super) #'(Typeof Type.super) #'(Typeof))
     #:with (t0:id ...) (datum->syntax #'Type (remove-duplicates (syntax->datum #'(t.base ...))))
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
     #:with (field-def ...)
     (generate-field-accessors
      #'Type
      (syntax-e #'Type.this)
      (syntax-e #'TypeTop)
      (syntax-e #'(t ...))
      (syntax-e #'((field-tag . field-spec) ...)))
     (quasisyntax/loc stx
       (begin
         (struct (t ...) name-spec ...
           (field-tag.spec ...)
           #:constructor-name makeType
           #:type-name Type.this)
         #,@(if (attribute t*)
                ;; Type definitions for polymorphic case
                #`((define-type TypeTop (Type.this t.Top ...))
                   (define-type TypeBot (Type.this t.Bot ...))
                   #,@(if (attribute make-record)
                          #'((: make-record (∀ (t0 ...) (→ field-tag*.r0 ... (Type.this t.base ...))))
                             (define (make-record field-tag*.id ...) (makeType field-tag*.op ...)))
                          #'()))
                ;; Type definitions for non-polymorphic case
                #`((define-type TypeTop Type.this)
                   (define-type TypeBot Type.this)
                   #,@(if (attribute make-record)
                          #'((: make-record (→ field-tag*.r0 ... Type.this))
                             (define (make-record field-tag*.id ...) (makeType field-tag*.op ...)))
                          #'())))
         #,@(if (attribute record?)
                #'((define record? (cast Typeof? (pred TypeTop))))
                #'())
         field-def ...))]))
