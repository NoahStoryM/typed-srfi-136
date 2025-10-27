#lang typed/racket/base/optional

(require (for-syntax racket/base
                     racket/case
                     racket/list
                     racket/syntax
                     syntax/parse))

(provide define-record-type)


(begin-for-syntax
  (define (variance->top-type type-stx)
    ;; Determine the Top type based on variance annotation
    ;; If type parameter starts with '-', it's contravariant (Top = Nothing)
    ;; Otherwise, it's covariant (Top = Any)
    (define type-str (symbol->string (syntax-e type-stx)))
    (if (and (positive? (string-length type-str))
             (char=? #\- (string-ref type-str 0)))
        #'Nothing
        #'Any))

  (define (variance->bot-type type-stx)
    ;; Determine the Bottom type based on variance annotation
    ;; If type parameter starts with '-', it's contravariant (Bot = Any)
    ;; Otherwise, it's covariant (Bot = Nothing)
    (define type-str (symbol->string (syntax-e type-stx)))
    (if (and (positive? (string-length type-str))
             (char=? #\- (string-ref type-str 0)))
        #'Any
        #'Nothing))

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
          #:with Typeof-name (format-id #f "~a-~a" Type #'name)
          #:with ((-t ...) . -Top) (parse-type #'w)
          #:with ((+t ...) . +Top) (parse-type #'r)
          (syntax/loc stx
            (begin
              (: set (∀ (-t ...) (→ -Top w Void)))
              (: get (∀ (+t ...) (→ +Top r)))
              (define (set record name) ((Typeof-name record) name))
              (define (get record) ((Typeof-name record)))))]
         [([name:id : t:type] . [_ get:id])
          #:with Typeof-name (format-id #f "~a-~a" Type #'name)
          #:with ((+t ...) . +Top) (parse-type #'t)
          (syntax/loc stx
            (begin
              (: get (∀ (+t ...) (→ +Top t)))
              (define (get record) ((Typeof-name record)))))])))))

(define-syntax (define-record-type stx)
  (syntax-parse stx
    [(_ (~or* (~seq) () (t* ...+)) Type:id
        (make-record:id field-tag:tag ...)
        record?:id
        .
        field-spec*)
     #:with (t:type-para ...) (if (attribute t*) #'(t* ...) #'())
     #:with Typeof  (datum->syntax #f (syntax-e #'Type))
     #:with Typeof? (format-id #f "~a?" (syntax-e #'Type))
     #:with TypeTop (format-id #'Type "~aTop" (syntax-e #'Type))
     #:with TypeBot (format-id #'Type "~aBot" (syntax-e #'Type))
     #:with (t0 ...) (datum->syntax #'Type (remove-duplicates (syntax->datum #'(t.base ...))))
     #:with (field-spec:spec ...)
     (let ([data-hash
            (for/hasheq ([field-spec (in-list (syntax->list #'field-spec*))])
              (syntax-parse field-spec
                [[name:id . _]
                 (values (syntax-e #'name) field-spec)]))])
       (for/list ([field-tag (in-list (syntax->list #'(field-tag ...)))])
         (syntax-parse field-tag
           [[name:id . _]
            (hash-ref data-hash (syntax-e #'name))])))
     #:with (field-def ...)
     (generate-field-accessors
       #'Type
       (syntax-e #'Type)
       (syntax-e #'TypeTop)
       (syntax-e #'(t ...))
       (syntax-e #'((field-tag . field-spec) ...)))
     (quasisyntax/loc stx
       (begin
         (struct (t ...) Typeof
           (field-tag.spec ...)
           #:constructor-name make-Typeof)
         #,(if (attribute t*)
               #'(begin
                   (define-type (Type t ...) (Typeof t ...))
                   (define-type TypeTop (Type t.Top ...))
                   (define-type TypeBot (Type t.Bot ...))
                   (: make-record (∀ (t0 ...) (→ field-tag.r0 ... (Type t.base ...)))))
               #'(begin
                   (define-type Type Typeof)
                   (define-type TypeTop Type)
                   (define-type TypeBot Type)
                   (: make-record (→ field-tag.r0 ... Type))))
         (define (make-record field-tag.id ...)
           (make-Typeof field-tag.op ...))
         (define record? (cast Typeof? (pred TypeTop)))
         field-def ...))]))
