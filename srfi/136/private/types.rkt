#lang typed/racket/base

(provide record-inspector
         record-type-inspector
         (struct-out <record>)
         (rename-out [<record>? record?]))

(define struct-inspector (current-inspector))
(define record-inspector (make-sibling-inspector))
(define record-type-inspector (make-inspector record-inspector))
(current-inspector record-type-inspector)
(struct <record> ())
(current-inspector struct-inspector)
