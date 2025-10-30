#lang typed/racket/base

(provide (struct-out record) <record>)

(struct record () #:type-name <record>)
