#lang racket/base

(require ffi/unsafe
         (for-syntax racket/base
                     syntax/parse))

(provide _pointer-to
         ptr-box
         define-ptr
         borrow)

(define-syntax-rule (_pointer-to _type)
  _pointer)

(define (ptr-box type value)
  (define ptr (malloc type))
  (ptr-set! ptr type value)
  ptr)

(begin-for-syntax
  (struct ptr-var [type ptr]
    #:property prop:set!-transformer
    (lambda (var stx)
      (with-syntax ([type (ptr-var-type var)]
                    [ptr (ptr-var-ptr var)])
        (syntax-parse stx
          [(set! _ value)
           (syntax/loc stx
             (ptr-set! ptr type value))]
          [(this . tail)
           (syntax/loc stx
             ((#%expression . this) . tail))]
          [_
           (syntax/loc stx
             (ptr-ref ptr type))])))))

(define-syntax (define-ptr stx)
  (syntax-parse stx
    [(_ id:id type:expr value:expr)
     (syntax/loc stx
       (begin
         (define ty type)
         (define ptr (malloc ty))
         (ptr-set! ptr ty value)
         (define-syntax id (ptr-var #'ty #'ptr))))]))

(define-syntax (borrow stx)
  (define-syntax-class ptr-var-expr
    #:description "a pointer variable"
    #:attributes (borrow)
    (pattern
     var:id
     #:do [(define binding (syntax-local-value this-syntax (lambda () #f)))]
     #:fail-unless (ptr-var? binding) "binding was not defined as a pointer variable"
     #:attr borrow (ptr-var-ptr binding)))
  (syntax-parse stx
    [(_ var:ptr-var-expr)
     (syntax/loc stx
       var.borrow)]))
