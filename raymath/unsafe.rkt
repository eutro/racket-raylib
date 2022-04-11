#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         racket/flonum
         raylib/support
         raylib/generated/structs
         racket/include
         (for-syntax racket/base
                     syntax/parse))

(provide _float3
         _float16)

(define-ffi-definer define-raylib raylib-ffi-lib
  #:provide provide-protected
  #:default-make-fail make-not-available)

(define (_array/flvector len)
  (define array-type (make-array-type _float len))
  (make-ctype
   array-type
   (λ (r)
     (unless (and (flvector? r)
                  (>= (flvector-length r) len))
       (raise-argument-error
        '_array/flvector
        (format "flvector? with length at least ~a" len)
        r))
     (define ptr (malloc _float len))
     (for ([i (in-range len)]
           [v (in-flvector r)])
       (ptr-set! ptr i v))
     ptr)
   (λ (c)
     (for/flvector
         #:length len
         ([i (in-range len)])
       (ptr-ref c _float i)))))

(define (_floatn len)
  (define flvec-type (_array/flvector len))
  (define struct-type (make-cstruct-type (list flvec-type)))
  (make-ctype
   struct-type
   (λ (r) (cast r flvec-type struct-type))
   (λ (c) (cast c struct-type flvec-type))))

(define _float3 (_floatn 3))
(define _float16 (_floatn 16))

(define-syntax (define-raymath stx)
  (syntax-parse stx
    #:track-literals
    #:datum-literals (-> :)
    [(_
      [description:str
       ...
       name:id -> return
       {~optional output:expr}
       ([param-name : param-type] ...)]
      ...)
     (syntax/loc stx
       (begin
         (define-raylib name
           (_fun
            [param-name : param-type] ...
            -> return
            (~? (~@ -> output))))
         ...))]))

(include "private/defs.rkt")
