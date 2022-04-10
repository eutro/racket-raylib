#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
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

(define _float3 (_array/vector _float 3))
(define _float16 (_array/vector _float 16))

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
