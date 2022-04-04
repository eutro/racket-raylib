#lang racket/base

(require ffi/unsafe ffi/unsafe/define "structs.rkt" "enums.rkt")

(define-ffi-definer define-raylib (ffi-lib "libraylib")
  #:provide provide-protected
  #:default-make-fail make-not-available)
