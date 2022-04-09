#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         raylib/generated/unsafe
         raylib/support)

(define-ffi-definer define-raylib raylib-ffi-lib
  #:provide provide-protected
  #:default-make-fail make-not-available)

(define-raylib GetDroppedFiles*
  (_fun
   [count : (_ptr o _int)]
   -> (_vector o _string count))
  #:c-id GetDroppedFiles
  #:wrap
  (lambda (proc)
    (define (GetDroppedFiles*)
      (define ret (proc))
      (ClearDroppedFiles)
      ret)
    GetDroppedFiles*))

(define-raylib DrawLineStrip*
  (_fun
   [points : (_vector i _Vector2)]
   [_int = (vector-length points)]
   [color : _Color]
   -> _void)
  #:c-id DrawLineStrip)

(define-raylib DrawTriangleFan*
  (_fun
   [points : (_vector i _Vector2)]
   [_int = (vector-length points)]
   [color : _Color]
   -> _void)
  #:c-id DrawTriangleFan)

(define-raylib DrawTriangleStrip*
  (_fun
   [points : (_vector i _Vector2)]
   [_int = (vector-length points)]
   [color : _Color]
   -> _void)
  #:c-id DrawTriangleStrip)
