#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         raylib/generated/unsafe
         raylib/support)

(define-ffi-definer define-raylib raylib-ffi-lib
  #:provide provide-protected
  #:default-make-fail make-not-available)

(provide-protected FilePathList->vector)
(define (FilePathList->vector fpl)
  (cblock->vector
   (FilePathList-paths fpl)
   _string
   (FilePathList-count fpl)))

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
