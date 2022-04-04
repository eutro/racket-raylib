#lang racket/base

(require ffi/unsafe)

(provide (all-defined-out))

;; Color, 4 components, R8G8B8A8 (32bit)
(define-cstruct _Color
  ([r _ubyte] ; Color red value
   [g _ubyte] ; Color green value
   [b _ubyte] ; Color blue value
   [a _ubyte] ; Color alpha value
   ))

(define _TraceLogCallback
  (_fun
   [logLevel : _int]
   [text : _string]
   [args : _byte #;"va_list"]
   -> _void))

(define _LoadFileDataCallback
  (_fun
   [fileName : _string]
   [bytesRead : _pointer #;"unsigned int *"]
   -> _pointer #;"unsigned char *"))

(define _SaveFileDataCallback
  (_fun
   [fileName : _string]
   [data : _pointer #;"void *"]
   [bytesToWrite : _uint]
   -> _bool))

(define _LoadFileTextCallback
  (_fun
   [fileName : _string]
   -> _pointer #;"char *"))

(define _SaveFileTextCallback
  (_fun
   [fileName : _string]
   [text : _pointer #;"char *"]
   -> _bool))

(define _AudioCallback
  (_fun
   [bufferData : _pointer #;"void *"]
   [frames : _uint]
   -> _void))
