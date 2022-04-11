#lang scribble/manual

@(require racket/include
          (for-label racket/base
                     ffi/unsafe
                     raylib/support
                     raylib/generated/structs
                     raylib/raymath/unsafe)
          (for-syntax racket/base
                      syntax/parse))

@title{Raymath}

@defmodule[raylib/raymath/unsafe]

Manually written bindings for Raylib's Raymath module.

These are marked as unsafe because safety cannot be guaranteed,
but you are very unlikely to experience undefined behaviour using this.

@deftogether[(@defthing[_float3 ctype?]
              @defthing[_float16 ctype?])]{
Struct-wrapped float arrays of 3 and 16 elements respectively.

These are converted to and from @racket[flvector?]s of the given length.
}

@(define-syntax (define-raymath stx)
   (syntax-parse stx
     #:track-literals
     #:datum-literals (-> : _ptr)
     [(_) #'(begin)]
     [(_
       [description:str
        ...
        name:id -> return
        ([param-name : param-type] ...)]
       etc ...)
      (syntax/loc stx
        (begin
          (defproc (name [param-name param-type] ...) return
            {~@ description "\n" "\n"} ...)
          (define-raymath etc ...)))]
     [(_
       [description:str
        ...
        name:id -> return
        output:expr
        ({~or [param-name : param-type:id]
              [out-name : (_ptr {~datum o} out-ty:expr)]}
         ...)]
       etc ...)
      (syntax/loc stx
        (begin
          (defproc (name {~? [param-name param-type]} ...)
            (values {~? out-ty} ...)
            "Returns " @racket[(values {~? out-name} ...)] "."
            "\n"
            "\n"
            {~@ description "\n" "\n"} ...)
          (define-raymath etc ...)))]))

@(include "./private/defs.rkt")
