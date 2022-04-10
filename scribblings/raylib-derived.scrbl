#lang scribble/manual

@(require (for-label (except-in ffi/unsafe ->)
                     racket/contract/base
                     raylib/derived/unsafe
                     raylib/generated/structs
                     raylib/generated/unsafe/functions
                     racket/base))

@title{Raylib Derived Utilities}

@defmodule[raylib/derived/unsafe]

Derived Raylib utilities, often modified versions of raw definitions
from @racketmodname[raylib/generated/unsafe/functions].

These are still unsafe, in that they can cause undefined behaviour if misused.

@defproc[(GetDroppedFiles*) (vectorof string?)]{
Modified version of @racket[GetDroppedFiles], which returns a vector directly, and clears the
memory Raylib allocates for the return value.
}

@defproc*[([(DrawLineStrip* [points (vectorof Vector2?)] [color Color?]) void?]
           [(DrawTriangleFan* [points (vectorof Vector2?)] [color Color?]) void?]
           [(DrawTriangleStrip* [points (vectorof Vector2?)] [color Color?]) void?])]{
Modified versions of @racket[DrawLineStrip], @racket[DrawTriangleFan] and @racket[DrawTriangleStrip]
respectively, accepting a vector instead of a raw pointer.
}
