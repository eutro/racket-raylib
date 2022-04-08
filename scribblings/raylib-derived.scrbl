#lang scribble/manual

@(require (for-label ffi/unsafe
                     raylib/derived
                     raylib/generated/structs
                     racket/base))

@title{Raylib Derived Utilities}

@defmodule[raylib/derived]

Derived Raylib utilities, often modified versions of raw definitions
from @racketmodname[raylib/generated/unsafe/functions].

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
