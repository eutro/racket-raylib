#lang scribble/manual

@title{Raylib 2D Bindings}
Unsafe bindings for @deftech{@hyperlink["https://www.raylib.com/"]{Raylib}}'s
2D components.

These bindings are currently for Raylib 4.0.

Most of these bindings are perfectly safe, as long as they are not horribly misused.
They are called and marked "unsafe", since they are a thin wrapper over a C API,
and thus undefined behaviour is possible if the bindings are used incorrectly.

@table-of-contents[]

@defmodule[raylib/2d/unsafe]
Reexports all of @racket[raylib/2d/unsafe/functions] and @racket[raylib/2d/*].

@include-section["unsafe/functions.scrbl"]
@include-section["structs.scrbl"]
@include-section["enums.scrbl"]
@include-section["constants.scrbl"]
