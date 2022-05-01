#lang scribble/manual

@(require raylib/generated/constants)

@title{Raylib}

Racket bindings for @hyperlink["https://www.raylib.com/"]{Raylib}.

These bindings are currently for
@(hyperlink
  @(format "https://github.com/raysan5/raylib/releases/tag/~a.0" RAYLIB_VERSION)
  @list{Raylib @|RAYLIB_VERSION|.0}).

@table-of-contents[]

@include-section["raylib-support.scrbl"]
@include-section["../generated/scribblings/raylib-generated.scrbl"]
@include-section["raylib-derived.scrbl"]
@include-section["../2d/scribblings/raylib-2d.scrbl"]
@include-section["../raymath/raylib-raymath.scrbl"]
