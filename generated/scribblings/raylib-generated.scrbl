#lang scribble/manual

@title{Generated Raylib Bindings}
Unsafe, automatically generated bindings for
@deftech{@hyperlink["https://www.raylib.com/"]{Raylib}}
4.0.

@table-of-contents[]

@defmodule[raylib/generated/unsafe]
Reexports all of @racket[raylib/generated/unsafe/functions]
and @racket[raylib/generated/*].

@include-section["unsafe/functions.scrbl"]
@include-section["structs.scrbl"]
@include-section["enums.scrbl"]
@include-section["constants.scrbl"]
