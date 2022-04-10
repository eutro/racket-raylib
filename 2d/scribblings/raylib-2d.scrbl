#lang scribble/manual

@title{Raylib 2D Bindings}

@defmodule[raylib/2d/unsafe]

Unsafe bindings for Raylib's 2D components.

They are called and marked unsafe, since they can cause undefined behaviour
if misused, as they are a thin wrapper over a C API.

This module re-exports all of
@racketmodname[raylib/2d/unsafe/functions],
@racketmodname[raylib/2d/structs],
@racketmodname[raylib/2d/enums] and
@racketmodname[raylib/2d/constants].

@table-of-contents[]

@include-section["unsafe/functions.scrbl"]

@section{2D Structs}
@defmodule[raylib/2d/structs]
Re-exports @racketmodname[raylib/generated/structs].

@section{2D Enums}
@defmodule[raylib/2d/enums]
Re-exports @racketmodname[raylib/generated/enums].

@section{2D Constants}
@defmodule[raylib/2d/constants]
Re-exports @racketmodname[raylib/generated/constants].
