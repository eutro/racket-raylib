#lang scribble/manual

@title{Raylib 2D Bindings}

Unsafe bindings for Raylib's 2D components.

These bindings are currently for Raylib 4.0.

Most of these bindings are perfectly safe, as long as they are not horribly misused.
They are called and marked "unsafe", since they are a thin wrapper over a C API,
and thus undefined behaviour is possible if the bindings are used incorrectly.

@defmodule[raylib/2d/unsafe]

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
