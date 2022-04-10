#lang scribble/manual

@(require (for-label ffi/unsafe
                     raylib/support
                     racket/base
                     (except-in racket/contract/base ->)))

@title{Raylib Support}

@defmodule[raylib/support]

Various utilities to make interfacing with Raylib nicer.

@defform[(_pointer-to type)]{
A @racket[ctype?] which represents a pointer to @racket[type].

This just returns @racket[_pointer], but is more helpful for documentation.
}

@defproc[(ptr-box [type ctype?] [value any/c]) cpointer?]{
Returns a new @racket[type] pointer, with @racket[value] written to it.
}

@defform[(define-ptr name type value)]{
Define a @deftech{pointer variable}, which behaves like a normal variable,
but can be referenced as a pointer with @racket[borrow].
}

@defform[(borrow var)]{
"Borrow" a @tech{pointer variable}, taking its memory address as a @racket[cpointer?].
}

@defthing[raylib-ffi-lib ffi-lib?]{
The Racket
@tech[#:doc '(lib "scribblings/foreign/foreign.scrbl")
      ]{foreign-library value}
for Raylib.

Use this if you want to redefine any functions to better suit your application.
The alternative is to use @racket[(ffi-lib #f)] after requiring this module.
}
