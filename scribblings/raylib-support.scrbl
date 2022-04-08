#lang scribble/manual

@(require (for-label ffi/unsafe
                     racket/base
                     (except-in racket/contract/base ->)))

@title{Raylib Support}

Various utilities to make interfacing with Raylib nicer.

@defmodule[raylib/support]

@defform[(_pointer-to type)]{
Returns @racket[_pointer], but is more useful for documentation.
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
