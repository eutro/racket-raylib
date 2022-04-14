#lang scribble/manual

@(require (for-label ffi/unsafe
                     ffi/unsafe/alloc
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
Define a @deftech{pointer variable}, as if by
@racket[(define name value)],
which behaves like a normal variable,
but can be referenced as a pointer with @racket[borrow].

@racket[type] must evaluate to an appropriate @racket[ctype?], and
values will be converted to and from this whenever
@racket[name] is referenced or @racket[set!].
}

@defform[(borrow var)]{
"Borrow" a @tech{pointer variable}, taking its memory address as a @racket[cpointer?].
}

@defproc[(attach-cleanup [cleanup (-> any/c any)] [value any/c]) any/c]{
Attach a cleanup procedure to @racket[value], as if by @racket[allocator], returning the value.

@racket[cleanup] is added as a finalizer for @racket[value], such that it is called
with @racket[value] when it is about to be garbage collected, or under other similar
conditions.

See @racket[allocator] for more details on behavior.
Notably, a second call to @racket[attach-cleanup] overwrites the original cleanup procedure.
}

@defproc[(call-with-cleanup
          [cleanup (-> any/c any)]
          [value any/c]
          [proc (-> any/c any)])
         any]{
Call @racket[proc] with @racket[value], calling @racket[cleanup] on @racket[value] on exit
by normal return, exceptions, continuation jumps or prompt aborts.

This also installs a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{continuation barrier}
by @racket[call-with-continuation-barrier],
to prevent continuation jumps back into @racket[proc],
so @racket[cleanup] doesn't get called more than once.
}

@defform[(let*-with-cleanup ([name cleanup value] ...) body ...)]{
Like @racket[let*], but calling each @racket[cleanup] on its respective @racket[value] on exit,
in reverse order.

@racket[call-with-cleanup] is called for each binding, ensuring that each
@racket[cleanup] procedure is called at most once, and that it is called
even if a following @racket[value] expression throws an exception or escapes otherwise.
Notably this also introduces a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{continuation barrier}
for each binding.
}

@defthing[raylib-ffi-lib ffi-lib?]{
The Racket
@tech[#:doc '(lib "scribblings/foreign/foreign.scrbl")]{foreign-library value}
for Raylib.

Use this if you want to redefine any functions to better suit your application.
The alternative is to use @racket[(ffi-lib #f)] after requiring this module.
}
