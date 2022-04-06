#lang scribble/text

@(require racket/match
          racket/format
          raylib/codegen/objects)
@(provide generate-enums)

@(define (generate-enums enums-parsed)
@list{
#lang scribble/manual

@"@"(require (for-label raylib/generated/unsafe/enums ffi/unsafe racket/base))

@"@"table-of-contents[]

@"@"title{Enums}
@"@"defmodule[raylib/generated/unsafe/enums]
@(splice
  (for/list ([parsed-enum (in-list enums-parsed)])
    (match-define (api-enum name description enum-values) parsed-enum)
@list{@(void)
@"@"section{@|description|}
@"@"defthing[_@|name| ctype?]{@|description|}@;
@(for/list ([enum-value (in-list enum-values)])
   (match-define (api-enum-value name enum-desc enum-int-value) enum-value)
@list{@(void)
@"@"defthing[@|name| exact-integer? #:value @|enum-int-value| @(~s enum-desc)]})
@(void)}))
})
