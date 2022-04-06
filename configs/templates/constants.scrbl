#lang scribble/text

@(require racket/match
          racket/format
          raylib/codegen/objects
          "common.rkt")
@(provide generate-constants)

@(define (generate-constants
          constants-parsed
          #:module this-mod
          #:structs-module structs-mod)
@list{
#lang scribble/manual

@"@"(require (for-label @|this-mod| @|structs-mod| racket/base))

@"@"table-of-contents[]

@"@"title{Constants}
@"@"defmodule[@|this-mod|]
@splice{
@(for/list ([parsed-constant (in-list constants-parsed)])
   (match-define (api-constant name description _ _) parsed-constant)
   (define-values (type value) (constant-type-and-value parsed-constant))
   (when value
@list{@(void)
@"@"defthing[@|name| @|type| #:value @|value| @(~s (or description ""))]
@(void)}))}})
