#lang scribble/text

@(require racket/match
          raylib/codegen/objects
          "common.rkt")
@(provide generate-functions)

@(define (generate-functions
          functions-parsed
          #:module this-mod
          #:structs-module structs-mod)
@list{
#lang scribble/manual

@"@"(require (for-label @|this-mod| @|structs-mod| raylib/support ffi/unsafe racket/base))

@"@"title{Functions}

@"@"defmodule[@|this-mod|]

@"@"table-of-contents[]

@(splice
  (for/list ([parsed-function (in-list functions-parsed)])
    (match-define (api-function name description return-type parameters _varargs) parsed-function)
@list{@(void)
@"@"defproc[@block{(@(block (add-newlines @list[
             @|name|
             @(unless (null? parameters)
                (add-newlines
                 (for/list ([param (in-list parameters)])
                   (match-define (cons pname ptype) param)
                   (define-values (pname* ptype*) (parse-type pname ptype))
                   @list{[@|pname*| @|ptype*|]})))])))
            @(parse-type* return-type)}]{
@|description|
}
@(void)}))})
