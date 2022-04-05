#lang scribble/text

@(require racket/match

          "../objects.rkt"
          "../util.rkt"
          "common.rkt")
@(provide generate-structs)

@(define (generate-structs structs-parsed typedefs-parsed function-typedefs-parsed)

@list{
#lang scribble/manual

@"@"(require (for-label raylib/generated/unsafe/structs ffi/unsafe racket/base))

@"@"table-of-contents[]

@"@"title{Structs}
@"@"defmodule[raylib/generated/unsafe/structs]
@"@"section{Struct types}
@(for/list ([struct-parsed (in-list structs-parsed)])
   (match-define (api-struct name desc fields) struct-parsed)
@list{
@(void)
@"@"deftogether[(@block{
                 @"@"defthing[_@|name| ctype?]
                 @"@"defstruct[@block{
                               @|name|
                               (@block{
@(add-newlines
  (for/list ([struct-field (in-list fields)])
    (match-define (api-field names _field-desc field-type) struct-field)
    (add-newlines
     #:sep " "
     (for/list ([name (in-list names)])
       (define-values (name* type) (parse-type name field-type))
       @list{[@|name*| @|type|]}))))})
                               #:constructor-name make-@|name|}]})]{
@|desc|
}
@(void)})
@"@"section{Type aliases}
@"@"deftogether[(@block{
                 @(add-newlines
                   (for/list ([typedef-parsed (in-list typedefs-parsed)])
                     (match-define (api-typedef name _desc type) typedef-parsed)
                     @list{@"@"defthing[_@|name| ctype? #:value _@|type|]}))})]{
Aliases for some struct types.
}

@"@"section{Callback function types}
@"@"deftogether[(@block{
@(add-newlines
  (for/list ([function-parsed (in-list function-typedefs-parsed)])
    (match-define (api-callback-typedef name _desc ret-type params)
      function-parsed)
    @list{
@"@"defthing[@block{
             _@|name| ctype?
             #:value
             (_fun
              @(generate-params params)
              -> @(parse-type* ret-type))}]}))})]{
Types for certain callback functions.
}
@(void)})
