#lang scribble/text

@(require racket/match
          raylib/codegen/objects
          "common.rkt")
@(provide generate-functions)

@(define (generate-functions
          functions-parsed
          #:module _this-mod
          #:structs-module structs-mod)
@list{
#lang racket/base

(require ffi/unsafe ffi/unsafe/define @|structs-mod| raylib/support)

(define-ffi-definer define-raylib raylib-ffi-lib
  #:provide provide-protected
  #:default-make-fail make-not-available)
@(splice
  (for/list ([parsed-function (in-list functions-parsed)])
    (match-define (api-function name description return-type parameters varargs) parsed-function)
@list{@(void)
@(when description @list{
;; @|description|
})
(define-raylib @|name|
  (_fun
   @block{
   @(add-newlines
     @(list
       (when varargs @list{#:varargs-after @(length parameters)})
       (generate-params parameters)
       (when varargs @list{;; ... varargs
                           })
       @list{-> @(parse-type* return-type)}))}))
@(void)}))})
