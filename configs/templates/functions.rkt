#lang scribble/text

@(require racket/match
          raylib/codegen/objects
          "common.rkt")
@(provide generate-functions)

@(define (generate-functions functions-parsed)
@list{
#lang racket/base

(require ffi/unsafe ffi/unsafe/define "structs.rkt" "enums.rkt")

(define-ffi-definer define-raylib (ffi-lib "libraylib")
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
