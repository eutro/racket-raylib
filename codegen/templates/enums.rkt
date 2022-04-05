#lang scribble/text

@(require racket/match
          "../objects.rkt")
@(provide generate-enums)

@(define (generate-enums enums-parsed)
@list{
#lang racket/base

(require ffi/unsafe)

(provide (all-defined-out))
@splice{
@(for/list ([parsed-enum (in-list enums-parsed)])
   (match-define (api-enum name description enum-values) parsed-enum)
@list{@(void)
@(when description @list{
;; @|description|
})
(define _@|name|
  (_enum '(@block{
@(add-newlines
  (for/list ([enum-value (in-list enum-values)])
    (match-define (api-enum-value name _enum-desc enum-int-value) enum-value)
    @list{@|name| = @|enum-int-value|}))}
           @; (intentional)
           )))
@(add-newlines
  (for/list ([enum-value (in-list enum-values)])
    (match-define (api-enum-value name enum-desc enum-int-value) enum-value)
    (add-newlines
     #:sep " "
     (list
      @list{(define @|name| @|enum-int-value|)}
      (when enum-desc @list{
        ; @|enum-desc|
      })))))
@(void)})
}})
