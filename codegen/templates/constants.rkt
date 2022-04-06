#lang scribble/text

@(require racket/match
          "../objects.rkt"
          "common.rkt")
@(provide generate-constants)

@(define (generate-constants constants-parsed)
@list{
#lang racket/base

(require "structs.rkt")

(provide (all-defined-out))@;
@splice{
@(for/list ([parsed-constant (in-list constants-parsed)])
   (match-define (api-constant name description _ _) parsed-constant)
   (define-values (_type value) (constant-type-and-value parsed-constant))
   (when value
@list{@(void)
@(when description @list{
;; @|description|
})
(define @|name| @|value|)
@(void)}))}})
