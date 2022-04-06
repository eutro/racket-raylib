#lang scribble/text
@(provide generate-root)

@(define (generate-root) @list{
#lang racket/base

(require "unsafe/functions.rkt"
         "unsafe/structs.rkt"
         "unsafe/enums.rkt"
         "unsafe/constants.rkt")

(provide (all-from-out "unsafe/functions.rkt"
                       "unsafe/structs.rkt"
                       "unsafe/enums.rkt"
                       "unsafe/constants.rkt"))
@(void)})
