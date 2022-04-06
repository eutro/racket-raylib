#lang scribble/text

@(require racket/format)

@(provide generate-root)

@(define (generate-root . modules) 
   (define module-output (block (add-newlines (map ~s modules))))
@list{
#lang racket/base

(require @|module-output|)

(provide (all-from-out @|module-output|))
@(void)})
