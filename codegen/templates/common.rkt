#lang scribble/text

@(require racket/match "../util.rkt")
@(provide (all-defined-out))

@(define (generate-params parameters)
   (block
    (add-newlines
     (for/list ([param (in-list parameters)])
       (match-define (cons pname ptype) param)
       (define-values (pname* ptype*) (parse-type pname ptype))
       @list{[@|pname*| : @|ptype*|]}))))
