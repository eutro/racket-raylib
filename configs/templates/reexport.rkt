#lang scribble/text

@(require racket/match)

@(provide generate-reexport)

@(define (generate-reexport . modules+exclusions)
@list{
#lang racket/base

(require @(block
           (add-newlines
            (for/list ([m+e (in-list modules+exclusions)])
              (match-define (cons import-mod exclusions) m+e)
              (if (null? exclusions)
                  import-mod
                  @list{
                  (except-in @|import-mod|
                             @(block (add-newlines exclusions)))
                  })))))

(provide (all-from-out @(block (add-newlines (map car modules+exclusions)))))
@(void)})

