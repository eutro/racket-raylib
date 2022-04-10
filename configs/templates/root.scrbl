#lang scribble/text

@(require racket/format)

@(provide generate-root)

@(define (generate-root
          #:title title
          #:module mod
          #:desc mod-desc
          . modules) @list{
#lang scribble/manual

@"@"title{@|title|}

@"@"defmodule[@|mod|]
@|mod-desc|

@"@"table-of-contents[]

@(splice
  (for/list ([mod (in-list modules)])
    @list{@(void)
@"@"include-section[@(~s mod)]}))
@(void)})
