#lang scribble/text

@(require racket/format)

@(provide generate-root)

@(define (generate-root
          #:top-desc desc
          #:module mod
          #:module-desc mod-desc
          . modules) @list{
#lang scribble/manual

@"@"title{Generated Raylib Bindings}
@|desc|

@"@"table-of-contents[]

@"@"defmodule[@|mod|]
@|mod-desc|
@(splice
  (for/list ([mod (in-list modules)])
    @list{@(void)
@"@"include-section[@(~s mod)]}))
@(void)})
