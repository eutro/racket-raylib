#lang racket/base

(require racket/string)

(provide nonempty-or-false)

(define (nonempty-or-false str)
  (if (non-empty-string? str) str #f))
