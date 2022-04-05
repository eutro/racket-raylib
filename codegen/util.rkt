#lang racket/base

(require racket/string
         racket/contract
         racket/match)

(provide nonempty-or-false parse-type parse-type*)

(define (nonempty-or-false str)
  (if (non-empty-string? str) str #f))

(define/contract (parse-type name ty-str)
  (-> string? string? (values string? string?))
  (cond
    [(string-suffix? name "]")
     (match-define (list* name* arrays) (string-split name "["))
     (define arrays* (map (λ (s) (string-trim s "]" #:left? #f)) arrays))
     (define-values (name** array-component) (parse-type name* ty-str))
     (values name** (format "(_array ~a ~a)" array-component (string-join arrays* " ")))]
    [(string=? ty-str "const char *") (values name "_string")]
    [(string-suffix? ty-str "*") (values name (format "_pointer #;~s" ty-str))]
    [else
     (values
      name
      (case ty-str
        [("va_list") "_byte #;\"va_list\""]
        [("unsigned int") "_uint"]
        [("char") "_byte"]
        [("unsigned char") "_ubyte"]
        [else (format "_~a" ty-str)]))]))

(define (parse-type* ty-str)
  (define-values (_name type) (parse-type "" ty-str))
  type)
