#lang scribble/text

@(require racket/match
          racket/format
          racket/contract
          "../util.rkt"
          "../objects.rkt")
@(provide (all-defined-out))

@(define (constant-type-and-value api-constant)
   (define raw-value (api-constant-value api-constant))
   (case (api-constant-type api-constant)
     [("STRING") (values "string?" (~s raw-value))]
     [("COLOR")
      (values
       "Color?"
       (~a
        (cons
         "make-Color"
         (cdr
          (regexp-match
           #px"CLITERAL\\(Color\\)\\{ (\\d+), (\\d+), (\\d+), (\\d+) \\}"
           (~a raw-value))))))]
     [else (values #f #f)]))

@(define/contract (parse-type name ty-str)
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

@(define (parse-type* ty-str)
   (define-values (_name type) (parse-type "" ty-str))
   type)

@(define (generate-params parameters)
   (block
    (add-newlines
     (for/list ([param (in-list parameters)])
       (match-define (cons pname ptype) param)
       (define-values (pname* ptype*) (parse-type pname ptype))
       @list{[@|pname*| : @|ptype*|]}))))
