#lang scribble/text

@(require racket/match
          racket/format
          racket/contract

          raylib/codegen/util
          raylib/codegen/objects)
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
     [(string-prefix? ty-str "const ")
      (parse-type name (substring ty-str (string-length "const ")))]
     [(string-suffix? ty-str "*")
      (define pointee-str (string-trim (substring ty-str 0 (sub1 (string-length ty-str)))))
      (values name (format "(_pointer-to ~a)" (parse-type* pointee-str)))]
     [else
      (values
       name
       (case ty-str
         [("va_list") "_byte #;\"va_list\""]
         [("char") "_byte"]
         [("bool") "_stdbool"]
         [("unsigned char") "_ubyte"]
         [("unsigned short") "_ushort"]
         [("unsigned int") "_uint"]
         [else (format "_~a" ty-str)]))]))

@(define (parse-type* ty-str)
   (define-values (_name type) (parse-type "" ty-str))
   type)

@(define (generate-params parameters)
   (unless (null? parameters)
     (block
      (add-newlines
       (for/list ([param (in-list parameters)])
         (match-define (cons pname ptype) param)
         (define-values (pname* ptype*) (parse-type pname ptype))
         @list{[@|pname*| : @|ptype*|]})))))
