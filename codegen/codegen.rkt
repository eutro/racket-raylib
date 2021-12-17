#!/usr/bin/env racket
#lang racket

(require json
         xml
         xml/path
         net/url
         syntax/datum)

(define (generate-bindings-in [dir (current-directory)])
  (define ports
    (append*
     (for/list ([name (in-list '("structs" "enums" "functions"))])
       (list
        (open-output-file (build-path dir (format "~a.rkt" name)) #:exists 'replace)
        (open-output-file (build-path dir "docs" (format "~a.scrbl" name)) #:exists 'replace)))))
  (dynamic-wind
    void
    (thunk (apply generate-bindings ports))
    (thunk (for-each close-output-port ports))))

(module+ main
  (generate-bindings-in))

(define/contract (generate-bindings structs-rkt-port structs-doc-port
                                    enums-rkt-port enums-doc-port
                                    functions-rkt-port functions-doc-port)
  (-> (or/c #f output-port?) (or/c #f output-port?)
      (or/c #f output-port?) (or/c #f output-port?)
      (or/c #f output-port?) (or/c #f output-port?)
      void?)
  (define api-json ((compose1 read-json
                              open-input-string
                              fetch-api-url)
                    "parser/raylib_api.json"))
  (define api-header (fetch-api-url "src/raylib.h"))
  (define api-xexpr ((compose1 xml->xexpr
                               document-element
                               read-xml
                               open-input-string
                               fetch-api-url)
                     "parser/raylib_api.xml"))
  (when (or structs-rkt-port structs-doc-port)
    (define structs-parsed (map parse-struct (hash-ref api-json 'structs)))
    (define typedefs-parsed (parse-typedefs api-header))
    (define function-typedefs-parsed (parse-function-typedefs api-header))
    (when structs-rkt-port
      (write-struct-bindings
       structs-rkt-port structs-parsed typedefs-parsed function-typedefs-parsed))
    (when structs-doc-port
      (write-struct-docs
       structs-doc-port structs-parsed typedefs-parsed function-typedefs-parsed)))
  (when (or enums-rkt-port enums-doc-port)
    (define enums-parsed (map parse-enum (hash-ref api-json 'enums)))
    (when enums-rkt-port
      (write-enum-bindings enums-rkt-port enums-parsed))
    (when enums-doc-port
      (write-enum-docs enums-doc-port enums-parsed)))
  (when (or functions-rkt-port functions-doc-port)
    (define functions-parsed
      (map parse-function (filter pair? (se-path*/list '(Functions) api-xexpr))))
    (when functions-rkt-port
      (write-function-bindings functions-rkt-port functions-parsed)))
  (void))

(define raylib-raw-root
  (make-parameter (or (getenv "RAYLIB_RAW_ROOT")
                      "https://raw.githubusercontent.com/raysan5/raylib/master")))

(define (fetch-api-url path)
  (define res-url (format "~a/~a" (raylib-raw-root) path))
  (port->string (get-pure-port (string->url res-url))))

;;; Utilities

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

;;; Functions

(define-struct/contract api-function
  ([name string?]
   [description (or/c #f string?)]
   [return-type string?]
   [parameters (listof (cons/c string? string?))]
   [varargs boolean?])
  #:transparent)

(define (parse-function function-xexpr)
  (datum-case
   (filter pair? function-xexpr) (Param desc name paramCount retType type)
   [(((desc fdesc) (name fname) (paramCount _) (retType return-type))
     (Param ((desc _) (name pname) (type ptype))) ...)
    (let ()
      (define params (datum ((pname . ptype) ...)))
      (define-values (params* varargs)
        (if (and (pair? params) (equal? '("" . "") (last params)))
            (values (drop-right params 1) #t)
            (values params #f)))
      (make-api-function
       (datum fname)
       (datum fdesc)
       (datum return-type)
       params*
       varargs))]))

(define (write-params parameters [indent 3])
  (define indent-str (build-string indent (const #\space)))
  (for ([param parameters])
    (match-define (cons pname ptype) param)
    (define-values (pname* ptype*) (parse-type pname ptype))
    (display indent-str)
    (printf "[~a : ~a]\n" pname* ptype*)))

(define (write-return-type return-type [indent 3])
  (define-values (_n parsed-return) (parse-type "" return-type))
  (display (build-string indent (const #\space)))
  (printf "-> ~a)" parsed-return))

(define (api-function->binding parsed)
  (match-define (api-function name description return-type parameters varargs) parsed)
  (when description
    (printf ";; ~a\n" description))
  (printf "(define-raylib ~a\n  (_fun\n" name)
  (when varargs
    (printf "   #:varargs-after ~a\n" (length parameters)))
  (write-params parameters)
  (when varargs
    (displayln "   ;; ... varargs\n"))
  (write-return-type return-type)
  (display ")\n"))

(define (write-function-bindings port functions-parsed)
  (parameterize ([current-output-port port])
    (display "#lang racket/base\n\n")
    (display "(require ffi/unsafe ffi/unsafe/define \"structs.rkt\" \"enums.rkt\")\n\n")
    (display "(define-ffi-definer define-raylib (ffi-lib \"libraylib\")\n")
    (display "  #:provide provide-protected\n")
    (display "  #:default-make-fail make-not-available)\n")
    (for ([api-function functions-parsed])
      (newline)
      (api-function->binding api-function))))

;;; Structs

;; represents a field, or multiple fields, in an API struct
;; note that any array subscripts are included in the field name
(define-struct/contract api-field
  ([names (listof string?)]
   [description (or/c #f string?)]
   [type string?])
  #:transparent)

;; represents an API struct, parsed from the JSON
(define-struct/contract api-struct
  ([name string?]
   [description (or/c #f string?)]
   [fields (listof api-field?)])
  #:transparent)

;; represents a typedef in the API, parsed from the header
(define-struct/contract api-typedef
  ([name string?]
   [type string?])
  #:transparent)

;; represents a callback typedef in the API, parsed from the header
(define-struct/contract api-callback-typedef
  ([name string?]
   [return-type string?]
   [params (listof (cons/c string? string?))])
  #:transparent)

(define (parse-typedefs header)
  (define typedef-lines
    (regexp-match* #px"\ntypedef ([^ ]+) ([^ ]+);" header
                   #:match-select values))
  (for/list ([typedef typedef-lines])
    (match-define (list _ aliased-type alias-name) typedef)
    (make-api-typedef alias-name aliased-type)))

(define (api-typedef->binding parsed)
  (define-values (name type) (parse-type (api-typedef-name parsed) (api-typedef-type parsed)))
  (printf "(define _~a ~a)\n" name type))

(define (parse-function-typedefs header)
  (define typedef-lines
    (filter-map (curry regexp-match #px"typedef (.+)\\(\\*([^\\)]+)\\)\\((.+)\\);") (string-split header "\n")))
  (for/list ([typedef typedef-lines])
    (match-define (list _ ret-type name params) typedef)
    (define param-list (string-split params ", "))
    (define (split-param param)
      (match-define (list _ type name)
        (regexp-match #px"^(.+? \\*?)(\\w+)$" param))
      (cons name (string-trim type)))
    (make-api-callback-typedef
     name
     (string-trim ret-type)
     (map split-param param-list))))

(define (api-function-typedef->binding parsed)
  (match-define (api-callback-typedef name ret-type params) parsed)
  (printf "(define _~a\n  (_fun\n" name)
  (write-params params)
  (write-return-type ret-type)
  (display ")\n"))

(define (parse-struct struct-json)
  (define (parse-struct-field field-json)
    (make-api-field
     (string-split (hash-ref field-json 'name) ", ")
     (nonempty-or-false (hash-ref field-json 'description))
     (hash-ref field-json 'type)))
  (make-api-struct
   (hash-ref struct-json 'name)
   (nonempty-or-false (hash-ref struct-json 'description))
   (map parse-struct-field (hash-ref struct-json 'fields))))

(define (api-struct->binding parsed)
  (match-define (api-struct name description fields) parsed)
  (when description
    (printf ";; ~a\n" description))
  (printf "(define-cstruct _~a\n  (" name)
  (for ([struct-field fields])
    (match-define (api-field names field-desc field-type) struct-field)
    (define field-strings
      (string-join
       (for/list ([name names])
         (define-values (name* type) (parse-type name field-type))
         (format "[~a ~a]" name* type))
       " "))
    (display field-strings)
    (when field-desc
      (printf " ; ~a" field-desc))
    (printf "\n   "))
  (display "))\n"))

(define (write-struct-bindings port
                               structs-parsed
                               typedefs-parsed
                               function-typedefs-parsed)
  (parameterize ([current-output-port port])
    (display "#lang racket/base\n\n(require ffi/unsafe)\n\n(provide (all-defined-out))\n")
    (for ([api-struct structs-parsed])
      (newline)
      (api-struct->binding api-struct)
      (for ([typedef
             (filter (λ (td)
                       (string=? (api-struct-name api-struct)
                                 (api-typedef-type td)))
                     typedefs-parsed)])
        (newline)
        (api-typedef->binding typedef)))
    (for ([fn-typedef function-typedefs-parsed])
      (newline)
      (api-function-typedef->binding fn-typedef))))

(define (api-struct->docs struct-parsed)
  (match-define (api-struct name desc fields) struct-parsed)
  (printf "@deftogether[(@defthing[_~a ctype?]\n" name)
  (printf "              @defstruct[~a\n" name)
  (printf "                         (~a)\n"
          (string-join
           (for/list ([struct-field fields])
             (match-define (api-field names _field-desc field-type) struct-field)
             (string-join
              (for/list ([name names])
                (define-values (name* type) (parse-type name field-type))
                (format "[~a ~a]" name* type))
              " "))
           "\n                          "))
  (printf "                         #:constructor-name make-~a])]" name)
  (printf "{\n~a\n}\n" (or desc "")))

(define (api-typedefs->docs typedefs-parsed)
  (newline)
  (display "@deftogether")
  (printf "[(~a)]{\nType aliases.\n}\n"
          (string-join
           (for/list ([typedef-parsed typedefs-parsed])
             (match-define (api-typedef name type) typedef-parsed)
             (format "@defthing[_~a ctype? #:value _~a]" name type))
           "\n              ")))

(define (api-function-typedefs->docs functions-parsed)
  (newline)
  (display "@deftogether")
  (printf
   "[(~a)]{\nCallback function types.\n}\n"
   (string-join
    (for/list ([function-parsed functions-parsed])
      (match-define (api-callback-typedef name ret-type params) function-parsed)
      (string-append
       (format "@defthing[_~a ctype?\n" name)
       (format "                        #:value\n")
       (format "                        (_fun\n")
       (with-output-to-string
         (thunk
          (write-params params 25)
          (write-return-type ret-type 25)))
       "]"))
    "\n              ")))

(define (write-struct-docs port
                           structs-parsed
                           typedefs-parsed
                           function-typedefs-parsed)
  (parameterize ([current-output-port port])
    (display "#lang scribble/manual\n\n")
    (display "@(require (for-label raylib/sys/structs ffi/unsafe racket/base))\n\n")
    (display "@title{Structs}\n")
    (display "@defmodule[raylib/sys/structs]\n")
    (for ([api-struct structs-parsed])
      (newline)
      (api-struct->docs api-struct))
    (api-typedefs->docs typedefs-parsed)
    (api-function-typedefs->docs function-typedefs-parsed)))

;;; Enums

(define-struct/contract api-enum-value
  ([name string?]
   [description (or/c #f string?)]
   [value integer?])
  #:transparent)

(define-struct/contract api-enum
  ([name string?]
   [description (or/c #f string?)]
   [values (listof api-enum-value?)])
  #:transparent)

(define (parse-enum enum-json)
  (define (parse-enum-value value-json)
    (make-api-enum-value
     (hash-ref value-json 'name)
     (nonempty-or-false (hash-ref value-json 'description))
     (hash-ref value-json 'value)))
  (make-api-enum
   (hash-ref enum-json 'name)
   (nonempty-or-false (hash-ref enum-json 'description))
   (map parse-enum-value (hash-ref enum-json 'values))))

(define (api-enum->binding parsed)
  (match-define (api-enum name description enum-values) parsed)
  (when description
    (printf ";; ~a\n" description))
  (printf "(define _~a\n  (_enum '(" name)
  (for ([enum-value enum-values])
    (match-define (api-enum-value name enum-desc enum-int-value) enum-value)
    (printf "~a = ~a" name enum-int-value)
    (printf "\n           "))
  (display ")))")
  (for ([enum-value enum-values])
    (match-define (api-enum-value name enum-desc enum-int-value) enum-value)
    (newline)
    (printf "(define ~a ~a)" name enum-int-value)
    (when enum-desc
      (printf " ; ~a" enum-desc)))
  (newline))

(define (write-enum-bindings port enums-parsed)
  (parameterize ([current-output-port port])
    (display "#lang racket/base\n\n(require ffi/unsafe)\n\n(provide (all-defined-out))\n")
    (for ([api-enum enums-parsed])
      (newline)
      (api-enum->binding api-enum))))

(define (api-enum->docs parsed)
  (match-define (api-enum name description enum-values) parsed)
  (printf "@section{~a}\n" description)
  (printf "@defthing[_~a ctype?]{~a}" name description)
  (for ([enum-value enum-values])
    (match-define (api-enum-value name enum-desc enum-int-value) enum-value)
    (newline)
    (printf "@defthing[~a exact-integer? #:value ~a ~s]" name enum-int-value enum-desc))
  (newline))

(define (write-enum-docs port enums-parsed)
  (parameterize ([current-output-port port])
    (display "#lang scribble/manual\n\n")
    (display "@(require (for-label raylib/sys/enums ffi/unsafe racket/base))\n\n")
    (display "@title{Enums}\n")
    (display "@defmodule[raylib/sys/enums]\n")
    (for ([api-enum enums-parsed])
      (newline)
      (api-enum->docs api-enum))))
