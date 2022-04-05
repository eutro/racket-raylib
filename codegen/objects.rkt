#lang racket/base

(require racket/contract/base
         racket/contract/region)

(provide (all-defined-out))

(define-struct/contract api-object
  ([name string?]
   [description (or/c #f string?)])
  #:transparent)

(define/contract current-api-object
  (parameter/c (or/c api-object? #f))
  (make-parameter #f))

(define-struct/contract (api-function api-object)
  ([return-type string?]
   [parameters (listof (cons/c string? string?))]
   [varargs boolean?])
  #:transparent)

;; represents a field, or multiple fields, in an API struct
;; note that any array subscripts are included in the field name
(define-struct/contract api-field
  ([name (listof string?)]
   [description (or/c #f string?)]
   [type string?])
  #:transparent)

;; represents an API struct, parsed from the JSON
(define-struct/contract (api-struct api-object)
  ([fields (listof api-field?)])
  #:transparent)

;; represents a typedef in the API, parsed from the header
(define-struct/contract (api-typedef api-object)
  ([type string?])
  #:transparent)

;; represents a callback typedef in the API, parsed from the header
(define-struct/contract (api-callback-typedef api-object)
  ([return-type string?]
   [params (listof (cons/c string? string?))])
  #:transparent)

(define-struct/contract (api-enum-value api-object)
  ([value integer?])
  #:transparent)

(define-struct/contract (api-enum api-object)
  ([values (listof api-enum-value?)])
  #:transparent)

(define-struct/contract (api-constant api-object)
  ([type string?]
   [value any/c])
  #:transparent)
