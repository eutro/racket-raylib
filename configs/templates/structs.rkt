#lang scribble/text

@(require racket/match
          raylib/codegen/objects
          raylib/codegen/util
          "common.rkt")
@(provide generate-structs)

@(define (generate-structs
          structs-parsed
          typedefs-parsed
          function-typedefs-parsed
          #:module _this-mod)
@list{
#lang racket/base

(require ffi/unsafe)

(provide (all-defined-out))
@(splice
  (for/list ([parsed-struct (in-list structs-parsed)])
    (list

     (match-let ([(api-struct name description fields) parsed-struct])
       @list{@(void)
@(when description @list{
;; @|description|
})
(define-cstruct _@|name|
  (@(block
     (add-newlines
      (for/list ([struct-field (in-list fields)])
        (match-define (api-field names field-desc field-type) struct-field)
        @(add-newlines
          #:sep " "
          (list
           (add-newlines
            #:sep " "
            (for/list ([name names])
              (define-values (name* type) (parse-type name field-type))
              @list{[@|name*| @|type|]}))
           (when field-desc @list{
              ; @|field-desc|
           }))))))
   @; (deliberate)
   ))
@(void)})

     (for/list
         ([parsed-typedef (in-list typedefs-parsed)]
          #:when
          (string=? (api-object-name parsed-struct)
                    (api-typedef-type parsed-typedef)))
       (define-values (name type)
         (parse-type (api-object-name parsed-typedef)
                     (api-typedef-type parsed-typedef)))
       @list{@(void)
             (define _@|name| @|type|)
             @(void)})))

  (for/list ([fn-typedef (in-list function-typedefs-parsed)])
    (match-define (api-callback-typedef name _desc ret-type params) fn-typedef)
@list{@(void)
(define _@|name|
  (_fun
   @(generate-params params)
   -> @(parse-type* ret-type)))
@(void)}))})
