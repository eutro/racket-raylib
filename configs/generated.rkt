#lang at-exp raylib/codegen/config

(define (all-modules fmt)
  (for/list ([path
              (in-list
               '("unsafe/functions"
                 "structs"
                 "enums"
                 "constants"))])
    (format fmt path)))

(define raylib-version
  (api-constant-value
   (car
    (include
     (name-matches "RAYLIB_VERSION")
     #:from (parsed 'constants)))))

(output
 #:to "version.rkt"
 #:from @list{
 #lang racket/base

 (provide RAYLIB_VERSION)

 (define RAYLIB_VERSION @|raylib-version|)
 @(void)})

(output
 #:to "unsafe.rkt"
 #:from
 (apply (template "./templates/root.rkt" 'generate-root)
        (all-modules "~a.rkt")))

(output
 #:to "scribblings/raylib-generated.scrbl"
 #:from
 (apply
  (template "./templates/root.scrbl" 'generate-root)
  #:title @list{Generated Raylib Bindings}
  #:module @list{raylib/generated/unsafe}
  #:desc @list{
  Unsafe, automatically generated bindings for
  @"@"hyperlink["https://www.raylib.com/"]{Raylib}
  @|raylib-version|.

  This module re-exports all of
  @"@"racketmodname[raylib/generated/unsafe/functions],
  @"@"racketmodname[raylib/generated/structs],
  @"@"racketmodname[raylib/generated/enums] and
  @"@"racketmodname[raylib/generated/constants].
  }
  (all-modules "~a.scrbl")))

(define structs-module
  @list{raylib/generated/structs})

(for ([out-fmt (in-list '("~a.rkt" "scribblings/~a.scrbl"))]
      [in-fmt (in-list '("~a.rkt" "~a.scrbl"))])
  (output
   #:to (format out-fmt "unsafe/functions")
   #:from
   ((template (format in-fmt "./templates/functions") 'generate-functions)
    #:module @list{raylib/generated/unsafe/functions}
    #:structs-module structs-module
    (parsed 'functions)))

  (output
   #:to (format out-fmt "structs")
   #:from
   ((template (format in-fmt "./templates/structs") 'generate-structs)
    #:module structs-module
    (parsed 'structs)
    (parsed 'typedefs)
    (parsed 'function-typedefs)))
  (output
   #:to (format out-fmt "constants")
   #:from
   ((template (format in-fmt "./templates/constants") 'generate-constants)
    #:module @list{raylib/generated/constants}
    #:structs-module structs-module
    (parsed 'constants)))
  (output
   #:to (format out-fmt "enums")
   #:from
   ((template (format in-fmt "./templates/enums") 'generate-enums)
    #:module @list{raylib/generated/enums}
    (parsed 'enums))))
