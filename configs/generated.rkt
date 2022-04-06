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
  #:top-desc @list{
  Unsafe, automatically generated bindings for
  @"@"deftech{@"@"hyperlink["https://www.raylib.com/"]{Raylib}}
  @|raylib-version|.
  }
  #:module @list{raylib/generated/unsafe}
  #:module-desc @list{
  Reexports all of @"@"racket[raylib/generated/unsafe/functions]
  and @"@"racket[raylib/generated/*].
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
