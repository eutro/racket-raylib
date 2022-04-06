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
 #:to "scribblings/raylib-2d.scrbl"
 #:from
 (apply
  (template "./templates/root.scrbl" 'generate-root)
  #:title @list{Raylib 2D Bindings}
  #:top-desc @list{
  Unsafe bindings for @"@"deftech{@"@"hyperlink["https://www.raylib.com/"]{Raylib}}'s
  2D components.

  These bindings are currently for Raylib @|raylib-version|.

  Most of these bindings are perfectly safe, as long as they are not horribly misused.
  They are called and marked "unsafe", since they are a thin wrapper over a C API,
  and thus undefined behaviour is possible if the bindings are used incorrectly.
  }
  #:module @list{raylib/2d/unsafe}
  #:module-desc @list{
  Reexports all of @"@"racket[raylib/2d/unsafe/functions] and @"@"racket[raylib/2d/*].
  }
  (all-modules "~a.scrbl")))

(define structs-module
  @list{raylib/2d/structs})

(for ([out-fmt (in-list '("~a.rkt" "scribblings/~a.scrbl"))]
      [in-fmt (in-list '("~a.rkt" "~a.scrbl"))])
  (output
   #:to (format out-fmt "unsafe/functions")
   #:from
   ((template (format in-fmt "./templates/functions") 'generate-functions)
    #:module @list{raylib/2d/unsafe/functions}
    #:structs-module structs-module
    (parsed 'functions)))

  ;; these are all perfectly safe
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
    #:module @list{raylib/2d/constants}
    #:structs-module structs-module
    (parsed 'constants)))
  (output
   #:to (format out-fmt "enums")
   #:from
   ((template (format in-fmt "./templates/enums") 'generate-enums)
    #:module @list{raylib/2d/enums}
    (parsed 'enums))))

