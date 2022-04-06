#lang at-exp raylib/codegen/config

(define (all-modules fmt)
  (for/list ([path
              (in-list
               '("unsafe/functions"
                 "unsafe/structs"
                 "unsafe/enums"
                 "unsafe/constants"))])
    (format fmt path)))

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
  #:top-desc @list{Unsafe, automatically generated, bindings for Raylib.}
  #:module @list{raylib/generated/unsafe}
  #:module-desc @list{Reexports all of @"@"racket[raylib/generated/unsafe/*].}
  (all-modules "~a.scrbl")))

(for ([out-fmt (in-list '("~a.rkt" "scribblings/~a.scrbl"))]
      [in-fmt (in-list '("~a.rkt" "~a.scrbl"))])
  (output
   #:to (format out-fmt "unsafe/structs")
   #:from
   ((template (format in-fmt "./templates/structs") 'generate-structs)
    (parsed 'structs)
    (parsed 'typedefs)
    (parsed 'function-typedefs)))
  (output
   #:to (format out-fmt "unsafe/functions")
   #:from
   ((template (format in-fmt "./templates/functions") 'generate-functions)
    (parsed 'functions)))
  (output
   #:to (format out-fmt "unsafe/constants")
   #:from
   ((template (format in-fmt "./templates/constants") 'generate-constants)
    (parsed 'constants)))
  (output
   #:to (format out-fmt "unsafe/enums")
   #:from
   ((template (format in-fmt "./templates/enums") 'generate-enums)
    (parsed 'enums))))
