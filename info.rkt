#lang info

(define collection "raylib")

(define compile-omit-paths
  '("codegen"
    "configs"
    "raylib-parser"
    "raylib-git"
    "working"
    "raymath/private"
    "x.rkt"))

(define test-omit-paths
  '("raymath/private"))

(define deps
  '("base"))

(define build-deps
  '("scribble-lib"
    "racket-doc"
    "gui-doc"
    "gui-lib"
    "net-doc"))

(define scribblings
  '(("scribblings/raylib.scrbl" (multi-page))))

(define pkg-desc "Semi-automatically generated Raylib bindings.")

(define version
  ;; Raylib version . package sub-version
  "4.0.0.4")

(define pkg-authors '(eutro))

(define license '(Apache-2.0 OR MIT))
