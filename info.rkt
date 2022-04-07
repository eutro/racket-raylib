#lang info

(define collection "raylib")

(define compile-omit-paths
  '("codegen"
    "configs"
    "raylib-parser"))

(define deps
  '("base"))

(define build-deps
  '("scribble-lib"
    "racket-doc"
    "gui-doc"
    "gui-lib"
    "net-doc"))

(define scribblings
  '(("generated/scribblings/raylib-generated.scrbl" (multi-page))
    ("2d/scribblings/raylib-2d.scrbl" (multi-page))))

(define pkg-desc "Semi-automatically generated Raylib bindings.")

(define version "4.0.0")

(define pkg-authors '(eutro))

(define license '(Apache-2.0 OR MIT))
