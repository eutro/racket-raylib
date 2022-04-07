#lang info
(define collection "raylib")
(define deps '("base"))
(define build-deps
  '("scribble-lib"
    "racket-doc"
    "at-exp-lib"
    "gui-doc"
    "gui-lib"
    "net-doc"))
(define scribblings
  '(("generated/scribblings/raylib-generated.scrbl" (multi-page))
    ("2d/scribblings/raylib-2d.scrbl" (multi-page))))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(eutro))
(define license '(Apache-2.0 OR MIT))
