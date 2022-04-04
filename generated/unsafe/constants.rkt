#lang racket/base

(require "structs.rkt")

(provide (all-defined-out))

(define RAYLIB_VERSION "4.1-dev")

;; Light Gray
(define LIGHTGRAY (make-Color 200 200 200 255))

;; Gray
(define GRAY (make-Color 130 130 130 255))

;; Dark Gray
(define DARKGRAY (make-Color 80 80 80 255))

;; Yellow
(define YELLOW (make-Color 253 249 0 255))

;; Gold
(define GOLD (make-Color 255 203 0 255))

;; Orange
(define ORANGE (make-Color 255 161 0 255))

;; Pink
(define PINK (make-Color 255 109 194 255))

;; Red
(define RED (make-Color 230 41 55 255))

;; Maroon
(define MAROON (make-Color 190 33 55 255))

;; Green
(define GREEN (make-Color 0 228 48 255))

;; Lime
(define LIME (make-Color 0 158 47 255))

;; Dark Green
(define DARKGREEN (make-Color 0 117 44 255))

;; Sky Blue
(define SKYBLUE (make-Color 102 191 255 255))

;; Blue
(define BLUE (make-Color 0 121 241 255))

;; Dark Blue
(define DARKBLUE (make-Color 0 82 172 255))

;; Purple
(define PURPLE (make-Color 200 122 255 255))

;; Violet
(define VIOLET (make-Color 135 60 190 255))

;; Dark Purple
(define DARKPURPLE (make-Color 112 31 126 255))

;; Beige
(define BEIGE (make-Color 211 176 131 255))

;; Brown
(define BROWN (make-Color 127 106 79 255))

;; Dark Brown
(define DARKBROWN (make-Color 76 63 47 255))

;; White
(define WHITE (make-Color 255 255 255 255))

;; Black
(define BLACK (make-Color 0 0 0 255))

;; Blank (Transparent)
(define BLANK (make-Color 0 0 0 0))

;; Magenta
(define MAGENTA (make-Color 255 0 255 255))

;; My own White (raylib logo)
(define RAYWHITE (make-Color 245 245 245 255))
