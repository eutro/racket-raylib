#lang scribble/manual

@(require (for-label raylib/generated/constants raylib/generated/structs racket/base))

@title{Constants}

@table-of-contents[]

@defmodule[raylib/generated/constants]

@defthing[RAYLIB_VERSION string? #:value "4.0" ""]

@defthing[LIGHTGRAY Color? #:value (make-Color 200 200 200 255) "Light Gray"]

@defthing[GRAY Color? #:value (make-Color 130 130 130 255) "Gray"]

@defthing[DARKGRAY Color? #:value (make-Color 80 80 80 255) "Dark Gray"]

@defthing[YELLOW Color? #:value (make-Color 253 249 0 255) "Yellow"]

@defthing[GOLD Color? #:value (make-Color 255 203 0 255) "Gold"]

@defthing[ORANGE Color? #:value (make-Color 255 161 0 255) "Orange"]

@defthing[PINK Color? #:value (make-Color 255 109 194 255) "Pink"]

@defthing[RED Color? #:value (make-Color 230 41 55 255) "Red"]

@defthing[MAROON Color? #:value (make-Color 190 33 55 255) "Maroon"]

@defthing[GREEN Color? #:value (make-Color 0 228 48 255) "Green"]

@defthing[LIME Color? #:value (make-Color 0 158 47 255) "Lime"]

@defthing[DARKGREEN Color? #:value (make-Color 0 117 44 255) "Dark Green"]

@defthing[SKYBLUE Color? #:value (make-Color 102 191 255 255) "Sky Blue"]

@defthing[BLUE Color? #:value (make-Color 0 121 241 255) "Blue"]

@defthing[DARKBLUE Color? #:value (make-Color 0 82 172 255) "Dark Blue"]

@defthing[PURPLE Color? #:value (make-Color 200 122 255 255) "Purple"]

@defthing[VIOLET Color? #:value (make-Color 135 60 190 255) "Violet"]

@defthing[DARKPURPLE Color? #:value (make-Color 112 31 126 255) "Dark Purple"]

@defthing[BEIGE Color? #:value (make-Color 211 176 131 255) "Beige"]

@defthing[BROWN Color? #:value (make-Color 127 106 79 255) "Brown"]

@defthing[DARKBROWN Color? #:value (make-Color 76 63 47 255) "Dark Brown"]

@defthing[WHITE Color? #:value (make-Color 255 255 255 255) "White"]

@defthing[BLACK Color? #:value (make-Color 0 0 0 255) "Black"]

@defthing[BLANK Color? #:value (make-Color 0 0 0 0) "Blank (Transparent)"]

@defthing[MAGENTA Color? #:value (make-Color 255 0 255 255) "Magenta"]

@defthing[RAYWHITE Color? #:value (make-Color 245 245 245 255) "My own White (raylib logo)"]
