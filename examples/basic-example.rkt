#!/usr/bin/env racket
#lang racket

(require raylib/generated/unsafe)

#<<EOF
#include "raylib.h"

int main(void)
{
    InitWindow(800, 450, "raylib [core] example - basic window");

    while (!WindowShouldClose())
    {
        BeginDrawing();
            ClearBackground(RAYWHITE);
            DrawText("Congrats! You created your first window!", 190, 200, 20, LIGHTGRAY);
        EndDrawing();
    }

    CloseWindow();

    return 0;
}
EOF

(InitWindow 800 450 "raylib [core] example - basic window")

(define RAYWHITE {make-Color 245 245 245 255})
(define LIGHTGRAY {make-Color 200 200 200 255})

(let loop ()
  (when (not (WindowShouldClose))
    (BeginDrawing)
    (ClearBackground RAYWHITE)
    (DrawText "Congrats! You created your first window!" 190 200 20 LIGHTGRAY)
    (EndDrawing)
    (loop)))

(CloseWindow)
