#!/usr/bin/env racket
#lang racket/base

;; https://github.com/raysan5/raylib/blob/master/examples/core/core_input_keys.c

(module+ main
  (require raylib/2d/unsafe)

  (define screen-width 800)
  (define screen-height 450)

  (InitWindow 800 450 "raylib [core] example - keyboard input")

  (define ball-position (make-Vector2 (/ screen-width 2.0) (/ screen-height 2.0)))

  (SetTargetFPS 60)

  (let loop ()
    (when (not (WindowShouldClose))
      (when (IsKeyDown KEY_RIGHT) (set-Vector2-x! ball-position (+ (Vector2-x ball-position) 2)))
      (when (IsKeyDown KEY_LEFT) (set-Vector2-x! ball-position (- (Vector2-x ball-position) 2)))
      (when (IsKeyDown KEY_DOWN) (set-Vector2-y! ball-position (+ (Vector2-y ball-position) 2)))
      (when (IsKeyDown KEY_UP) (set-Vector2-y! ball-position (- (Vector2-y ball-position) 2)))

      (BeginDrawing)
      (ClearBackground RAYWHITE)
      (DrawText "move the ball with arrow keys" 10 10 20 DARKGRAY)
      (DrawCircleV ball-position 50.0 MAROON)
      (EndDrawing)

      (loop)))

  (CloseWindow))
