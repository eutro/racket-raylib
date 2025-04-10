#!/usr/bin/env racket
#lang racket/base

(module+ main
  (require raylib/2d/unsafe)

  (define screen-width 800)
  (define screen-height 450)

  (InitWindow screen-width screen-height "raylib [shapes] example - basic shapes drawing")
  
  (SetTargetFPS 60)

  (let loop ([rotation 0.0])
    (unless (WindowShouldClose)

      (BeginDrawing)

      (ClearBackground RAYWHITE)

      (DrawText "some basic shapes available on raylib" 20 20 20 DARKGRAY)

      (DrawCircle         (/ screen-width 5) 120 35.0 DARKBLUE)
      (DrawCircleGradient (/ screen-width 5) 220 60.0 GREEN SKYBLUE)
      (DrawCircleLines    (/ screen-width 5) 340 80.0 DARKBLUE)

      ; Rectangle shapes and lines
      (DrawRectangle          (- (* (/ screen-width 4) 2) 60) 100 120 60 RED)
      (DrawRectangleGradientH (- (* (/ screen-width 4) 2) 90) 170 180 130 MAROON GOLD)
      (DrawRectangleLines     (- (* (/ screen-width 4) 2) 40) 320 80 60 ORANGE)

      ; Triangle shapes and lines
      (DrawTriangle (make-Vector2 (* (/ screen-width 4.0) 3) 80.0) 
                    (make-Vector2 (- (* (/ screen-width 4.0) 3) 60) 150.0)
                    (make-Vector2 (+ (* (/ screen-width 4.0) 3) 60) 150.0) VIOLET)

      (DrawTriangleLines (make-Vector2 (* (/ screen-width 4.0) 3) 160.0)
                         (make-Vector2 (- (* (/ screen-width 4.0) 3) 20) 230.0)
                         (make-Vector2 (+ (* (/ screen-width 4.0) 3) 20) 230.0) DARKBLUE)

       ; Polygon shapes and lines
      (DrawPoly        (make-Vector2 (* (/ screen-width 4.0) 3) 330.0) 6 80.0 rotation BROWN)
      (DrawPolyLines   (make-Vector2 (* (/ screen-width 4.0) 3) 330.0) 6 90.0 rotation BROWN)
      (DrawPolyLinesEx (make-Vector2 (* (/ screen-width 4.0) 3) 330.0) 6 85.0 rotation 6.0 BEIGE)

      (DrawLine 18 42 (- screen-width 18) 42 BLACK)
      (EndDrawing)

      (loop (+ rotation 0.2))))

  (CloseWindow))
