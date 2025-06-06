#!/usr/bin/env racket
#lang racket/base

(void #<<EOF
#include "raylib.h"

int main(void)
{
    // Initialization
    //--------------------------------------------------------------------------------------
    const int screenWidth = 800;
    const int screenHeight = 450;

    InitWindow(screenWidth, screenHeight, "raylib [core] example - drop files");

    int count = 0;
    char **droppedFiles = { 0 };

    SetTargetFPS(60);               // Set our game to run at 60 frames-per-second
    //--------------------------------------------------------------------------------------

    // Main game loop
    while (!WindowShouldClose())    // Detect window close button or ESC key
    {
        // Update
        //----------------------------------------------------------------------------------
        if (IsFileDropped())
        {
            droppedFiles = GetDroppedFiles(&count);
        }
        //----------------------------------------------------------------------------------

        // Draw
        //----------------------------------------------------------------------------------
        BeginDrawing();

            ClearBackground(RAYWHITE);

            if (count == 0) DrawText("Drop your files to this window!", 100, 40, 20, DARKGRAY);
            else
            {
                DrawText("Dropped files:", 100, 40, 20, DARKGRAY);

                for (int i = 0; i < count; i++)
                {
                    if (i%2 == 0) DrawRectangle(0, 85 + 40*i, screenWidth, 40, Fade(LIGHTGRAY, 0.5f));
                    else DrawRectangle(0, 85 + 40*i, screenWidth, 40, Fade(LIGHTGRAY, 0.3f));

                    DrawText(droppedFiles[i], 120, 100 + 40*i, 10, GRAY);
                }

                DrawText("Drop new files...", 100, 110 + 40*count, 20, DARKGRAY);
            }

        EndDrawing();
        //----------------------------------------------------------------------------------
    }

    // De-Initialization
    //--------------------------------------------------------------------------------------
    ClearDroppedFiles();    // Clear internal buffers

    CloseWindow();          // Close window and OpenGL context
    //--------------------------------------------------------------------------------------

    return 0;
}
EOF
)

(module+ main
  (require raylib/2d/unsafe
           raylib/derived/unsafe)

  (define screen-width 800)
  (define screen-height 450)

  (InitWindow screen-width screen-height "raylib [core] example - drop files")

  (define dropped-files (vector))

  (SetTargetFPS 60)

  (let loop ()
    (unless (WindowShouldClose)
      (when (IsFileDropped)
        (define fpl (LoadDroppedFiles))
        (set! dropped-files (FilePathList->vector fpl))
        (UnloadDroppedFiles fpl))

      (BeginDrawing)

      (ClearBackground RAYWHITE)

      (define count-v (vector-length dropped-files))
      (cond
        [(= count-v 0)
         (DrawText "Drop your files to this window!" 100 40 20 DARKGRAY)]
        [else
         (DrawText "Dropped files:" 100 40 20 DARKGRAY)

         (for ([i (in-range count-v)]
               [dropped-file (in-vector dropped-files)])
           (if (even? i)
               (DrawRectangle 0 (+ 85 (* 40 i)) screen-width 40 (Fade LIGHTGRAY 0.5))
               (DrawRectangle 0 (+ 85 (* 40 i)) screen-width 40 (Fade LIGHTGRAY 0.3)))

           (DrawText dropped-file 120 (+ 100 (* 40 i)) 10 GRAY))

         (DrawText "Drop new files..." 100 (+ 110 (* 40 count-v)) 20 DARKGRAY)])

      (EndDrawing)

      (loop)))

  (CloseWindow))
