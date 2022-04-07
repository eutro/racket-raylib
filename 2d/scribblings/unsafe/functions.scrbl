#lang scribble/manual

@(require (for-label raylib/2d/unsafe/functions raylib/2d/structs ffi/unsafe racket/base))

@table-of-contents[]

@title{Functions}
@defmodule[raylib/2d/unsafe/functions]

@defproc[(InitWindow
          [width _int]
          [height _int]
          [title _string])
         _void]{
Initialize window and OpenGL context
}

@defproc[(WindowShouldClose)
         _bool]{
Check if KEY_ESCAPE pressed or Close icon pressed
}

@defproc[(CloseWindow)
         _void]{
Close window and unload OpenGL context
}

@defproc[(IsWindowReady)
         _bool]{
Check if window has been initialized successfully
}

@defproc[(IsWindowFullscreen)
         _bool]{
Check if window is currently fullscreen
}

@defproc[(IsWindowHidden)
         _bool]{
Check if window is currently hidden (only PLATFORM_DESKTOP)
}

@defproc[(IsWindowMinimized)
         _bool]{
Check if window is currently minimized (only PLATFORM_DESKTOP)
}

@defproc[(IsWindowMaximized)
         _bool]{
Check if window is currently maximized (only PLATFORM_DESKTOP)
}

@defproc[(IsWindowFocused)
         _bool]{
Check if window is currently focused (only PLATFORM_DESKTOP)
}

@defproc[(IsWindowResized)
         _bool]{
Check if window has been resized last frame
}

@defproc[(IsWindowState
          [flag _uint])
         _bool]{
Check if one specific window flag is enabled
}

@defproc[(SetWindowState
          [flags _uint])
         _void]{
Set window configuration state using flags (only PLATFORM_DESKTOP)
}

@defproc[(ClearWindowState
          [flags _uint])
         _void]{
Clear window configuration state flags
}

@defproc[(ToggleFullscreen)
         _void]{
Toggle window state: fullscreen/windowed (only PLATFORM_DESKTOP)
}

@defproc[(MaximizeWindow)
         _void]{
Set window state: maximized, if resizable (only PLATFORM_DESKTOP)
}

@defproc[(MinimizeWindow)
         _void]{
Set window state: minimized, if resizable (only PLATFORM_DESKTOP)
}

@defproc[(RestoreWindow)
         _void]{
Set window state: not minimized/maximized (only PLATFORM_DESKTOP)
}

@defproc[(SetWindowIcon
          [image _Image])
         _void]{
Set icon for window (only PLATFORM_DESKTOP)
}

@defproc[(SetWindowTitle
          [title _string])
         _void]{
Set title for window (only PLATFORM_DESKTOP)
}

@defproc[(SetWindowPosition
          [x _int]
          [y _int])
         _void]{
Set window position on screen (only PLATFORM_DESKTOP)
}

@defproc[(SetWindowMonitor
          [monitor _int])
         _void]{
Set monitor for the current window (fullscreen mode)
}

@defproc[(SetWindowMinSize
          [width _int]
          [height _int])
         _void]{
Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
}

@defproc[(SetWindowSize
          [width _int]
          [height _int])
         _void]{
Set window dimensions
}

@defproc[(SetWindowOpacity
          [opacity _float])
         _void]{
Set window opacity [0.0f..1.0f] (only PLATFORM_DESKTOP)
}

@defproc[(GetWindowHandle)
         _pointer #;"void *"]{
Get native window handle
}

@defproc[(GetScreenWidth)
         _int]{
Get current screen width
}

@defproc[(GetScreenHeight)
         _int]{
Get current screen height
}

@defproc[(GetRenderWidth)
         _int]{
Get current render width (it considers HiDPI)
}

@defproc[(GetRenderHeight)
         _int]{
Get current render height (it considers HiDPI)
}

@defproc[(GetMonitorCount)
         _int]{
Get number of connected monitors
}

@defproc[(GetCurrentMonitor)
         _int]{
Get current connected monitor
}

@defproc[(GetMonitorPosition
          [monitor _int])
         _Vector2]{
Get specified monitor position
}

@defproc[(GetMonitorWidth
          [monitor _int])
         _int]{
Get specified monitor width (max available by monitor)
}

@defproc[(GetMonitorHeight
          [monitor _int])
         _int]{
Get specified monitor height (max available by monitor)
}

@defproc[(GetMonitorPhysicalWidth
          [monitor _int])
         _int]{
Get specified monitor physical width in millimetres
}

@defproc[(GetMonitorPhysicalHeight
          [monitor _int])
         _int]{
Get specified monitor physical height in millimetres
}

@defproc[(GetMonitorRefreshRate
          [monitor _int])
         _int]{
Get specified monitor refresh rate
}

@defproc[(GetWindowPosition)
         _Vector2]{
Get window position XY on monitor
}

@defproc[(GetWindowScaleDPI)
         _Vector2]{
Get window scale DPI factor
}

@defproc[(GetMonitorName
          [monitor _int])
         _string]{
Get the human-readable, UTF-8 encoded name of the primary monitor
}

@defproc[(SwapScreenBuffer)
         _void]{
Swap back buffer with front buffer (screen drawing)
}

@defproc[(PollInputEvents)
         _void]{
Register all input events
}

@defproc[(ShowCursor)
         _void]{
Shows cursor
}

@defproc[(HideCursor)
         _void]{
Hides cursor
}

@defproc[(IsCursorHidden)
         _bool]{
Check if cursor is not visible
}

@defproc[(EnableCursor)
         _void]{
Enables cursor (unlock cursor)
}

@defproc[(DisableCursor)
         _void]{
Disables cursor (lock cursor)
}

@defproc[(IsCursorOnScreen)
         _bool]{
Check if cursor is on the screen
}

@defproc[(ClearBackground
          [color _Color])
         _void]{
Set background color (framebuffer clear color)
}

@defproc[(BeginDrawing)
         _void]{
Setup canvas (framebuffer) to start drawing
}

@defproc[(EndDrawing)
         _void]{
End canvas drawing and swap buffers (double buffering)
}

@defproc[(BeginMode2D
          [camera _Camera2D])
         _void]{
Begin 2D mode with custom camera (2D)
}

@defproc[(EndMode2D)
         _void]{
Ends 2D mode with custom camera
}

@defproc[(BeginTextureMode
          [target _RenderTexture2D])
         _void]{
Begin drawing to render texture
}

@defproc[(EndTextureMode)
         _void]{
Ends drawing to render texture
}

@defproc[(BeginShaderMode
          [shader _Shader])
         _void]{
Begin custom shader drawing
}

@defproc[(EndShaderMode)
         _void]{
End custom shader drawing (use default shader)
}

@defproc[(BeginBlendMode
          [mode _int])
         _void]{
Begin blending mode (alpha, additive, multiplied, subtract, custom)
}

@defproc[(EndBlendMode)
         _void]{
End blending mode (reset to default: alpha blending)
}

@defproc[(BeginScissorMode
          [x _int]
          [y _int]
          [width _int]
          [height _int])
         _void]{
Begin scissor mode (define screen area for following drawing)
}

@defproc[(EndScissorMode)
         _void]{
End scissor mode
}

@defproc[(LoadShader
          [vsFileName _string]
          [fsFileName _string])
         _Shader]{
Load shader from files and bind default locations
}

@defproc[(LoadShaderFromMemory
          [vsCode _string]
          [fsCode _string])
         _Shader]{
Load shader from code strings and bind default locations
}

@defproc[(GetShaderLocation
          [shader _Shader]
          [uniformName _string])
         _int]{
Get shader uniform location
}

@defproc[(GetShaderLocationAttrib
          [shader _Shader]
          [attribName _string])
         _int]{
Get shader attribute location
}

@defproc[(SetShaderValue
          [shader _Shader]
          [locIndex _int]
          [value _pointer #;"const void *"]
          [uniformType _int])
         _void]{
Set shader uniform value
}

@defproc[(SetShaderValueV
          [shader _Shader]
          [locIndex _int]
          [value _pointer #;"const void *"]
          [uniformType _int]
          [count _int])
         _void]{
Set shader uniform value vector
}

@defproc[(SetShaderValueMatrix
          [shader _Shader]
          [locIndex _int]
          [mat _Matrix])
         _void]{
Set shader uniform value (matrix 4x4)
}

@defproc[(SetShaderValueTexture
          [shader _Shader]
          [locIndex _int]
          [texture _Texture2D])
         _void]{
Set shader uniform value for texture (sampler2d)
}

@defproc[(UnloadShader
          [shader _Shader])
         _void]{
Unload shader from GPU memory (VRAM)
}

@defproc[(GetMouseRay
          [mousePosition _Vector2]
          [camera _Camera])
         _Ray]{
Get a ray trace from mouse position
}

@defproc[(GetCameraMatrix
          [camera _Camera])
         _Matrix]{
Get camera transform matrix (view matrix)
}

@defproc[(GetCameraMatrix2D
          [camera _Camera2D])
         _Matrix]{
Get camera 2d transform matrix
}

@defproc[(GetWorldToScreen
          [position _Vector3]
          [camera _Camera])
         _Vector2]{
Get the screen space position for a 3d world space position
}

@defproc[(GetWorldToScreenEx
          [position _Vector3]
          [camera _Camera]
          [width _int]
          [height _int])
         _Vector2]{
Get size position for a 3d world space position
}

@defproc[(GetWorldToScreen2D
          [position _Vector2]
          [camera _Camera2D])
         _Vector2]{
Get the screen space position for a 2d camera world space position
}

@defproc[(GetScreenToWorld2D
          [position _Vector2]
          [camera _Camera2D])
         _Vector2]{
Get the world space position for a 2d camera screen space position
}

@defproc[(SetTargetFPS
          [fps _int])
         _void]{
Set target FPS (maximum)
}

@defproc[(GetFPS)
         _int]{
Get current FPS
}

@defproc[(GetFrameTime)
         _float]{
Get time in seconds for last frame drawn (delta time)
}

@defproc[(GetTime)
         _double]{
Get elapsed time in seconds since InitWindow()
}

@defproc[(TakeScreenshot
          [fileName _string])
         _void]{
Takes a screenshot of current screen (filename extension defines format)
}

@defproc[(SetConfigFlags
          [flags _uint])
         _void]{
Setup init configuration flags (view FLAGS)
}

@defproc[(TraceLog
          [logLevel _int]
          [text _string])
         _void]{
Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR...)
}

@defproc[(SetTraceLogLevel
          [logLevel _int])
         _void]{
Set the current threshold (minimum) log level
}

@defproc[(SetLoadFileDataCallback
          [callback _LoadFileDataCallback])
         _void]{
Set custom file binary data loader
}

@defproc[(SetSaveFileDataCallback
          [callback _SaveFileDataCallback])
         _void]{
Set custom file binary data saver
}

@defproc[(SetLoadFileTextCallback
          [callback _LoadFileTextCallback])
         _void]{
Set custom file text data loader
}

@defproc[(SetSaveFileTextCallback
          [callback _SaveFileTextCallback])
         _void]{
Set custom file text data saver
}

@defproc[(IsFileDropped)
         _bool]{
Check if a file has been dropped into window
}

@defproc[(GetDroppedFiles
          [count _pointer #;"int *"])
         _pointer #;"char **"]{
Get dropped files names (memory should be freed)
}

@defproc[(ClearDroppedFiles)
         _void]{
Clear dropped files paths buffer (free memory)
}

@defproc[(SaveStorageValue
          [position _uint]
          [value _int])
         _bool]{
Save integer value to storage file (to defined position), returns true on success
}

@defproc[(LoadStorageValue
          [position _uint])
         _int]{
Load integer value from storage file (from defined position)
}

@defproc[(OpenURL
          [url _string])
         _void]{
Open URL with default system browser (if available)
}

@defproc[(IsKeyPressed
          [key _int])
         _bool]{
Check if a key has been pressed once
}

@defproc[(IsKeyDown
          [key _int])
         _bool]{
Check if a key is being pressed
}

@defproc[(IsKeyReleased
          [key _int])
         _bool]{
Check if a key has been released once
}

@defproc[(IsKeyUp
          [key _int])
         _bool]{
Check if a key is NOT being pressed
}

@defproc[(SetExitKey
          [key _int])
         _void]{
Set a custom key to exit program (default is ESC)
}

@defproc[(GetKeyPressed)
         _int]{
Get key pressed (keycode), call it multiple times for keys queued, returns 0 when the queue is empty
}

@defproc[(GetCharPressed)
         _int]{
Get char pressed (unicode), call it multiple times for chars queued, returns 0 when the queue is empty
}

@defproc[(IsGamepadAvailable
          [gamepad _int])
         _bool]{
Check if a gamepad is available
}

@defproc[(GetGamepadName
          [gamepad _int])
         _string]{
Get gamepad internal name id
}

@defproc[(IsGamepadButtonPressed
          [gamepad _int]
          [button _int])
         _bool]{
Check if a gamepad button has been pressed once
}

@defproc[(IsGamepadButtonDown
          [gamepad _int]
          [button _int])
         _bool]{
Check if a gamepad button is being pressed
}

@defproc[(IsGamepadButtonReleased
          [gamepad _int]
          [button _int])
         _bool]{
Check if a gamepad button has been released once
}

@defproc[(IsGamepadButtonUp
          [gamepad _int]
          [button _int])
         _bool]{
Check if a gamepad button is NOT being pressed
}

@defproc[(GetGamepadButtonPressed)
         _int]{
Get the last gamepad button pressed
}

@defproc[(GetGamepadAxisCount
          [gamepad _int])
         _int]{
Get gamepad axis count for a gamepad
}

@defproc[(GetGamepadAxisMovement
          [gamepad _int]
          [axis _int])
         _float]{
Get axis movement value for a gamepad axis
}

@defproc[(SetGamepadMappings
          [mappings _string])
         _int]{
Set internal gamepad mappings (SDL_GameControllerDB)
}

@defproc[(IsMouseButtonPressed
          [button _int])
         _bool]{
Check if a mouse button has been pressed once
}

@defproc[(IsMouseButtonDown
          [button _int])
         _bool]{
Check if a mouse button is being pressed
}

@defproc[(IsMouseButtonReleased
          [button _int])
         _bool]{
Check if a mouse button has been released once
}

@defproc[(IsMouseButtonUp
          [button _int])
         _bool]{
Check if a mouse button is NOT being pressed
}

@defproc[(GetMouseX)
         _int]{
Get mouse position X
}

@defproc[(GetMouseY)
         _int]{
Get mouse position Y
}

@defproc[(GetMousePosition)
         _Vector2]{
Get mouse position XY
}

@defproc[(GetMouseDelta)
         _Vector2]{
Get mouse delta between frames
}

@defproc[(SetMousePosition
          [x _int]
          [y _int])
         _void]{
Set mouse position XY
}

@defproc[(SetMouseOffset
          [offsetX _int]
          [offsetY _int])
         _void]{
Set mouse offset
}

@defproc[(SetMouseScale
          [scaleX _float]
          [scaleY _float])
         _void]{
Set mouse scaling
}

@defproc[(GetMouseWheelMove)
         _float]{
Get mouse wheel movement Y
}

@defproc[(SetMouseCursor
          [cursor _int])
         _void]{
Set mouse cursor
}

@defproc[(GetTouchX)
         _int]{
Get touch position X for touch point 0 (relative to screen size)
}

@defproc[(GetTouchY)
         _int]{
Get touch position Y for touch point 0 (relative to screen size)
}

@defproc[(GetTouchPosition
          [index _int])
         _Vector2]{
Get touch position XY for a touch point index (relative to screen size)
}

@defproc[(GetTouchPointId
          [index _int])
         _int]{
Get touch point identifier for given index
}

@defproc[(GetTouchPointCount)
         _int]{
Get number of touch points
}

@defproc[(SetGesturesEnabled
          [flags _uint])
         _void]{
Enable a set of gestures using flags
}

@defproc[(IsGestureDetected
          [gesture _int])
         _bool]{
Check if a gesture have been detected
}

@defproc[(GetGestureDetected)
         _int]{
Get latest detected gesture
}

@defproc[(GetGestureHoldDuration)
         _float]{
Get gesture hold time in milliseconds
}

@defproc[(GetGestureDragVector)
         _Vector2]{
Get gesture drag vector
}

@defproc[(GetGestureDragAngle)
         _float]{
Get gesture drag angle
}

@defproc[(GetGesturePinchVector)
         _Vector2]{
Get gesture pinch delta
}

@defproc[(GetGesturePinchAngle)
         _float]{
Get gesture pinch angle
}

@defproc[(SetCameraMode
          [camera _Camera]
          [mode _int])
         _void]{
Set camera mode (multiple camera modes available)
}

@defproc[(UpdateCamera
          [camera _pointer #;"Camera *"])
         _void]{
Update camera position for selected mode
}

@defproc[(SetCameraPanControl
          [keyPan _int])
         _void]{
Set camera pan key to combine with mouse movement (free camera)
}

@defproc[(SetCameraAltControl
          [keyAlt _int])
         _void]{
Set camera alt key to combine with mouse movement (free camera)
}

@defproc[(SetCameraSmoothZoomControl
          [keySmoothZoom _int])
         _void]{
Set camera smooth zoom key to combine with mouse (free camera)
}

@defproc[(SetCameraMoveControls
          [keyFront _int]
          [keyBack _int]
          [keyRight _int]
          [keyLeft _int]
          [keyUp _int]
          [keyDown _int])
         _void]{
Set camera move controls (1st person and 3rd person cameras)
}

@defproc[(SetShapesTexture
          [texture _Texture2D]
          [source _Rectangle])
         _void]{
Set texture and rectangle to be used on shapes drawing
}

@defproc[(DrawPixel
          [posX _int]
          [posY _int]
          [color _Color])
         _void]{
Draw a pixel
}

@defproc[(DrawPixelV
          [position _Vector2]
          [color _Color])
         _void]{
Draw a pixel (Vector version)
}

@defproc[(DrawLine
          [startPosX _int]
          [startPosY _int]
          [endPosX _int]
          [endPosY _int]
          [color _Color])
         _void]{
Draw a line
}

@defproc[(DrawLineV
          [startPos _Vector2]
          [endPos _Vector2]
          [color _Color])
         _void]{
Draw a line (Vector version)
}

@defproc[(DrawLineEx
          [startPos _Vector2]
          [endPos _Vector2]
          [thick _float]
          [color _Color])
         _void]{
Draw a line defining thickness
}

@defproc[(DrawLineBezier
          [startPos _Vector2]
          [endPos _Vector2]
          [thick _float]
          [color _Color])
         _void]{
Draw a line using cubic-bezier curves in-out
}

@defproc[(DrawLineBezierQuad
          [startPos _Vector2]
          [endPos _Vector2]
          [controlPos _Vector2]
          [thick _float]
          [color _Color])
         _void]{
Draw line using quadratic bezier curves with a control point
}

@defproc[(DrawLineBezierCubic
          [startPos _Vector2]
          [endPos _Vector2]
          [startControlPos _Vector2]
          [endControlPos _Vector2]
          [thick _float]
          [color _Color])
         _void]{
Draw line using cubic bezier curves with 2 control points
}

@defproc[(DrawLineStrip
          [points _pointer #;"Vector2 *"]
          [pointCount _int]
          [color _Color])
         _void]{
Draw lines sequence
}

@defproc[(DrawCircle
          [centerX _int]
          [centerY _int]
          [radius _float]
          [color _Color])
         _void]{
Draw a color-filled circle
}

@defproc[(DrawCircleSector
          [center _Vector2]
          [radius _float]
          [startAngle _float]
          [endAngle _float]
          [segments _int]
          [color _Color])
         _void]{
Draw a piece of a circle
}

@defproc[(DrawCircleSectorLines
          [center _Vector2]
          [radius _float]
          [startAngle _float]
          [endAngle _float]
          [segments _int]
          [color _Color])
         _void]{
Draw circle sector outline
}

@defproc[(DrawCircleGradient
          [centerX _int]
          [centerY _int]
          [radius _float]
          [color1 _Color]
          [color2 _Color])
         _void]{
Draw a gradient-filled circle
}

@defproc[(DrawCircleV
          [center _Vector2]
          [radius _float]
          [color _Color])
         _void]{
Draw a color-filled circle (Vector version)
}

@defproc[(DrawCircleLines
          [centerX _int]
          [centerY _int]
          [radius _float]
          [color _Color])
         _void]{
Draw circle outline
}

@defproc[(DrawEllipse
          [centerX _int]
          [centerY _int]
          [radiusH _float]
          [radiusV _float]
          [color _Color])
         _void]{
Draw ellipse
}

@defproc[(DrawEllipseLines
          [centerX _int]
          [centerY _int]
          [radiusH _float]
          [radiusV _float]
          [color _Color])
         _void]{
Draw ellipse outline
}

@defproc[(DrawRing
          [center _Vector2]
          [innerRadius _float]
          [outerRadius _float]
          [startAngle _float]
          [endAngle _float]
          [segments _int]
          [color _Color])
         _void]{
Draw ring
}

@defproc[(DrawRingLines
          [center _Vector2]
          [innerRadius _float]
          [outerRadius _float]
          [startAngle _float]
          [endAngle _float]
          [segments _int]
          [color _Color])
         _void]{
Draw ring outline
}

@defproc[(DrawRectangle
          [posX _int]
          [posY _int]
          [width _int]
          [height _int]
          [color _Color])
         _void]{
Draw a color-filled rectangle
}

@defproc[(DrawRectangleV
          [position _Vector2]
          [size _Vector2]
          [color _Color])
         _void]{
Draw a color-filled rectangle (Vector version)
}

@defproc[(DrawRectangleRec
          [rec _Rectangle]
          [color _Color])
         _void]{
Draw a color-filled rectangle
}

@defproc[(DrawRectanglePro
          [rec _Rectangle]
          [origin _Vector2]
          [rotation _float]
          [color _Color])
         _void]{
Draw a color-filled rectangle with pro parameters
}

@defproc[(DrawRectangleGradientV
          [posX _int]
          [posY _int]
          [width _int]
          [height _int]
          [color1 _Color]
          [color2 _Color])
         _void]{
Draw a vertical-gradient-filled rectangle
}

@defproc[(DrawRectangleGradientH
          [posX _int]
          [posY _int]
          [width _int]
          [height _int]
          [color1 _Color]
          [color2 _Color])
         _void]{
Draw a horizontal-gradient-filled rectangle
}

@defproc[(DrawRectangleGradientEx
          [rec _Rectangle]
          [col1 _Color]
          [col2 _Color]
          [col3 _Color]
          [col4 _Color])
         _void]{
Draw a gradient-filled rectangle with custom vertex colors
}

@defproc[(DrawRectangleLines
          [posX _int]
          [posY _int]
          [width _int]
          [height _int]
          [color _Color])
         _void]{
Draw rectangle outline
}

@defproc[(DrawRectangleLinesEx
          [rec _Rectangle]
          [lineThick _float]
          [color _Color])
         _void]{
Draw rectangle outline with extended parameters
}

@defproc[(DrawRectangleRounded
          [rec _Rectangle]
          [roundness _float]
          [segments _int]
          [color _Color])
         _void]{
Draw rectangle with rounded edges
}

@defproc[(DrawRectangleRoundedLines
          [rec _Rectangle]
          [roundness _float]
          [segments _int]
          [lineThick _float]
          [color _Color])
         _void]{
Draw rectangle with rounded edges outline
}

@defproc[(DrawTriangle
          [v1 _Vector2]
          [v2 _Vector2]
          [v3 _Vector2]
          [color _Color])
         _void]{
Draw a color-filled triangle (vertex in counter-clockwise order!)
}

@defproc[(DrawTriangleLines
          [v1 _Vector2]
          [v2 _Vector2]
          [v3 _Vector2]
          [color _Color])
         _void]{
Draw triangle outline (vertex in counter-clockwise order!)
}

@defproc[(DrawTriangleFan
          [points _pointer #;"Vector2 *"]
          [pointCount _int]
          [color _Color])
         _void]{
Draw a triangle fan defined by points (first vertex is the center)
}

@defproc[(DrawTriangleStrip
          [points _pointer #;"Vector2 *"]
          [pointCount _int]
          [color _Color])
         _void]{
Draw a triangle strip defined by points
}

@defproc[(DrawPoly
          [center _Vector2]
          [sides _int]
          [radius _float]
          [rotation _float]
          [color _Color])
         _void]{
Draw a regular polygon (Vector version)
}

@defproc[(DrawPolyLines
          [center _Vector2]
          [sides _int]
          [radius _float]
          [rotation _float]
          [color _Color])
         _void]{
Draw a polygon outline of n sides
}

@defproc[(DrawPolyLinesEx
          [center _Vector2]
          [sides _int]
          [radius _float]
          [rotation _float]
          [lineThick _float]
          [color _Color])
         _void]{
Draw a polygon outline of n sides with extended parameters
}

@defproc[(CheckCollisionRecs
          [rec1 _Rectangle]
          [rec2 _Rectangle])
         _bool]{
Check collision between two rectangles
}

@defproc[(CheckCollisionCircles
          [center1 _Vector2]
          [radius1 _float]
          [center2 _Vector2]
          [radius2 _float])
         _bool]{
Check collision between two circles
}

@defproc[(CheckCollisionCircleRec
          [center _Vector2]
          [radius _float]
          [rec _Rectangle])
         _bool]{
Check collision between circle and rectangle
}

@defproc[(CheckCollisionPointRec
          [point _Vector2]
          [rec _Rectangle])
         _bool]{
Check if point is inside rectangle
}

@defproc[(CheckCollisionPointCircle
          [point _Vector2]
          [center _Vector2]
          [radius _float])
         _bool]{
Check if point is inside circle
}

@defproc[(CheckCollisionPointTriangle
          [point _Vector2]
          [p1 _Vector2]
          [p2 _Vector2]
          [p3 _Vector2])
         _bool]{
Check if point is inside a triangle
}

@defproc[(CheckCollisionLines
          [startPos1 _Vector2]
          [endPos1 _Vector2]
          [startPos2 _Vector2]
          [endPos2 _Vector2]
          [collisionPoint _pointer #;"Vector2 *"])
         _bool]{
Check the collision between two lines defined by two points each, returns collision point by reference
}

@defproc[(CheckCollisionPointLine
          [point _Vector2]
          [p1 _Vector2]
          [p2 _Vector2]
          [threshold _int])
         _bool]{
Check if point belongs to line created between two points [p1] and [p2] with defined margin in pixels [threshold]
}

@defproc[(GetCollisionRec
          [rec1 _Rectangle]
          [rec2 _Rectangle])
         _Rectangle]{
Get collision rectangle for two rectangles collision
}

@defproc[(LoadImage
          [fileName _string])
         _Image]{
Load image from file into CPU memory (RAM)
}

@defproc[(LoadImageRaw
          [fileName _string]
          [width _int]
          [height _int]
          [format _int]
          [headerSize _int])
         _Image]{
Load image from RAW file data
}

@defproc[(LoadImageAnim
          [fileName _string]
          [frames _pointer #;"int *"])
         _Image]{
Load image sequence from file (frames appended to image.data)
}

@defproc[(LoadImageFromMemory
          [fileType _string]
          [fileData _pointer #;"const unsigned char *"]
          [dataSize _int])
         _Image]{
Load image from memory buffer, fileType refers to extension: i.e. '.png'
}

@defproc[(LoadImageFromTexture
          [texture _Texture2D])
         _Image]{
Load image from GPU texture data
}

@defproc[(LoadImageFromScreen)
         _Image]{
Load image from screen buffer and (screenshot)
}

@defproc[(UnloadImage
          [image _Image])
         _void]{
Unload image from CPU memory (RAM)
}

@defproc[(ExportImage
          [image _Image]
          [fileName _string])
         _bool]{
Export image data to file, returns true on success
}

@defproc[(ExportImageAsCode
          [image _Image]
          [fileName _string])
         _bool]{
Export image as code file defining an array of bytes, returns true on success
}

@defproc[(GenImageColor
          [width _int]
          [height _int]
          [color _Color])
         _Image]{
Generate image: plain color
}

@defproc[(GenImageGradientV
          [width _int]
          [height _int]
          [top _Color]
          [bottom _Color])
         _Image]{
Generate image: vertical gradient
}

@defproc[(GenImageGradientH
          [width _int]
          [height _int]
          [left _Color]
          [right _Color])
         _Image]{
Generate image: horizontal gradient
}

@defproc[(GenImageGradientRadial
          [width _int]
          [height _int]
          [density _float]
          [inner _Color]
          [outer _Color])
         _Image]{
Generate image: radial gradient
}

@defproc[(GenImageChecked
          [width _int]
          [height _int]
          [checksX _int]
          [checksY _int]
          [col1 _Color]
          [col2 _Color])
         _Image]{
Generate image: checked
}

@defproc[(GenImageWhiteNoise
          [width _int]
          [height _int]
          [factor _float])
         _Image]{
Generate image: white noise
}

@defproc[(GenImageCellular
          [width _int]
          [height _int]
          [tileSize _int])
         _Image]{
Generate image: cellular algorithm, bigger tileSize means bigger cells
}

@defproc[(ImageCopy
          [image _Image])
         _Image]{
Create an image duplicate (useful for transformations)
}

@defproc[(ImageFromImage
          [image _Image]
          [rec _Rectangle])
         _Image]{
Create an image from another image piece
}

@defproc[(ImageText
          [text _string]
          [fontSize _int]
          [color _Color])
         _Image]{
Create an image from text (default font)
}

@defproc[(ImageTextEx
          [font _Font]
          [text _string]
          [fontSize _float]
          [spacing _float]
          [tint _Color])
         _Image]{
Create an image from text (custom sprite font)
}

@defproc[(ImageFormat
          [image _pointer #;"Image *"]
          [newFormat _int])
         _void]{
Convert image data to desired format
}

@defproc[(ImageToPOT
          [image _pointer #;"Image *"]
          [fill _Color])
         _void]{
Convert image to POT (power-of-two)
}

@defproc[(ImageCrop
          [image _pointer #;"Image *"]
          [crop _Rectangle])
         _void]{
Crop an image to a defined rectangle
}

@defproc[(ImageAlphaCrop
          [image _pointer #;"Image *"]
          [threshold _float])
         _void]{
Crop image depending on alpha value
}

@defproc[(ImageAlphaClear
          [image _pointer #;"Image *"]
          [color _Color]
          [threshold _float])
         _void]{
Clear alpha channel to desired color
}

@defproc[(ImageAlphaMask
          [image _pointer #;"Image *"]
          [alphaMask _Image])
         _void]{
Apply alpha mask to image
}

@defproc[(ImageAlphaPremultiply
          [image _pointer #;"Image *"])
         _void]{
Premultiply alpha channel
}

@defproc[(ImageResize
          [image _pointer #;"Image *"]
          [newWidth _int]
          [newHeight _int])
         _void]{
Resize image (Bicubic scaling algorithm)
}

@defproc[(ImageResizeNN
          [image _pointer #;"Image *"]
          [newWidth _int]
          [newHeight _int])
         _void]{
Resize image (Nearest-Neighbor scaling algorithm)
}

@defproc[(ImageResizeCanvas
          [image _pointer #;"Image *"]
          [newWidth _int]
          [newHeight _int]
          [offsetX _int]
          [offsetY _int]
          [fill _Color])
         _void]{
Resize canvas and fill with color
}

@defproc[(ImageMipmaps
          [image _pointer #;"Image *"])
         _void]{
Compute all mipmap levels for a provided image
}

@defproc[(ImageDither
          [image _pointer #;"Image *"]
          [rBpp _int]
          [gBpp _int]
          [bBpp _int]
          [aBpp _int])
         _void]{
Dither image data to 16bpp or lower (Floyd-Steinberg dithering)
}

@defproc[(ImageFlipVertical
          [image _pointer #;"Image *"])
         _void]{
Flip image vertically
}

@defproc[(ImageFlipHorizontal
          [image _pointer #;"Image *"])
         _void]{
Flip image horizontally
}

@defproc[(ImageRotateCW
          [image _pointer #;"Image *"])
         _void]{
Rotate image clockwise 90deg
}

@defproc[(ImageRotateCCW
          [image _pointer #;"Image *"])
         _void]{
Rotate image counter-clockwise 90deg
}

@defproc[(ImageColorTint
          [image _pointer #;"Image *"]
          [color _Color])
         _void]{
Modify image color: tint
}

@defproc[(ImageColorInvert
          [image _pointer #;"Image *"])
         _void]{
Modify image color: invert
}

@defproc[(ImageColorGrayscale
          [image _pointer #;"Image *"])
         _void]{
Modify image color: grayscale
}

@defproc[(ImageColorContrast
          [image _pointer #;"Image *"]
          [contrast _float])
         _void]{
Modify image color: contrast (-100 to 100)
}

@defproc[(ImageColorBrightness
          [image _pointer #;"Image *"]
          [brightness _int])
         _void]{
Modify image color: brightness (-255 to 255)
}

@defproc[(ImageColorReplace
          [image _pointer #;"Image *"]
          [color _Color]
          [replace _Color])
         _void]{
Modify image color: replace color
}

@defproc[(LoadImageColors
          [image _Image])
         _pointer #;"Color *"]{
Load color data from image as a Color array (RGBA - 32bit)
}

@defproc[(LoadImagePalette
          [image _Image]
          [maxPaletteSize _int]
          [colorCount _pointer #;"int *"])
         _pointer #;"Color *"]{
Load colors palette from image as a Color array (RGBA - 32bit)
}

@defproc[(UnloadImageColors
          [colors _pointer #;"Color *"])
         _void]{
Unload color data loaded with LoadImageColors()
}

@defproc[(UnloadImagePalette
          [colors _pointer #;"Color *"])
         _void]{
Unload colors palette loaded with LoadImagePalette()
}

@defproc[(GetImageAlphaBorder
          [image _Image]
          [threshold _float])
         _Rectangle]{
Get image alpha border rectangle
}

@defproc[(GetImageColor
          [image _Image]
          [x _int]
          [y _int])
         _Color]{
Get image pixel color at (x, y) position
}

@defproc[(ImageClearBackground
          [dst _pointer #;"Image *"]
          [color _Color])
         _void]{
Clear image background with given color
}

@defproc[(ImageDrawPixel
          [dst _pointer #;"Image *"]
          [posX _int]
          [posY _int]
          [color _Color])
         _void]{
Draw pixel within an image
}

@defproc[(ImageDrawPixelV
          [dst _pointer #;"Image *"]
          [position _Vector2]
          [color _Color])
         _void]{
Draw pixel within an image (Vector version)
}

@defproc[(ImageDrawLine
          [dst _pointer #;"Image *"]
          [startPosX _int]
          [startPosY _int]
          [endPosX _int]
          [endPosY _int]
          [color _Color])
         _void]{
Draw line within an image
}

@defproc[(ImageDrawLineV
          [dst _pointer #;"Image *"]
          [start _Vector2]
          [end _Vector2]
          [color _Color])
         _void]{
Draw line within an image (Vector version)
}

@defproc[(ImageDrawCircle
          [dst _pointer #;"Image *"]
          [centerX _int]
          [centerY _int]
          [radius _int]
          [color _Color])
         _void]{
Draw circle within an image
}

@defproc[(ImageDrawCircleV
          [dst _pointer #;"Image *"]
          [center _Vector2]
          [radius _int]
          [color _Color])
         _void]{
Draw circle within an image (Vector version)
}

@defproc[(ImageDrawRectangle
          [dst _pointer #;"Image *"]
          [posX _int]
          [posY _int]
          [width _int]
          [height _int]
          [color _Color])
         _void]{
Draw rectangle within an image
}

@defproc[(ImageDrawRectangleV
          [dst _pointer #;"Image *"]
          [position _Vector2]
          [size _Vector2]
          [color _Color])
         _void]{
Draw rectangle within an image (Vector version)
}

@defproc[(ImageDrawRectangleRec
          [dst _pointer #;"Image *"]
          [rec _Rectangle]
          [color _Color])
         _void]{
Draw rectangle within an image
}

@defproc[(ImageDrawRectangleLines
          [dst _pointer #;"Image *"]
          [rec _Rectangle]
          [thick _int]
          [color _Color])
         _void]{
Draw rectangle lines within an image
}

@defproc[(ImageDraw
          [dst _pointer #;"Image *"]
          [src _Image]
          [srcRec _Rectangle]
          [dstRec _Rectangle]
          [tint _Color])
         _void]{
Draw a source image within a destination image (tint applied to source)
}

@defproc[(ImageDrawText
          [dst _pointer #;"Image *"]
          [text _string]
          [posX _int]
          [posY _int]
          [fontSize _int]
          [color _Color])
         _void]{
Draw text (using default font) within an image (destination)
}

@defproc[(ImageDrawTextEx
          [dst _pointer #;"Image *"]
          [font _Font]
          [text _string]
          [position _Vector2]
          [fontSize _float]
          [spacing _float]
          [tint _Color])
         _void]{
Draw text (custom sprite font) within an image (destination)
}

@defproc[(LoadTexture
          [fileName _string])
         _Texture2D]{
Load texture from file into GPU memory (VRAM)
}

@defproc[(LoadTextureFromImage
          [image _Image])
         _Texture2D]{
Load texture from image data
}

@defproc[(LoadTextureCubemap
          [image _Image]
          [layout _int])
         _TextureCubemap]{
Load cubemap from image, multiple image cubemap layouts supported
}

@defproc[(LoadRenderTexture
          [width _int]
          [height _int])
         _RenderTexture2D]{
Load texture for rendering (framebuffer)
}

@defproc[(UnloadTexture
          [texture _Texture2D])
         _void]{
Unload texture from GPU memory (VRAM)
}

@defproc[(UnloadRenderTexture
          [target _RenderTexture2D])
         _void]{
Unload render texture from GPU memory (VRAM)
}

@defproc[(UpdateTexture
          [texture _Texture2D]
          [pixels _pointer #;"const void *"])
         _void]{
Update GPU texture with new data
}

@defproc[(UpdateTextureRec
          [texture _Texture2D]
          [rec _Rectangle]
          [pixels _pointer #;"const void *"])
         _void]{
Update GPU texture rectangle with new data
}

@defproc[(GenTextureMipmaps
          [texture _pointer #;"Texture2D *"])
         _void]{
Generate GPU mipmaps for a texture
}

@defproc[(SetTextureFilter
          [texture _Texture2D]
          [filter _int])
         _void]{
Set texture scaling filter mode
}

@defproc[(SetTextureWrap
          [texture _Texture2D]
          [wrap _int])
         _void]{
Set texture wrapping mode
}

@defproc[(DrawTexture
          [texture _Texture2D]
          [posX _int]
          [posY _int]
          [tint _Color])
         _void]{
Draw a Texture2D
}

@defproc[(DrawTextureV
          [texture _Texture2D]
          [position _Vector2]
          [tint _Color])
         _void]{
Draw a Texture2D with position defined as Vector2
}

@defproc[(DrawTextureEx
          [texture _Texture2D]
          [position _Vector2]
          [rotation _float]
          [scale _float]
          [tint _Color])
         _void]{
Draw a Texture2D with extended parameters
}

@defproc[(DrawTextureRec
          [texture _Texture2D]
          [source _Rectangle]
          [position _Vector2]
          [tint _Color])
         _void]{
Draw a part of a texture defined by a rectangle
}

@defproc[(DrawTextureQuad
          [texture _Texture2D]
          [tiling _Vector2]
          [offset _Vector2]
          [quad _Rectangle]
          [tint _Color])
         _void]{
Draw texture quad with tiling and offset parameters
}

@defproc[(DrawTextureTiled
          [texture _Texture2D]
          [source _Rectangle]
          [dest _Rectangle]
          [origin _Vector2]
          [rotation _float]
          [scale _float]
          [tint _Color])
         _void]{
Draw part of a texture (defined by a rectangle) with rotation and scale tiled into dest.
}

@defproc[(DrawTexturePro
          [texture _Texture2D]
          [source _Rectangle]
          [dest _Rectangle]
          [origin _Vector2]
          [rotation _float]
          [tint _Color])
         _void]{
Draw a part of a texture defined by a rectangle with 'pro' parameters
}

@defproc[(DrawTextureNPatch
          [texture _Texture2D]
          [nPatchInfo _NPatchInfo]
          [dest _Rectangle]
          [origin _Vector2]
          [rotation _float]
          [tint _Color])
         _void]{
Draws a texture (or part of it) that stretches or shrinks nicely
}

@defproc[(DrawTexturePoly
          [texture _Texture2D]
          [center _Vector2]
          [points _pointer #;"Vector2 *"]
          [texcoords _pointer #;"Vector2 *"]
          [pointCount _int]
          [tint _Color])
         _void]{
Draw a textured polygon
}

@defproc[(Fade
          [color _Color]
          [alpha _float])
         _Color]{
Get color with alpha applied, alpha goes from 0.0f to 1.0f
}

@defproc[(ColorToInt
          [color _Color])
         _int]{
Get hexadecimal value for a Color
}

@defproc[(ColorNormalize
          [color _Color])
         _Vector4]{
Get Color normalized as float [0..1]
}

@defproc[(ColorFromNormalized
          [normalized _Vector4])
         _Color]{
Get Color from normalized values [0..1]
}

@defproc[(ColorToHSV
          [color _Color])
         _Vector3]{
Get HSV values for a Color, hue [0..360], saturation/value [0..1]
}

@defproc[(ColorFromHSV
          [hue _float]
          [saturation _float]
          [value _float])
         _Color]{
Get a Color from HSV values, hue [0..360], saturation/value [0..1]
}

@defproc[(ColorAlpha
          [color _Color]
          [alpha _float])
         _Color]{
Get color with alpha applied, alpha goes from 0.0f to 1.0f
}

@defproc[(ColorAlphaBlend
          [dst _Color]
          [src _Color]
          [tint _Color])
         _Color]{
Get src alpha-blended into dst color with tint
}

@defproc[(GetColor
          [hexValue _uint])
         _Color]{
Get Color structure from hexadecimal value
}

@defproc[(GetPixelColor
          [srcPtr _pointer #;"void *"]
          [format _int])
         _Color]{
Get Color from a source pixel pointer of certain format
}

@defproc[(SetPixelColor
          [dstPtr _pointer #;"void *"]
          [color _Color]
          [format _int])
         _void]{
Set color formatted into destination pixel pointer
}

@defproc[(GetPixelDataSize
          [width _int]
          [height _int]
          [format _int])
         _int]{
Get pixel data size in bytes for certain format
}

@defproc[(GetFontDefault)
         _Font]{
Get the default Font
}

@defproc[(LoadFont
          [fileName _string])
         _Font]{
Load font from file into GPU memory (VRAM)
}

@defproc[(LoadFontEx
          [fileName _string]
          [fontSize _int]
          [fontChars _pointer #;"int *"]
          [glyphCount _int])
         _Font]{
Load font from file with extended parameters, use NULL for fontChars and 0 for glyphCount to load the default character set
}

@defproc[(LoadFontFromImage
          [image _Image]
          [key _Color]
          [firstChar _int])
         _Font]{
Load font from Image (XNA style)
}

@defproc[(LoadFontFromMemory
          [fileType _string]
          [fileData _pointer #;"const unsigned char *"]
          [dataSize _int]
          [fontSize _int]
          [fontChars _pointer #;"int *"]
          [glyphCount _int])
         _Font]{
Load font from memory buffer, fileType refers to extension: i.e. '.ttf'
}

@defproc[(LoadFontData
          [fileData _pointer #;"const unsigned char *"]
          [dataSize _int]
          [fontSize _int]
          [fontChars _pointer #;"int *"]
          [glyphCount _int]
          [type _int])
         _pointer #;"GlyphInfo *"]{
Load font data for further use
}

@defproc[(GenImageFontAtlas
          [chars _pointer #;"const GlyphInfo *"]
          [recs _pointer #;"Rectangle **"]
          [glyphCount _int]
          [fontSize _int]
          [padding _int]
          [packMethod _int])
         _Image]{
Generate image font atlas using chars info
}

@defproc[(UnloadFontData
          [chars _pointer #;"GlyphInfo *"]
          [glyphCount _int])
         _void]{
Unload font chars info data (RAM)
}

@defproc[(UnloadFont
          [font _Font])
         _void]{
Unload font from GPU memory (VRAM)
}

@defproc[(ExportFontAsCode
          [font _Font]
          [fileName _string])
         _bool]{
Export font as code file, returns true on success
}

@defproc[(DrawFPS
          [posX _int]
          [posY _int])
         _void]{
Draw current FPS
}

@defproc[(DrawText
          [text _string]
          [posX _int]
          [posY _int]
          [fontSize _int]
          [color _Color])
         _void]{
Draw text (using default font)
}

@defproc[(DrawTextEx
          [font _Font]
          [text _string]
          [position _Vector2]
          [fontSize _float]
          [spacing _float]
          [tint _Color])
         _void]{
Draw text using font and additional parameters
}

@defproc[(DrawTextPro
          [font _Font]
          [text _string]
          [position _Vector2]
          [origin _Vector2]
          [rotation _float]
          [fontSize _float]
          [spacing _float]
          [tint _Color])
         _void]{
Draw text using Font and pro parameters (rotation)
}

@defproc[(DrawTextCodepoint
          [font _Font]
          [codepoint _int]
          [position _Vector2]
          [fontSize _float]
          [tint _Color])
         _void]{
Draw one character (codepoint)
}

@defproc[(DrawTextCodepoints
          [font _Font]
          [codepoints _pointer #;"const int *"]
          [count _int]
          [position _Vector2]
          [fontSize _float]
          [spacing _float]
          [tint _Color])
         _void]{
Draw multiple character (codepoint)
}

@defproc[(MeasureText
          [text _string]
          [fontSize _int])
         _int]{
Measure string width for default font
}

@defproc[(MeasureTextEx
          [font _Font]
          [text _string]
          [fontSize _float]
          [spacing _float])
         _Vector2]{
Measure string size for Font
}

@defproc[(GetGlyphIndex
          [font _Font]
          [codepoint _int])
         _int]{
Get glyph index position in font for a codepoint (unicode character), fallback to '?' if not found
}

@defproc[(GetGlyphInfo
          [font _Font]
          [codepoint _int])
         _GlyphInfo]{
Get glyph font info data for a codepoint (unicode character), fallback to '?' if not found
}

@defproc[(GetGlyphAtlasRec
          [font _Font]
          [codepoint _int])
         _Rectangle]{
Get glyph rectangle in font atlas for a codepoint (unicode character), fallback to '?' if not found
}

@defproc[(LoadCodepoints
          [text _string]
          [count _pointer #;"int *"])
         _pointer #;"int *"]{
Load all codepoints from a UTF-8 text string, codepoints count returned by parameter
}

@defproc[(UnloadCodepoints
          [codepoints _pointer #;"int *"])
         _void]{
Unload codepoints data from memory
}

@defproc[(InitAudioDevice)
         _void]{
Initialize audio device and context
}

@defproc[(CloseAudioDevice)
         _void]{
Close the audio device and context
}

@defproc[(IsAudioDeviceReady)
         _bool]{
Check if audio device has been initialized successfully
}

@defproc[(SetMasterVolume
          [volume _float])
         _void]{
Set master volume (listener)
}

@defproc[(LoadWave
          [fileName _string])
         _Wave]{
Load wave data from file
}

@defproc[(LoadWaveFromMemory
          [fileType _string]
          [fileData _pointer #;"const unsigned char *"]
          [dataSize _int])
         _Wave]{
Load wave from memory buffer, fileType refers to extension: i.e. '.wav'
}

@defproc[(LoadSound
          [fileName _string])
         _Sound]{
Load sound from file
}

@defproc[(LoadSoundFromWave
          [wave _Wave])
         _Sound]{
Load sound from wave data
}

@defproc[(UpdateSound
          [sound _Sound]
          [data _pointer #;"const void *"]
          [sampleCount _int])
         _void]{
Update sound buffer with new data
}

@defproc[(UnloadWave
          [wave _Wave])
         _void]{
Unload wave data
}

@defproc[(UnloadSound
          [sound _Sound])
         _void]{
Unload sound
}

@defproc[(ExportWave
          [wave _Wave]
          [fileName _string])
         _bool]{
Export wave data to file, returns true on success
}

@defproc[(ExportWaveAsCode
          [wave _Wave]
          [fileName _string])
         _bool]{
Export wave sample data to code (.h), returns true on success
}

@defproc[(PlaySound
          [sound _Sound])
         _void]{
Play a sound
}

@defproc[(StopSound
          [sound _Sound])
         _void]{
Stop playing a sound
}

@defproc[(PauseSound
          [sound _Sound])
         _void]{
Pause a sound
}

@defproc[(ResumeSound
          [sound _Sound])
         _void]{
Resume a paused sound
}

@defproc[(PlaySoundMulti
          [sound _Sound])
         _void]{
Play a sound (using multichannel buffer pool)
}

@defproc[(StopSoundMulti)
         _void]{
Stop any sound playing (using multichannel buffer pool)
}

@defproc[(GetSoundsPlaying)
         _int]{
Get number of sounds playing in the multichannel
}

@defproc[(IsSoundPlaying
          [sound _Sound])
         _bool]{
Check if a sound is currently playing
}

@defproc[(SetSoundVolume
          [sound _Sound]
          [volume _float])
         _void]{
Set volume for a sound (1.0 is max level)
}

@defproc[(SetSoundPitch
          [sound _Sound]
          [pitch _float])
         _void]{
Set pitch for a sound (1.0 is base level)
}

@defproc[(SetSoundPan
          [sound _Sound]
          [pan _float])
         _void]{
Set pan for a sound (0.5 is center)
}

@defproc[(WaveCopy
          [wave _Wave])
         _Wave]{
Copy a wave to a new wave
}

@defproc[(WaveCrop
          [wave _pointer #;"Wave *"]
          [initSample _int]
          [finalSample _int])
         _void]{
Crop a wave to defined samples range
}

@defproc[(WaveFormat
          [wave _pointer #;"Wave *"]
          [sampleRate _int]
          [sampleSize _int]
          [channels _int])
         _void]{
Convert wave data to desired format
}

@defproc[(LoadWaveSamples
          [wave _Wave])
         _pointer #;"float *"]{
Load samples data from wave as a 32bit float data array
}

@defproc[(UnloadWaveSamples
          [samples _pointer #;"float *"])
         _void]{
Unload samples data loaded with LoadWaveSamples()
}

@defproc[(LoadMusicStream
          [fileName _string])
         _Music]{
Load music stream from file
}

@defproc[(LoadMusicStreamFromMemory
          [fileType _string]
          [data _pointer #;"const unsigned char *"]
          [dataSize _int])
         _Music]{
Load music stream from data
}

@defproc[(UnloadMusicStream
          [music _Music])
         _void]{
Unload music stream
}

@defproc[(PlayMusicStream
          [music _Music])
         _void]{
Start music playing
}

@defproc[(IsMusicStreamPlaying
          [music _Music])
         _bool]{
Check if music is playing
}

@defproc[(UpdateMusicStream
          [music _Music])
         _void]{
Updates buffers for music streaming
}

@defproc[(StopMusicStream
          [music _Music])
         _void]{
Stop music playing
}

@defproc[(PauseMusicStream
          [music _Music])
         _void]{
Pause music playing
}

@defproc[(ResumeMusicStream
          [music _Music])
         _void]{
Resume playing paused music
}

@defproc[(SeekMusicStream
          [music _Music]
          [position _float])
         _void]{
Seek music to a position (in seconds)
}

@defproc[(SetMusicVolume
          [music _Music]
          [volume _float])
         _void]{
Set volume for music (1.0 is max level)
}

@defproc[(SetMusicPitch
          [music _Music]
          [pitch _float])
         _void]{
Set pitch for a music (1.0 is base level)
}

@defproc[(SetMusicPan
          [music _Music]
          [pan _float])
         _void]{
Set pan for a music (0.5 is center)
}

@defproc[(GetMusicTimeLength
          [music _Music])
         _float]{
Get music time length (in seconds)
}

@defproc[(GetMusicTimePlayed
          [music _Music])
         _float]{
Get current music time played (in seconds)
}

@defproc[(LoadAudioStream
          [sampleRate _uint]
          [sampleSize _uint]
          [channels _uint])
         _AudioStream]{
Load audio stream (to stream raw audio pcm data)
}

@defproc[(UnloadAudioStream
          [stream _AudioStream])
         _void]{
Unload audio stream and free memory
}

@defproc[(UpdateAudioStream
          [stream _AudioStream]
          [data _pointer #;"const void *"]
          [frameCount _int])
         _void]{
Update audio stream buffers with data
}

@defproc[(IsAudioStreamProcessed
          [stream _AudioStream])
         _bool]{
Check if any audio stream buffers requires refill
}

@defproc[(PlayAudioStream
          [stream _AudioStream])
         _void]{
Play audio stream
}

@defproc[(PauseAudioStream
          [stream _AudioStream])
         _void]{
Pause audio stream
}

@defproc[(ResumeAudioStream
          [stream _AudioStream])
         _void]{
Resume audio stream
}

@defproc[(IsAudioStreamPlaying
          [stream _AudioStream])
         _bool]{
Check if audio stream is playing
}

@defproc[(StopAudioStream
          [stream _AudioStream])
         _void]{
Stop audio stream
}

@defproc[(SetAudioStreamVolume
          [stream _AudioStream]
          [volume _float])
         _void]{
Set volume for audio stream (1.0 is max level)
}

@defproc[(SetAudioStreamPitch
          [stream _AudioStream]
          [pitch _float])
         _void]{
Set pitch for audio stream (1.0 is base level)
}

@defproc[(SetAudioStreamPan
          [stream _AudioStream]
          [pan _float])
         _void]{
Set pan for audio stream (0.5 is centered)
}

@defproc[(SetAudioStreamBufferSizeDefault
          [size _int])
         _void]{
Default size for new audio streams
}

@defproc[(SetAudioStreamCallback
          [stream _AudioStream]
          [callback _AudioCallback])
         _void]{
Audio thread callback to request new data
}

@defproc[(AttachAudioStreamProcessor
          [stream _AudioStream]
          [processor _AudioCallback])
         _void]{

}

@defproc[(DetachAudioStreamProcessor
          [stream _AudioStream]
          [processor _AudioCallback])
         _void]{

}

@section{Excluded Functions}
This is a list of all the functions excluded from these bindings.

These are documented here in case you are looking at one of the
@hyperlink["https://www.raylib.com/examples.html"]{Raylib examples}
and come across a function that doesn't exist in these bindings.

@(require (for-label racket/gui/base))

@tabular[
  #:style 'boxed
  #:row-properties '(bottom-border)
  (list (list @bold{Function(s) Excluded}
              @bold{Reason})
        (list @list{@racket[SetClipboardText], @racket[GetClipboardText]}
              @list{@racket[clipboard<%>] should be used instead})
        (list @list{@racket[WaitTime]}
              @list{@racket[sleep] should be used instead})
        (list @list{@racket[GetRandomValue], @racket[SetRandomSeed]}
              @list{@racket[random] should be used instead})
        (list @list{@racket[MemAlloc], @racket[MemRealloc], @racket[MemFree]}
              @list{@racket[malloc], @racket[free] and other Racket pointer
                    conversion functions should be used instead})
        (list @list{@racket[CompressData], @racket[DecompressData], @racket[EncodeDataBase64], @racket[DecodeDataBase64]}
              @list{@seclink["zip" #:doc '(lib "file/scribblings/file.scrbl")]{@racket[file/zip]}
                    and
                    @seclink["base64" #:doc '(lib "net/scribblings/net.scrbl")]{@racket[net/base64]}
                    (or alternatives) should be used instead})
        (list @list{@racket[LoadFileData], @racket[LoadFileText], @racket[UnloadFileData], @racket[UnloadFileText], @racket[SaveFileData], @racket[SaveFileText], @racket[FileExists], @racket[DirectoryExists], @racket[IsFileExtension], @racket[GetFileLength], @racket[GetFileExtension], @racket[GetFileName], @racket[GetFileNameWithoutExt], @racket[GetDirectoryPath], @racket[GetPrevDirectoryPath], @racket[GetWorkingDirectory], @racket[GetApplicationDirectory], @racket[GetDirectoryFiles], @racket[ClearDirectoryFiles], @racket[ChangeDirectory], @racket[GetFileModTime]}
              @list{Racket's own IO functions should be used instead})
        (list @list{@racket[GetCodepointCount], @racket[GetCodepoint], @racket[CodepointToUTF8], @racket[TextCodepointsToUTF8], @racket[TextCopy], @racket[TextIsEqual], @racket[TextLength], @racket[TextFormat], @racket[TextSubtext], @racket[TextReplace], @racket[TextInsert], @racket[TextJoin], @racket[TextSplit], @racket[TextAppend], @racket[TextFindIndex], @racket[TextToUpper], @racket[TextToLower], @racket[TextToPascal], @racket[TextToInteger]}
              @list{Racket's own string functions should be used instead})
        (list @list{@racket[SetTraceLogCallback]}
              @list{this takes a varargs function pointer, which is
                    impossible to produce with pure Racket bindings})
        (list @list{@racket[BeginVrStereoMode], @racket[BeginMode3D], @racket[EndVrStereoMode], @racket[EndMode3D], @racket[LoadVrStereoConfig], @racket[UnloadVrStereoConfig], @racket[DrawLine3D], @racket[DrawPoint3D], @racket[DrawCircle3D], @racket[DrawTriangle3D], @racket[DrawTriangleStrip3D], @racket[DrawCube], @racket[DrawCubeV], @racket[DrawCubeWires], @racket[DrawCubeWiresV], @racket[DrawCubeTexture], @racket[DrawCubeTextureRec], @racket[DrawSphere], @racket[DrawSphereEx], @racket[DrawSphereWires], @racket[DrawCylinder], @racket[DrawCylinderEx], @racket[DrawCylinderWires], @racket[DrawCylinderWiresEx], @racket[DrawPlane], @racket[DrawRay], @racket[DrawGrid], @racket[LoadModel], @racket[LoadModelFromMesh], @racket[UnloadModel], @racket[UnloadModelKeepMeshes], @racket[GetModelBoundingBox], @racket[DrawModel], @racket[DrawModelEx], @racket[DrawModelWires], @racket[DrawModelWiresEx], @racket[DrawBoundingBox], @racket[DrawBillboard], @racket[DrawBillboardRec], @racket[DrawBillboardPro], @racket[UploadMesh], @racket[UpdateMeshBuffer], @racket[UnloadMesh], @racket[DrawMesh], @racket[DrawMeshInstanced], @racket[ExportMesh], @racket[GetMeshBoundingBox], @racket[GenMeshTangents], @racket[GenMeshBinormals], @racket[GenMeshPoly], @racket[GenMeshPlane], @racket[GenMeshCube], @racket[GenMeshSphere], @racket[GenMeshHemiSphere], @racket[GenMeshCylinder], @racket[GenMeshCone], @racket[GenMeshTorus], @racket[GenMeshKnot], @racket[GenMeshHeightmap], @racket[GenMeshCubicmap], @racket[LoadMaterials], @racket[LoadMaterialDefault], @racket[UnloadMaterial], @racket[SetMaterialTexture], @racket[SetModelMeshMaterial], @racket[LoadModelAnimations], @racket[UpdateModelAnimation], @racket[UnloadModelAnimation], @racket[UnloadModelAnimations], @racket[IsModelAnimationValid], @racket[CheckCollisionSpheres], @racket[CheckCollisionBoxes], @racket[CheckCollisionBoxSphere], @racket[GetRayCollisionSphere], @racket[GetRayCollisionBox], @racket[GetRayCollisionMesh], @racket[GetRayCollisionTriangle], @racket[GetRayCollisionQuad]}
              @list{these are not applicable to 2D rendering}))
]
