#lang scribble/manual

@(require (for-label raylib/sys/functions ffi/unsafe racket/base))

@title{Functions}
@defmodule[raylib/sys/functions]

@defproc[(InitWindow) any?]{
Initialize window and OpenGL context
}

@defproc[(WindowShouldClose) any?]{
Check if KEY_ESCAPE pressed or Close icon pressed
}

@defproc[(CloseWindow) any?]{
Close window and unload OpenGL context
}

@defproc[(IsWindowReady) any?]{
Check if window has been initialized successfully
}

@defproc[(IsWindowFullscreen) any?]{
Check if window is currently fullscreen
}

@defproc[(IsWindowHidden) any?]{
Check if window is currently hidden (only PLATFORM_DESKTOP)
}

@defproc[(IsWindowMinimized) any?]{
Check if window is currently minimized (only PLATFORM_DESKTOP)
}

@defproc[(IsWindowMaximized) any?]{
Check if window is currently maximized (only PLATFORM_DESKTOP)
}

@defproc[(IsWindowFocused) any?]{
Check if window is currently focused (only PLATFORM_DESKTOP)
}

@defproc[(IsWindowResized) any?]{
Check if window has been resized last frame
}

@defproc[(IsWindowState) any?]{
Check if one specific window flag is enabled
}

@defproc[(SetWindowState) any?]{
Set window configuration state using flags
}

@defproc[(ClearWindowState) any?]{
Clear window configuration state flags
}

@defproc[(ToggleFullscreen) any?]{
Toggle window state: fullscreen/windowed (only PLATFORM_DESKTOP)
}

@defproc[(MaximizeWindow) any?]{
Set window state: maximized, if resizable (only PLATFORM_DESKTOP)
}

@defproc[(MinimizeWindow) any?]{
Set window state: minimized, if resizable (only PLATFORM_DESKTOP)
}

@defproc[(RestoreWindow) any?]{
Set window state: not minimized/maximized (only PLATFORM_DESKTOP)
}

@defproc[(SetWindowIcon) any?]{
Set icon for window (only PLATFORM_DESKTOP)
}

@defproc[(SetWindowTitle) any?]{
Set title for window (only PLATFORM_DESKTOP)
}

@defproc[(SetWindowPosition) any?]{
Set window position on screen (only PLATFORM_DESKTOP)
}

@defproc[(SetWindowMonitor) any?]{
Set monitor for the current window (fullscreen mode)
}

@defproc[(SetWindowMinSize) any?]{
Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
}

@defproc[(SetWindowSize) any?]{
Set window dimensions
}

@defproc[(GetWindowHandle) any?]{
Get native window handle
}

@defproc[(GetScreenWidth) any?]{
Get current screen width
}

@defproc[(GetScreenHeight) any?]{
Get current screen height
}

@defproc[(GetRenderWidth) any?]{
Get current render width (it considers HiDPI)
}

@defproc[(GetRenderHeight) any?]{
Get current render height (it considers HiDPI)
}

@defproc[(GetMonitorCount) any?]{
Get number of connected monitors
}

@defproc[(GetCurrentMonitor) any?]{
Get current connected monitor
}

@defproc[(GetMonitorPosition) any?]{
Get specified monitor position
}

@defproc[(GetMonitorWidth) any?]{
Get specified monitor width (max available by monitor)
}

@defproc[(GetMonitorHeight) any?]{
Get specified monitor height (max available by monitor)
}

@defproc[(GetMonitorPhysicalWidth) any?]{
Get specified monitor physical width in millimetres
}

@defproc[(GetMonitorPhysicalHeight) any?]{
Get specified monitor physical height in millimetres
}

@defproc[(GetMonitorRefreshRate) any?]{
Get specified monitor refresh rate
}

@defproc[(GetWindowPosition) any?]{
Get window position XY on monitor
}

@defproc[(GetWindowScaleDPI) any?]{
Get window scale DPI factor
}

@defproc[(GetMonitorName) any?]{
Get the human-readable, UTF-8 encoded name of the primary monitor
}

@defproc[(SetClipboardText) any?]{
Set clipboard text content
}

@defproc[(GetClipboardText) any?]{
Get clipboard text content
}

@defproc[(SwapScreenBuffer) any?]{
Swap back buffer with front buffer (screen drawing)
}

@defproc[(PollInputEvents) any?]{
Register all input events
}

@defproc[(WaitTime) any?]{
Wait for some milliseconds (halt program execution)
}

@defproc[(ShowCursor) any?]{
Shows cursor
}

@defproc[(HideCursor) any?]{
Hides cursor
}

@defproc[(IsCursorHidden) any?]{
Check if cursor is not visible
}

@defproc[(EnableCursor) any?]{
Enables cursor (unlock cursor)
}

@defproc[(DisableCursor) any?]{
Disables cursor (lock cursor)
}

@defproc[(IsCursorOnScreen) any?]{
Check if cursor is on the screen
}

@defproc[(ClearBackground) any?]{
Set background color (framebuffer clear color)
}

@defproc[(BeginDrawing) any?]{
Setup canvas (framebuffer) to start drawing
}

@defproc[(EndDrawing) any?]{
End canvas drawing and swap buffers (double buffering)
}

@defproc[(BeginMode2D) any?]{
Begin 2D mode with custom camera (2D)
}

@defproc[(EndMode2D) any?]{
Ends 2D mode with custom camera
}

@defproc[(BeginMode3D) any?]{
Begin 3D mode with custom camera (3D)
}

@defproc[(EndMode3D) any?]{
Ends 3D mode and returns to default 2D orthographic mode
}

@defproc[(BeginTextureMode) any?]{
Begin drawing to render texture
}

@defproc[(EndTextureMode) any?]{
Ends drawing to render texture
}

@defproc[(BeginShaderMode) any?]{
Begin custom shader drawing
}

@defproc[(EndShaderMode) any?]{
End custom shader drawing (use default shader)
}

@defproc[(BeginBlendMode) any?]{
Begin blending mode (alpha, additive, multiplied, subtract, custom)
}

@defproc[(EndBlendMode) any?]{
End blending mode (reset to default: alpha blending)
}

@defproc[(BeginScissorMode) any?]{
Begin scissor mode (define screen area for following drawing)
}

@defproc[(EndScissorMode) any?]{
End scissor mode
}

@defproc[(BeginVrStereoMode) any?]{
Begin stereo rendering (requires VR simulator)
}

@defproc[(EndVrStereoMode) any?]{
End stereo rendering (requires VR simulator)
}

@defproc[(LoadVrStereoConfig) any?]{
Load VR stereo config for VR simulator device parameters
}

@defproc[(UnloadVrStereoConfig) any?]{
Unload VR stereo config
}

@defproc[(LoadShader) any?]{
Load shader from files and bind default locations
}

@defproc[(LoadShaderFromMemory) any?]{
Load shader from code strings and bind default locations
}

@defproc[(GetShaderLocation) any?]{
Get shader uniform location
}

@defproc[(GetShaderLocationAttrib) any?]{
Get shader attribute location
}

@defproc[(SetShaderValue) any?]{
Set shader uniform value
}

@defproc[(SetShaderValueV) any?]{
Set shader uniform value vector
}

@defproc[(SetShaderValueMatrix) any?]{
Set shader uniform value (matrix 4x4)
}

@defproc[(SetShaderValueTexture) any?]{
Set shader uniform value for texture (sampler2d)
}

@defproc[(UnloadShader) any?]{
Unload shader from GPU memory (VRAM)
}

@defproc[(GetMouseRay) any?]{
Get a ray trace from mouse position
}

@defproc[(GetCameraMatrix) any?]{
Get camera transform matrix (view matrix)
}

@defproc[(GetCameraMatrix2D) any?]{
Get camera 2d transform matrix
}

@defproc[(GetWorldToScreen) any?]{
Get the screen space position for a 3d world space position
}

@defproc[(GetWorldToScreenEx) any?]{
Get size position for a 3d world space position
}

@defproc[(GetWorldToScreen2D) any?]{
Get the screen space position for a 2d camera world space position
}

@defproc[(GetScreenToWorld2D) any?]{
Get the world space position for a 2d camera screen space position
}

@defproc[(SetTargetFPS) any?]{
Set target FPS (maximum)
}

@defproc[(GetFPS) any?]{
Get current FPS
}

@defproc[(GetFrameTime) any?]{
Get time in seconds for last frame drawn (delta time)
}

@defproc[(GetTime) any?]{
Get elapsed time in seconds since InitWindow()
}

@defproc[(GetRandomValue) any?]{
Get a random value between min and max (both included)
}

@defproc[(SetRandomSeed) any?]{
Set the seed for the random number generator
}

@defproc[(TakeScreenshot) any?]{
Takes a screenshot of current screen (filename extension defines format)
}

@defproc[(SetConfigFlags) any?]{
Setup init configuration flags (view FLAGS)
}

@defproc[(TraceLog) any?]{
Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR...)
}

@defproc[(SetTraceLogLevel) any?]{
Set the current threshold (minimum) log level
}

@defproc[(MemAlloc) any?]{
Internal memory allocator
}

@defproc[(MemRealloc) any?]{
Internal memory reallocator
}

@defproc[(MemFree) any?]{
Internal memory free
}

@defproc[(SetTraceLogCallback) any?]{
Set custom trace log
}

@defproc[(SetLoadFileDataCallback) any?]{
Set custom file binary data loader
}

@defproc[(SetSaveFileDataCallback) any?]{
Set custom file binary data saver
}

@defproc[(SetLoadFileTextCallback) any?]{
Set custom file text data loader
}

@defproc[(SetSaveFileTextCallback) any?]{
Set custom file text data saver
}

@defproc[(LoadFileData) any?]{
Load file data as byte array (read)
}

@defproc[(UnloadFileData) any?]{
Unload file data allocated by LoadFileData()
}

@defproc[(SaveFileData) any?]{
Save data to file from byte array (write), returns true on success
}

@defproc[(LoadFileText) any?]{
Load text data from file (read), returns a '\0' terminated string
}

@defproc[(UnloadFileText) any?]{
Unload file text data allocated by LoadFileText()
}

@defproc[(SaveFileText) any?]{
Save text data to file (write), string must be '\0' terminated, returns true on success
}

@defproc[(FileExists) any?]{
Check if file exists
}

@defproc[(DirectoryExists) any?]{
Check if a directory path exists
}

@defproc[(IsFileExtension) any?]{
Check file extension (including point: .png, .wav)
}

@defproc[(GetFileExtension) any?]{
Get pointer to extension for a filename string (includes dot: '.png')
}

@defproc[(GetFileName) any?]{
Get pointer to filename for a path string
}

@defproc[(GetFileNameWithoutExt) any?]{
Get filename string without extension (uses static string)
}

@defproc[(GetDirectoryPath) any?]{
Get full path for a given fileName with path (uses static string)
}

@defproc[(GetPrevDirectoryPath) any?]{
Get previous directory path for a given path (uses static string)
}

@defproc[(GetWorkingDirectory) any?]{
Get current working directory (uses static string)
}

@defproc[(GetDirectoryFiles) any?]{
Get filenames in a directory path (memory should be freed)
}

@defproc[(ClearDirectoryFiles) any?]{
Clear directory files paths buffers (free memory)
}

@defproc[(ChangeDirectory) any?]{
Change working directory, return true on success
}

@defproc[(IsFileDropped) any?]{
Check if a file has been dropped into window
}

@defproc[(GetDroppedFiles) any?]{
Get dropped files names (memory should be freed)
}

@defproc[(ClearDroppedFiles) any?]{
Clear dropped files paths buffer (free memory)
}

@defproc[(GetFileModTime) any?]{
Get file modification time (last write time)
}

@defproc[(CompressData) any?]{
Compress data (DEFLATE algorithm)
}

@defproc[(DecompressData) any?]{
Decompress data (DEFLATE algorithm)
}

@defproc[(EncodeDataBase64) any?]{
Encode data to Base64 string
}

@defproc[(DecodeDataBase64) any?]{
Decode Base64 string data
}

@defproc[(SaveStorageValue) any?]{
Save integer value to storage file (to defined position), returns true on success
}

@defproc[(LoadStorageValue) any?]{
Load integer value from storage file (from defined position)
}

@defproc[(OpenURL) any?]{
Open URL with default system browser (if available)
}

@defproc[(IsKeyPressed) any?]{
Check if a key has been pressed once
}

@defproc[(IsKeyDown) any?]{
Check if a key is being pressed
}

@defproc[(IsKeyReleased) any?]{
Check if a key has been released once
}

@defproc[(IsKeyUp) any?]{
Check if a key is NOT being pressed
}

@defproc[(SetExitKey) any?]{
Set a custom key to exit program (default is ESC)
}

@defproc[(GetKeyPressed) any?]{
Get key pressed (keycode), call it multiple times for keys queued, returns 0 when the queue is empty
}

@defproc[(GetCharPressed) any?]{
Get char pressed (unicode), call it multiple times for chars queued, returns 0 when the queue is empty
}

@defproc[(IsGamepadAvailable) any?]{
Check if a gamepad is available
}

@defproc[(GetGamepadName) any?]{
Get gamepad internal name id
}

@defproc[(IsGamepadButtonPressed) any?]{
Check if a gamepad button has been pressed once
}

@defproc[(IsGamepadButtonDown) any?]{
Check if a gamepad button is being pressed
}

@defproc[(IsGamepadButtonReleased) any?]{
Check if a gamepad button has been released once
}

@defproc[(IsGamepadButtonUp) any?]{
Check if a gamepad button is NOT being pressed
}

@defproc[(GetGamepadButtonPressed) any?]{
Get the last gamepad button pressed
}

@defproc[(GetGamepadAxisCount) any?]{
Get gamepad axis count for a gamepad
}

@defproc[(GetGamepadAxisMovement) any?]{
Get axis movement value for a gamepad axis
}

@defproc[(SetGamepadMappings) any?]{
Set internal gamepad mappings (SDL_GameControllerDB)
}

@defproc[(IsMouseButtonPressed) any?]{
Check if a mouse button has been pressed once
}

@defproc[(IsMouseButtonDown) any?]{
Check if a mouse button is being pressed
}

@defproc[(IsMouseButtonReleased) any?]{
Check if a mouse button has been released once
}

@defproc[(IsMouseButtonUp) any?]{
Check if a mouse button is NOT being pressed
}

@defproc[(GetMouseX) any?]{
Get mouse position X
}

@defproc[(GetMouseY) any?]{
Get mouse position Y
}

@defproc[(GetMousePosition) any?]{
Get mouse position XY
}

@defproc[(GetMouseDelta) any?]{
Get mouse delta between frames
}

@defproc[(SetMousePosition) any?]{
Set mouse position XY
}

@defproc[(SetMouseOffset) any?]{
Set mouse offset
}

@defproc[(SetMouseScale) any?]{
Set mouse scaling
}

@defproc[(GetMouseWheelMove) any?]{
Get mouse wheel movement Y
}

@defproc[(SetMouseCursor) any?]{
Set mouse cursor
}

@defproc[(GetTouchX) any?]{
Get touch position X for touch point 0 (relative to screen size)
}

@defproc[(GetTouchY) any?]{
Get touch position Y for touch point 0 (relative to screen size)
}

@defproc[(GetTouchPosition) any?]{
Get touch position XY for a touch point index (relative to screen size)
}

@defproc[(GetTouchPointId) any?]{
Get touch point identifier for given index
}

@defproc[(GetTouchPointCount) any?]{
Get number of touch points
}

@defproc[(SetGesturesEnabled) any?]{
Enable a set of gestures using flags
}

@defproc[(IsGestureDetected) any?]{
Check if a gesture have been detected
}

@defproc[(GetGestureDetected) any?]{
Get latest detected gesture
}

@defproc[(GetGestureHoldDuration) any?]{
Get gesture hold time in milliseconds
}

@defproc[(GetGestureDragVector) any?]{
Get gesture drag vector
}

@defproc[(GetGestureDragAngle) any?]{
Get gesture drag angle
}

@defproc[(GetGesturePinchVector) any?]{
Get gesture pinch delta
}

@defproc[(GetGesturePinchAngle) any?]{
Get gesture pinch angle
}

@defproc[(SetCameraMode) any?]{
Set camera mode (multiple camera modes available)
}

@defproc[(UpdateCamera) any?]{
Update camera position for selected mode
}

@defproc[(SetCameraPanControl) any?]{
Set camera pan key to combine with mouse movement (free camera)
}

@defproc[(SetCameraAltControl) any?]{
Set camera alt key to combine with mouse movement (free camera)
}

@defproc[(SetCameraSmoothZoomControl) any?]{
Set camera smooth zoom key to combine with mouse (free camera)
}

@defproc[(SetCameraMoveControls) any?]{
Set camera move controls (1st person and 3rd person cameras)
}

@defproc[(SetShapesTexture) any?]{
Set texture and rectangle to be used on shapes drawing
}

@defproc[(DrawPixel) any?]{
Draw a pixel
}

@defproc[(DrawPixelV) any?]{
Draw a pixel (Vector version)
}

@defproc[(DrawLine) any?]{
Draw a line
}

@defproc[(DrawLineV) any?]{
Draw a line (Vector version)
}

@defproc[(DrawLineEx) any?]{
Draw a line defining thickness
}

@defproc[(DrawLineBezier) any?]{
Draw a line using cubic-bezier curves in-out
}

@defproc[(DrawLineBezierQuad) any?]{
Draw line using quadratic bezier curves with a control point
}

@defproc[(DrawLineBezierCubic) any?]{
Draw line using cubic bezier curves with 2 control points
}

@defproc[(DrawLineStrip) any?]{
Draw lines sequence
}

@defproc[(DrawCircle) any?]{
Draw a color-filled circle
}

@defproc[(DrawCircleSector) any?]{
Draw a piece of a circle
}

@defproc[(DrawCircleSectorLines) any?]{
Draw circle sector outline
}

@defproc[(DrawCircleGradient) any?]{
Draw a gradient-filled circle
}

@defproc[(DrawCircleV) any?]{
Draw a color-filled circle (Vector version)
}

@defproc[(DrawCircleLines) any?]{
Draw circle outline
}

@defproc[(DrawEllipse) any?]{
Draw ellipse
}

@defproc[(DrawEllipseLines) any?]{
Draw ellipse outline
}

@defproc[(DrawRing) any?]{
Draw ring
}

@defproc[(DrawRingLines) any?]{
Draw ring outline
}

@defproc[(DrawRectangle) any?]{
Draw a color-filled rectangle
}

@defproc[(DrawRectangleV) any?]{
Draw a color-filled rectangle (Vector version)
}

@defproc[(DrawRectangleRec) any?]{
Draw a color-filled rectangle
}

@defproc[(DrawRectanglePro) any?]{
Draw a color-filled rectangle with pro parameters
}

@defproc[(DrawRectangleGradientV) any?]{
Draw a vertical-gradient-filled rectangle
}

@defproc[(DrawRectangleGradientH) any?]{
Draw a horizontal-gradient-filled rectangle
}

@defproc[(DrawRectangleGradientEx) any?]{
Draw a gradient-filled rectangle with custom vertex colors
}

@defproc[(DrawRectangleLines) any?]{
Draw rectangle outline
}

@defproc[(DrawRectangleLinesEx) any?]{
Draw rectangle outline with extended parameters
}

@defproc[(DrawRectangleRounded) any?]{
Draw rectangle with rounded edges
}

@defproc[(DrawRectangleRoundedLines) any?]{
Draw rectangle with rounded edges outline
}

@defproc[(DrawTriangle) any?]{
Draw a color-filled triangle (vertex in counter-clockwise order!)
}

@defproc[(DrawTriangleLines) any?]{
Draw triangle outline (vertex in counter-clockwise order!)
}

@defproc[(DrawTriangleFan) any?]{
Draw a triangle fan defined by points (first vertex is the center)
}

@defproc[(DrawTriangleStrip) any?]{
Draw a triangle strip defined by points
}

@defproc[(DrawPoly) any?]{
Draw a regular polygon (Vector version)
}

@defproc[(DrawPolyLines) any?]{
Draw a polygon outline of n sides
}

@defproc[(DrawPolyLinesEx) any?]{
Draw a polygon outline of n sides with extended parameters
}

@defproc[(CheckCollisionRecs) any?]{
Check collision between two rectangles
}

@defproc[(CheckCollisionCircles) any?]{
Check collision between two circles
}

@defproc[(CheckCollisionCircleRec) any?]{
Check collision between circle and rectangle
}

@defproc[(CheckCollisionPointRec) any?]{
Check if point is inside rectangle
}

@defproc[(CheckCollisionPointCircle) any?]{
Check if point is inside circle
}

@defproc[(CheckCollisionPointTriangle) any?]{
Check if point is inside a triangle
}

@defproc[(CheckCollisionLines) any?]{
Check the collision between two lines defined by two points each, returns collision point by reference
}

@defproc[(CheckCollisionPointLine) any?]{
Check if point belongs to line created between two points [p1] and [p2] with defined margin in pixels [threshold]
}

@defproc[(GetCollisionRec) any?]{
Get collision rectangle for two rectangles collision
}

@defproc[(LoadImage) any?]{
Load image from file into CPU memory (RAM)
}

@defproc[(LoadImageRaw) any?]{
Load image from RAW file data
}

@defproc[(LoadImageAnim) any?]{
Load image sequence from file (frames appended to image.data)
}

@defproc[(LoadImageFromMemory) any?]{
Load image from memory buffer, fileType refers to extension: i.e. '.png'
}

@defproc[(LoadImageFromTexture) any?]{
Load image from GPU texture data
}

@defproc[(LoadImageFromScreen) any?]{
Load image from screen buffer and (screenshot)
}

@defproc[(UnloadImage) any?]{
Unload image from CPU memory (RAM)
}

@defproc[(ExportImage) any?]{
Export image data to file, returns true on success
}

@defproc[(ExportImageAsCode) any?]{
Export image as code file defining an array of bytes, returns true on success
}

@defproc[(GenImageColor) any?]{
Generate image: plain color
}

@defproc[(GenImageGradientV) any?]{
Generate image: vertical gradient
}

@defproc[(GenImageGradientH) any?]{
Generate image: horizontal gradient
}

@defproc[(GenImageGradientRadial) any?]{
Generate image: radial gradient
}

@defproc[(GenImageChecked) any?]{
Generate image: checked
}

@defproc[(GenImageWhiteNoise) any?]{
Generate image: white noise
}

@defproc[(GenImageCellular) any?]{
Generate image: cellular algorithm, bigger tileSize means bigger cells
}

@defproc[(ImageCopy) any?]{
Create an image duplicate (useful for transformations)
}

@defproc[(ImageFromImage) any?]{
Create an image from another image piece
}

@defproc[(ImageText) any?]{
Create an image from text (default font)
}

@defproc[(ImageTextEx) any?]{
Create an image from text (custom sprite font)
}

@defproc[(ImageFormat) any?]{
Convert image data to desired format
}

@defproc[(ImageToPOT) any?]{
Convert image to POT (power-of-two)
}

@defproc[(ImageCrop) any?]{
Crop an image to a defined rectangle
}

@defproc[(ImageAlphaCrop) any?]{
Crop image depending on alpha value
}

@defproc[(ImageAlphaClear) any?]{
Clear alpha channel to desired color
}

@defproc[(ImageAlphaMask) any?]{
Apply alpha mask to image
}

@defproc[(ImageAlphaPremultiply) any?]{
Premultiply alpha channel
}

@defproc[(ImageResize) any?]{
Resize image (Bicubic scaling algorithm)
}

@defproc[(ImageResizeNN) any?]{
Resize image (Nearest-Neighbor scaling algorithm)
}

@defproc[(ImageResizeCanvas) any?]{
Resize canvas and fill with color
}

@defproc[(ImageMipmaps) any?]{
Compute all mipmap levels for a provided image
}

@defproc[(ImageDither) any?]{
Dither image data to 16bpp or lower (Floyd-Steinberg dithering)
}

@defproc[(ImageFlipVertical) any?]{
Flip image vertically
}

@defproc[(ImageFlipHorizontal) any?]{
Flip image horizontally
}

@defproc[(ImageRotateCW) any?]{
Rotate image clockwise 90deg
}

@defproc[(ImageRotateCCW) any?]{
Rotate image counter-clockwise 90deg
}

@defproc[(ImageColorTint) any?]{
Modify image color: tint
}

@defproc[(ImageColorInvert) any?]{
Modify image color: invert
}

@defproc[(ImageColorGrayscale) any?]{
Modify image color: grayscale
}

@defproc[(ImageColorContrast) any?]{
Modify image color: contrast (-100 to 100)
}

@defproc[(ImageColorBrightness) any?]{
Modify image color: brightness (-255 to 255)
}

@defproc[(ImageColorReplace) any?]{
Modify image color: replace color
}

@defproc[(LoadImageColors) any?]{
Load color data from image as a Color array (RGBA - 32bit)
}

@defproc[(LoadImagePalette) any?]{
Load colors palette from image as a Color array (RGBA - 32bit)
}

@defproc[(UnloadImageColors) any?]{
Unload color data loaded with LoadImageColors()
}

@defproc[(UnloadImagePalette) any?]{
Unload colors palette loaded with LoadImagePalette()
}

@defproc[(GetImageAlphaBorder) any?]{
Get image alpha border rectangle
}

@defproc[(GetImageColor) any?]{
Get image pixel color at (x, y) position
}

@defproc[(ImageClearBackground) any?]{
Clear image background with given color
}

@defproc[(ImageDrawPixel) any?]{
Draw pixel within an image
}

@defproc[(ImageDrawPixelV) any?]{
Draw pixel within an image (Vector version)
}

@defproc[(ImageDrawLine) any?]{
Draw line within an image
}

@defproc[(ImageDrawLineV) any?]{
Draw line within an image (Vector version)
}

@defproc[(ImageDrawCircle) any?]{
Draw circle within an image
}

@defproc[(ImageDrawCircleV) any?]{
Draw circle within an image (Vector version)
}

@defproc[(ImageDrawRectangle) any?]{
Draw rectangle within an image
}

@defproc[(ImageDrawRectangleV) any?]{
Draw rectangle within an image (Vector version)
}

@defproc[(ImageDrawRectangleRec) any?]{
Draw rectangle within an image
}

@defproc[(ImageDrawRectangleLines) any?]{
Draw rectangle lines within an image
}

@defproc[(ImageDraw) any?]{
Draw a source image within a destination image (tint applied to source)
}

@defproc[(ImageDrawText) any?]{
Draw text (using default font) within an image (destination)
}

@defproc[(ImageDrawTextEx) any?]{
Draw text (custom sprite font) within an image (destination)
}

@defproc[(LoadTexture) any?]{
Load texture from file into GPU memory (VRAM)
}

@defproc[(LoadTextureFromImage) any?]{
Load texture from image data
}

@defproc[(LoadTextureCubemap) any?]{
Load cubemap from image, multiple image cubemap layouts supported
}

@defproc[(LoadRenderTexture) any?]{
Load texture for rendering (framebuffer)
}

@defproc[(UnloadTexture) any?]{
Unload texture from GPU memory (VRAM)
}

@defproc[(UnloadRenderTexture) any?]{
Unload render texture from GPU memory (VRAM)
}

@defproc[(UpdateTexture) any?]{
Update GPU texture with new data
}

@defproc[(UpdateTextureRec) any?]{
Update GPU texture rectangle with new data
}

@defproc[(GenTextureMipmaps) any?]{
Generate GPU mipmaps for a texture
}

@defproc[(SetTextureFilter) any?]{
Set texture scaling filter mode
}

@defproc[(SetTextureWrap) any?]{
Set texture wrapping mode
}

@defproc[(DrawTexture) any?]{
Draw a Texture2D
}

@defproc[(DrawTextureV) any?]{
Draw a Texture2D with position defined as Vector2
}

@defproc[(DrawTextureEx) any?]{
Draw a Texture2D with extended parameters
}

@defproc[(DrawTextureRec) any?]{
Draw a part of a texture defined by a rectangle
}

@defproc[(DrawTextureQuad) any?]{
Draw texture quad with tiling and offset parameters
}

@defproc[(DrawTextureTiled) any?]{
Draw part of a texture (defined by a rectangle) with rotation and scale tiled into dest.
}

@defproc[(DrawTexturePro) any?]{
Draw a part of a texture defined by a rectangle with 'pro' parameters
}

@defproc[(DrawTextureNPatch) any?]{
Draws a texture (or part of it) that stretches or shrinks nicely
}

@defproc[(DrawTexturePoly) any?]{
Draw a textured polygon
}

@defproc[(Fade) any?]{
Get color with alpha applied, alpha goes from 0.0f to 1.0f
}

@defproc[(ColorToInt) any?]{
Get hexadecimal value for a Color
}

@defproc[(ColorNormalize) any?]{
Get Color normalized as float [0..1]
}

@defproc[(ColorFromNormalized) any?]{
Get Color from normalized values [0..1]
}

@defproc[(ColorToHSV) any?]{
Get HSV values for a Color, hue [0..360], saturation/value [0..1]
}

@defproc[(ColorFromHSV) any?]{
Get a Color from HSV values, hue [0..360], saturation/value [0..1]
}

@defproc[(ColorAlpha) any?]{
Get color with alpha applied, alpha goes from 0.0f to 1.0f
}

@defproc[(ColorAlphaBlend) any?]{
Get src alpha-blended into dst color with tint
}

@defproc[(GetColor) any?]{
Get Color structure from hexadecimal value
}

@defproc[(GetPixelColor) any?]{
Get Color from a source pixel pointer of certain format
}

@defproc[(SetPixelColor) any?]{
Set color formatted into destination pixel pointer
}

@defproc[(GetPixelDataSize) any?]{
Get pixel data size in bytes for certain format
}

@defproc[(GetFontDefault) any?]{
Get the default Font
}

@defproc[(LoadFont) any?]{
Load font from file into GPU memory (VRAM)
}

@defproc[(LoadFontEx) any?]{
Load font from file with extended parameters
}

@defproc[(LoadFontFromImage) any?]{
Load font from Image (XNA style)
}

@defproc[(LoadFontFromMemory) any?]{
Load font from memory buffer, fileType refers to extension: i.e. '.ttf'
}

@defproc[(LoadFontData) any?]{
Load font data for further use
}

@defproc[(GenImageFontAtlas) any?]{
Generate image font atlas using chars info
}

@defproc[(UnloadFontData) any?]{
Unload font chars info data (RAM)
}

@defproc[(UnloadFont) any?]{
Unload Font from GPU memory (VRAM)
}

@defproc[(DrawFPS) any?]{
Draw current FPS
}

@defproc[(DrawText) any?]{
Draw text (using default font)
}

@defproc[(DrawTextEx) any?]{
Draw text using font and additional parameters
}

@defproc[(DrawTextPro) any?]{
Draw text using Font and pro parameters (rotation)
}

@defproc[(DrawTextCodepoint) any?]{
Draw one character (codepoint)
}

@defproc[(MeasureText) any?]{
Measure string width for default font
}

@defproc[(MeasureTextEx) any?]{
Measure string size for Font
}

@defproc[(GetGlyphIndex) any?]{
Get glyph index position in font for a codepoint (unicode character), fallback to '?' if not found
}

@defproc[(GetGlyphInfo) any?]{
Get glyph font info data for a codepoint (unicode character), fallback to '?' if not found
}

@defproc[(GetGlyphAtlasRec) any?]{
Get glyph rectangle in font atlas for a codepoint (unicode character), fallback to '?' if not found
}

@defproc[(LoadCodepoints) any?]{
Load all codepoints from a UTF-8 text string, codepoints count returned by parameter
}

@defproc[(UnloadCodepoints) any?]{
Unload codepoints data from memory
}

@defproc[(GetCodepointCount) any?]{
Get total number of codepoints in a UTF-8 encoded string
}

@defproc[(GetCodepoint) any?]{
Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
}

@defproc[(CodepointToUTF8) any?]{
Encode one codepoint into UTF-8 byte array (array length returned as parameter)
}

@defproc[(TextCodepointsToUTF8) any?]{
Encode text as codepoints array into UTF-8 text string (WARNING: memory must be freed!)
}

@defproc[(TextCopy) any?]{
Copy one string to another, returns bytes copied
}

@defproc[(TextIsEqual) any?]{
Check if two text string are equal
}

@defproc[(TextLength) any?]{
Get text length, checks for '\0' ending
}

@defproc[(TextFormat) any?]{
Text formatting with variables (sprintf() style)
}

@defproc[(TextSubtext) any?]{
Get a piece of a text string
}

@defproc[(TextReplace) any?]{
Replace text string (WARNING: memory must be freed!)
}

@defproc[(TextInsert) any?]{
Insert text in a position (WARNING: memory must be freed!)
}

@defproc[(TextJoin) any?]{
Join text strings with delimiter
}

@defproc[(TextSplit) any?]{
Split text into multiple strings
}

@defproc[(TextAppend) any?]{
Append text at specific position and move cursor!
}

@defproc[(TextFindIndex) any?]{
Find first text occurrence within a string
}

@defproc[(TextToUpper) any?]{
Get upper case version of provided string
}

@defproc[(TextToLower) any?]{
Get lower case version of provided string
}

@defproc[(TextToPascal) any?]{
Get Pascal case notation version of provided string
}

@defproc[(TextToInteger) any?]{
Get integer value from text (negative values not supported)
}

@defproc[(DrawLine3D) any?]{
Draw a line in 3D world space
}

@defproc[(DrawPoint3D) any?]{
Draw a point in 3D space, actually a small line
}

@defproc[(DrawCircle3D) any?]{
Draw a circle in 3D world space
}

@defproc[(DrawTriangle3D) any?]{
Draw a color-filled triangle (vertex in counter-clockwise order!)
}

@defproc[(DrawTriangleStrip3D) any?]{
Draw a triangle strip defined by points
}

@defproc[(DrawCube) any?]{
Draw cube
}

@defproc[(DrawCubeV) any?]{
Draw cube (Vector version)
}

@defproc[(DrawCubeWires) any?]{
Draw cube wires
}

@defproc[(DrawCubeWiresV) any?]{
Draw cube wires (Vector version)
}

@defproc[(DrawCubeTexture) any?]{
Draw cube textured
}

@defproc[(DrawCubeTextureRec) any?]{
Draw cube with a region of a texture
}

@defproc[(DrawSphere) any?]{
Draw sphere
}

@defproc[(DrawSphereEx) any?]{
Draw sphere with extended parameters
}

@defproc[(DrawSphereWires) any?]{
Draw sphere wires
}

@defproc[(DrawCylinder) any?]{
Draw a cylinder/cone
}

@defproc[(DrawCylinderEx) any?]{
Draw a cylinder with base at startPos and top at endPos
}

@defproc[(DrawCylinderWires) any?]{
Draw a cylinder/cone wires
}

@defproc[(DrawCylinderWiresEx) any?]{
Draw a cylinder wires with base at startPos and top at endPos
}

@defproc[(DrawPlane) any?]{
Draw a plane XZ
}

@defproc[(DrawRay) any?]{
Draw a ray line
}

@defproc[(DrawGrid) any?]{
Draw a grid (centered at (0, 0, 0))
}

@defproc[(LoadModel) any?]{
Load model from files (meshes and materials)
}

@defproc[(LoadModelFromMesh) any?]{
Load model from generated mesh (default material)
}

@defproc[(UnloadModel) any?]{
Unload model (including meshes) from memory (RAM and/or VRAM)
}

@defproc[(UnloadModelKeepMeshes) any?]{
Unload model (but not meshes) from memory (RAM and/or VRAM)
}

@defproc[(GetModelBoundingBox) any?]{
Compute model bounding box limits (considers all meshes)
}

@defproc[(DrawModel) any?]{
Draw a model (with texture if set)
}

@defproc[(DrawModelEx) any?]{
Draw a model with extended parameters
}

@defproc[(DrawModelWires) any?]{
Draw a model wires (with texture if set)
}

@defproc[(DrawModelWiresEx) any?]{
Draw a model wires (with texture if set) with extended parameters
}

@defproc[(DrawBoundingBox) any?]{
Draw bounding box (wires)
}

@defproc[(DrawBillboard) any?]{
Draw a billboard texture
}

@defproc[(DrawBillboardRec) any?]{
Draw a billboard texture defined by source
}

@defproc[(DrawBillboardPro) any?]{
Draw a billboard texture defined by source and rotation
}

@defproc[(UploadMesh) any?]{
Upload mesh vertex data in GPU and provide VAO/VBO ids
}

@defproc[(UpdateMeshBuffer) any?]{
Update mesh vertex data in GPU for a specific buffer index
}

@defproc[(UnloadMesh) any?]{
Unload mesh data from CPU and GPU
}

@defproc[(DrawMesh) any?]{
Draw a 3d mesh with material and transform
}

@defproc[(DrawMeshInstanced) any?]{
Draw multiple mesh instances with material and different transforms
}

@defproc[(ExportMesh) any?]{
Export mesh data to file, returns true on success
}

@defproc[(GetMeshBoundingBox) any?]{
Compute mesh bounding box limits
}

@defproc[(GenMeshTangents) any?]{
Compute mesh tangents
}

@defproc[(GenMeshBinormals) any?]{
Compute mesh binormals
}

@defproc[(GenMeshPoly) any?]{
Generate polygonal mesh
}

@defproc[(GenMeshPlane) any?]{
Generate plane mesh (with subdivisions)
}

@defproc[(GenMeshCube) any?]{
Generate cuboid mesh
}

@defproc[(GenMeshSphere) any?]{
Generate sphere mesh (standard sphere)
}

@defproc[(GenMeshHemiSphere) any?]{
Generate half-sphere mesh (no bottom cap)
}

@defproc[(GenMeshCylinder) any?]{
Generate cylinder mesh
}

@defproc[(GenMeshCone) any?]{
Generate cone/pyramid mesh
}

@defproc[(GenMeshTorus) any?]{
Generate torus mesh
}

@defproc[(GenMeshKnot) any?]{
Generate trefoil knot mesh
}

@defproc[(GenMeshHeightmap) any?]{
Generate heightmap mesh from image data
}

@defproc[(GenMeshCubicmap) any?]{
Generate cubes-based map mesh from image data
}

@defproc[(LoadMaterials) any?]{
Load materials from model file
}

@defproc[(LoadMaterialDefault) any?]{
Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)
}

@defproc[(UnloadMaterial) any?]{
Unload material from GPU memory (VRAM)
}

@defproc[(SetMaterialTexture) any?]{
Set texture for a material map type (MATERIAL_MAP_DIFFUSE, MATERIAL_MAP_SPECULAR...)
}

@defproc[(SetModelMeshMaterial) any?]{
Set material for a mesh
}

@defproc[(LoadModelAnimations) any?]{
Load model animations from file
}

@defproc[(UpdateModelAnimation) any?]{
Update model animation pose
}

@defproc[(UnloadModelAnimation) any?]{
Unload animation data
}

@defproc[(UnloadModelAnimations) any?]{
Unload animation array data
}

@defproc[(IsModelAnimationValid) any?]{
Check model animation skeleton match
}

@defproc[(CheckCollisionSpheres) any?]{
Check collision between two spheres
}

@defproc[(CheckCollisionBoxes) any?]{
Check collision between two bounding boxes
}

@defproc[(CheckCollisionBoxSphere) any?]{
Check collision between box and sphere
}

@defproc[(GetRayCollisionSphere) any?]{
Get collision info between ray and sphere
}

@defproc[(GetRayCollisionBox) any?]{
Get collision info between ray and box
}

@defproc[(GetRayCollisionModel) any?]{
Get collision info between ray and model
}

@defproc[(GetRayCollisionMesh) any?]{
Get collision info between ray and mesh
}

@defproc[(GetRayCollisionTriangle) any?]{
Get collision info between ray and triangle
}

@defproc[(GetRayCollisionQuad) any?]{
Get collision info between ray and quad
}

@defproc[(InitAudioDevice) any?]{
Initialize audio device and context
}

@defproc[(CloseAudioDevice) any?]{
Close the audio device and context
}

@defproc[(IsAudioDeviceReady) any?]{
Check if audio device has been initialized successfully
}

@defproc[(SetMasterVolume) any?]{
Set master volume (listener)
}

@defproc[(LoadWave) any?]{
Load wave data from file
}

@defproc[(LoadWaveFromMemory) any?]{
Load wave from memory buffer, fileType refers to extension: i.e. '.wav'
}

@defproc[(LoadSound) any?]{
Load sound from file
}

@defproc[(LoadSoundFromWave) any?]{
Load sound from wave data
}

@defproc[(UpdateSound) any?]{
Update sound buffer with new data
}

@defproc[(UnloadWave) any?]{
Unload wave data
}

@defproc[(UnloadSound) any?]{
Unload sound
}

@defproc[(ExportWave) any?]{
Export wave data to file, returns true on success
}

@defproc[(ExportWaveAsCode) any?]{
Export wave sample data to code (.h), returns true on success
}

@defproc[(PlaySound) any?]{
Play a sound
}

@defproc[(StopSound) any?]{
Stop playing a sound
}

@defproc[(PauseSound) any?]{
Pause a sound
}

@defproc[(ResumeSound) any?]{
Resume a paused sound
}

@defproc[(PlaySoundMulti) any?]{
Play a sound (using multichannel buffer pool)
}

@defproc[(StopSoundMulti) any?]{
Stop any sound playing (using multichannel buffer pool)
}

@defproc[(GetSoundsPlaying) any?]{
Get number of sounds playing in the multichannel
}

@defproc[(IsSoundPlaying) any?]{
Check if a sound is currently playing
}

@defproc[(SetSoundVolume) any?]{
Set volume for a sound (1.0 is max level)
}

@defproc[(SetSoundPitch) any?]{
Set pitch for a sound (1.0 is base level)
}

@defproc[(WaveFormat) any?]{
Convert wave data to desired format
}

@defproc[(WaveCopy) any?]{
Copy a wave to a new wave
}

@defproc[(WaveCrop) any?]{
Crop a wave to defined samples range
}

@defproc[(LoadWaveSamples) any?]{
Load samples data from wave as a floats array
}

@defproc[(UnloadWaveSamples) any?]{
Unload samples data loaded with LoadWaveSamples()
}

@defproc[(LoadMusicStream) any?]{
Load music stream from file
}

@defproc[(LoadMusicStreamFromMemory) any?]{
Load music stream from data
}

@defproc[(UnloadMusicStream) any?]{
Unload music stream
}

@defproc[(PlayMusicStream) any?]{
Start music playing
}

@defproc[(IsMusicStreamPlaying) any?]{
Check if music is playing
}

@defproc[(UpdateMusicStream) any?]{
Updates buffers for music streaming
}

@defproc[(StopMusicStream) any?]{
Stop music playing
}

@defproc[(PauseMusicStream) any?]{
Pause music playing
}

@defproc[(ResumeMusicStream) any?]{
Resume playing paused music
}

@defproc[(SeekMusicStream) any?]{
Seek music to a position (in seconds)
}

@defproc[(SetMusicVolume) any?]{
Set volume for music (1.0 is max level)
}

@defproc[(SetMusicPitch) any?]{
Set pitch for a music (1.0 is base level)
}

@defproc[(GetMusicTimeLength) any?]{
Get music time length (in seconds)
}

@defproc[(GetMusicTimePlayed) any?]{
Get current music time played (in seconds)
}

@defproc[(LoadAudioStream) any?]{
Load audio stream (to stream raw audio pcm data)
}

@defproc[(UnloadAudioStream) any?]{
Unload audio stream and free memory
}

@defproc[(UpdateAudioStream) any?]{
Update audio stream buffers with data
}

@defproc[(IsAudioStreamProcessed) any?]{
Check if any audio stream buffers requires refill
}

@defproc[(PlayAudioStream) any?]{
Play audio stream
}

@defproc[(PauseAudioStream) any?]{
Pause audio stream
}

@defproc[(ResumeAudioStream) any?]{
Resume audio stream
}

@defproc[(IsAudioStreamPlaying) any?]{
Check if audio stream is playing
}

@defproc[(StopAudioStream) any?]{
Stop audio stream
}

@defproc[(SetAudioStreamVolume) any?]{
Set volume for audio stream (1.0 is max level)
}

@defproc[(SetAudioStreamPitch) any?]{
Set pitch for audio stream (1.0 is base level)
}

@defproc[(SetAudioStreamBufferSizeDefault) any?]{
Default size for new audio streams
}
