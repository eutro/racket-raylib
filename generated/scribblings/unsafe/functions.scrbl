#lang scribble/manual

@(require (for-label raylib/generated/unsafe/functions raylib/generated/structs raylib/support ffi/unsafe racket/base))

@title{Functions}

@defmodule[raylib/generated/unsafe/functions]

@table-of-contents[]


@defproc[(InitWindow
          [width _int]
          [height _int]
          [title _string])
         _void]{
Initialize window and OpenGL context
}

@defproc[(CloseWindow)
         _void]{
Close window and unload OpenGL context
}

@defproc[(WindowShouldClose)
         _stdbool]{
Check if application should close (KEY_ESCAPE pressed or windows close icon clicked)
}

@defproc[(IsWindowReady)
         _stdbool]{
Check if window has been initialized successfully
}

@defproc[(IsWindowFullscreen)
         _stdbool]{
Check if window is currently fullscreen
}

@defproc[(IsWindowHidden)
         _stdbool]{
Check if window is currently hidden
}

@defproc[(IsWindowMinimized)
         _stdbool]{
Check if window is currently minimized
}

@defproc[(IsWindowMaximized)
         _stdbool]{
Check if window is currently maximized
}

@defproc[(IsWindowFocused)
         _stdbool]{
Check if window is currently focused
}

@defproc[(IsWindowResized)
         _stdbool]{
Check if window has been resized last frame
}

@defproc[(IsWindowState
          [flag _uint])
         _stdbool]{
Check if one specific window flag is enabled
}

@defproc[(SetWindowState
          [flags _uint])
         _void]{
Set window configuration state using flags
}

@defproc[(ClearWindowState
          [flags _uint])
         _void]{
Clear window configuration state flags
}

@defproc[(ToggleFullscreen)
         _void]{
Toggle window state: fullscreen/windowed, resizes monitor to match window resolution
}

@defproc[(ToggleBorderlessWindowed)
         _void]{
Toggle window state: borderless windowed, resizes window to match monitor resolution
}

@defproc[(MaximizeWindow)
         _void]{
Set window state: maximized, if resizable
}

@defproc[(MinimizeWindow)
         _void]{
Set window state: minimized, if resizable
}

@defproc[(RestoreWindow)
         _void]{
Set window state: not minimized/maximized
}

@defproc[(SetWindowIcon
          [image _Image])
         _void]{
Set icon for window (single image, RGBA 32bit)
}

@defproc[(SetWindowIcons
          [images (_pointer-to _Image)]
          [count _int])
         _void]{
Set icon for window (multiple images, RGBA 32bit)
}

@defproc[(SetWindowTitle
          [title _string])
         _void]{
Set title for window
}

@defproc[(SetWindowPosition
          [x _int]
          [y _int])
         _void]{
Set window position on screen
}

@defproc[(SetWindowMonitor
          [monitor _int])
         _void]{
Set monitor for the current window
}

@defproc[(SetWindowMinSize
          [width _int]
          [height _int])
         _void]{
Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
}

@defproc[(SetWindowMaxSize
          [width _int]
          [height _int])
         _void]{
Set window maximum dimensions (for FLAG_WINDOW_RESIZABLE)
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
Set window opacity [0.0f..1.0f]
}

@defproc[(SetWindowFocused)
         _void]{
Set window focused
}

@defproc[(GetWindowHandle)
         (_pointer-to _void)]{
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
Get current monitor where window is placed
}

@defproc[(GetMonitorPosition
          [monitor _int])
         _Vector2]{
Get specified monitor position
}

@defproc[(GetMonitorWidth
          [monitor _int])
         _int]{
Get specified monitor width (current video mode used by monitor)
}

@defproc[(GetMonitorHeight
          [monitor _int])
         _int]{
Get specified monitor height (current video mode used by monitor)
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
Get the human-readable, UTF-8 encoded name of the specified monitor
}

@defproc[(SetClipboardText
          [text _string])
         _void]{
Set clipboard text content
}

@defproc[(GetClipboardText)
         _string]{
Get clipboard text content
}

@defproc[(GetClipboardImage)
         _Image]{
Get clipboard image content
}

@defproc[(EnableEventWaiting)
         _void]{
Enable waiting for events on EndDrawing(), no automatic event polling
}

@defproc[(DisableEventWaiting)
         _void]{
Disable waiting for events on EndDrawing(), automatic events polling
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
         _stdbool]{
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
         _stdbool]{
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

@defproc[(BeginMode3D
          [camera _Camera3D])
         _void]{
Begin 3D mode with custom camera (3D)
}

@defproc[(EndMode3D)
         _void]{
Ends 3D mode and returns to default 2D orthographic mode
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

@defproc[(BeginVrStereoMode
          [config _VrStereoConfig])
         _void]{
Begin stereo rendering (requires VR simulator)
}

@defproc[(EndVrStereoMode)
         _void]{
End stereo rendering (requires VR simulator)
}

@defproc[(LoadVrStereoConfig
          [device _VrDeviceInfo])
         _VrStereoConfig]{
Load VR stereo config for VR simulator device parameters
}

@defproc[(UnloadVrStereoConfig
          [config _VrStereoConfig])
         _void]{
Unload VR stereo config
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

@defproc[(IsShaderValid
          [shader _Shader])
         _stdbool]{
Check if a shader is valid (loaded on GPU)
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
          [value (_pointer-to _void)]
          [uniformType _int])
         _void]{
Set shader uniform value
}

@defproc[(SetShaderValueV
          [shader _Shader]
          [locIndex _int]
          [value (_pointer-to _void)]
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

@defproc[(GetScreenToWorldRay
          [position _Vector2]
          [camera _Camera])
         _Ray]{
Get a ray trace from screen position (i.e mouse)
}

@defproc[(GetScreenToWorldRayEx
          [position _Vector2]
          [camera _Camera]
          [width _int]
          [height _int])
         _Ray]{
Get a ray trace from screen position (i.e mouse) in a viewport
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

@defproc[(SetTargetFPS
          [fps _int])
         _void]{
Set target FPS (maximum)
}

@defproc[(GetFrameTime)
         _float]{
Get time in seconds for last frame drawn (delta time)
}

@defproc[(GetTime)
         _double]{
Get elapsed time in seconds since InitWindow()
}

@defproc[(GetFPS)
         _int]{
Get current FPS
}

@defproc[(SwapScreenBuffer)
         _void]{
Swap back buffer with front buffer (screen drawing)
}

@defproc[(PollInputEvents)
         _void]{
Register all input events
}

@defproc[(WaitTime
          [seconds _double])
         _void]{
Wait for some time (halt program execution)
}

@defproc[(SetRandomSeed
          [seed _uint])
         _void]{
Set the seed for the random number generator
}

@defproc[(GetRandomValue
          [min _int]
          [max _int])
         _int]{
Get a random value between min and max (both included)
}

@defproc[(LoadRandomSequence
          [count _uint]
          [min _int]
          [max _int])
         (_pointer-to _int)]{
Load random values sequence, no values repeated
}

@defproc[(UnloadRandomSequence
          [sequence (_pointer-to _int)])
         _void]{
Unload random values sequence
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

@defproc[(OpenURL
          [url _string])
         _void]{
Open URL with default system browser (if available)
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

@defproc[(MemAlloc
          [size _uint])
         (_pointer-to _void)]{
Internal memory allocator
}

@defproc[(MemRealloc
          [ptr (_pointer-to _void)]
          [size _uint])
         (_pointer-to _void)]{
Internal memory reallocator
}

@defproc[(MemFree
          [ptr (_pointer-to _void)])
         _void]{
Internal memory free
}

@defproc[(SetTraceLogCallback
          [callback _TraceLogCallback])
         _void]{
Set custom trace log
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

@defproc[(LoadFileData
          [fileName _string]
          [dataSize (_pointer-to _int)])
         (_pointer-to _ubyte)]{
Load file data as byte array (read)
}

@defproc[(UnloadFileData
          [data (_pointer-to _ubyte)])
         _void]{
Unload file data allocated by LoadFileData()
}

@defproc[(SaveFileData
          [fileName _string]
          [data (_pointer-to _void)]
          [dataSize _int])
         _stdbool]{
Save data to file from byte array (write), returns true on success
}

@defproc[(ExportDataAsCode
          [data (_pointer-to _ubyte)]
          [dataSize _int]
          [fileName _string])
         _stdbool]{
Export data to code (.h), returns true on success
}

@defproc[(LoadFileText
          [fileName _string])
         (_pointer-to _byte)]{
Load text data from file (read), returns a '\0' terminated string
}

@defproc[(UnloadFileText
          [text (_pointer-to _byte)])
         _void]{
Unload file text data allocated by LoadFileText()
}

@defproc[(SaveFileText
          [fileName _string]
          [text (_pointer-to _byte)])
         _stdbool]{
Save text data to file (write), string must be '\0' terminated, returns true on success
}

@defproc[(FileExists
          [fileName _string])
         _stdbool]{
Check if file exists
}

@defproc[(DirectoryExists
          [dirPath _string])
         _stdbool]{
Check if a directory path exists
}

@defproc[(IsFileExtension
          [fileName _string]
          [ext _string])
         _stdbool]{
Check file extension (including point: .png, .wav)
}

@defproc[(GetFileLength
          [fileName _string])
         _int]{
Get file length in bytes (NOTE: GetFileSize() conflicts with windows.h)
}

@defproc[(GetFileExtension
          [fileName _string])
         _string]{
Get pointer to extension for a filename string (includes dot: '.png')
}

@defproc[(GetFileName
          [filePath _string])
         _string]{
Get pointer to filename for a path string
}

@defproc[(GetFileNameWithoutExt
          [filePath _string])
         _string]{
Get filename string without extension (uses static string)
}

@defproc[(GetDirectoryPath
          [filePath _string])
         _string]{
Get full path for a given fileName with path (uses static string)
}

@defproc[(GetPrevDirectoryPath
          [dirPath _string])
         _string]{
Get previous directory path for a given path (uses static string)
}

@defproc[(GetWorkingDirectory)
         _string]{
Get current working directory (uses static string)
}

@defproc[(GetApplicationDirectory)
         _string]{
Get the directory of the running application (uses static string)
}

@defproc[(MakeDirectory
          [dirPath _string])
         _int]{
Create directories (including full path requested), returns 0 on success
}

@defproc[(ChangeDirectory
          [dir _string])
         _stdbool]{
Change working directory, return true on success
}

@defproc[(IsPathFile
          [path _string])
         _stdbool]{
Check if a given path is a file or a directory
}

@defproc[(IsFileNameValid
          [fileName _string])
         _stdbool]{
Check if fileName is valid for the platform/OS
}

@defproc[(LoadDirectoryFiles
          [dirPath _string])
         _FilePathList]{
Load directory filepaths
}

@defproc[(LoadDirectoryFilesEx
          [basePath _string]
          [filter _string]
          [scanSubdirs _stdbool])
         _FilePathList]{
Load directory filepaths with extension filtering and recursive directory scan. Use 'DIR' in the filter string to include directories in the result
}

@defproc[(UnloadDirectoryFiles
          [files _FilePathList])
         _void]{
Unload filepaths
}

@defproc[(IsFileDropped)
         _stdbool]{
Check if a file has been dropped into window
}

@defproc[(LoadDroppedFiles)
         _FilePathList]{
Load dropped filepaths
}

@defproc[(UnloadDroppedFiles
          [files _FilePathList])
         _void]{
Unload dropped filepaths
}

@defproc[(GetFileModTime
          [fileName _string])
         _long]{
Get file modification time (last write time)
}

@defproc[(CompressData
          [data (_pointer-to _ubyte)]
          [dataSize _int]
          [compDataSize (_pointer-to _int)])
         (_pointer-to _ubyte)]{
Compress data (DEFLATE algorithm), memory must be MemFree()
}

@defproc[(DecompressData
          [compData (_pointer-to _ubyte)]
          [compDataSize _int]
          [dataSize (_pointer-to _int)])
         (_pointer-to _ubyte)]{
Decompress data (DEFLATE algorithm), memory must be MemFree()
}

@defproc[(EncodeDataBase64
          [data (_pointer-to _ubyte)]
          [dataSize _int]
          [outputSize (_pointer-to _int)])
         (_pointer-to _byte)]{
Encode data to Base64 string, memory must be MemFree()
}

@defproc[(DecodeDataBase64
          [data (_pointer-to _ubyte)]
          [outputSize (_pointer-to _int)])
         (_pointer-to _ubyte)]{
Decode Base64 string data, memory must be MemFree()
}

@defproc[(ComputeCRC32
          [data (_pointer-to _ubyte)]
          [dataSize _int])
         _uint]{
Compute CRC32 hash code
}

@defproc[(ComputeMD5
          [data (_pointer-to _ubyte)]
          [dataSize _int])
         (_pointer-to _uint)]{
Compute MD5 hash code, returns static int[4] (16 bytes)
}

@defproc[(ComputeSHA1
          [data (_pointer-to _ubyte)]
          [dataSize _int])
         (_pointer-to _uint)]{
Compute SHA1 hash code, returns static int[5] (20 bytes)
}

@defproc[(LoadAutomationEventList
          [fileName _string])
         _AutomationEventList]{
Load automation events list from file, NULL for empty list, capacity = MAX_AUTOMATION_EVENTS
}

@defproc[(UnloadAutomationEventList
          [list _AutomationEventList])
         _void]{
Unload automation events list from file
}

@defproc[(ExportAutomationEventList
          [list _AutomationEventList]
          [fileName _string])
         _stdbool]{
Export automation events list as text file
}

@defproc[(SetAutomationEventList
          [list (_pointer-to _AutomationEventList)])
         _void]{
Set automation event list to record to
}

@defproc[(SetAutomationEventBaseFrame
          [frame _int])
         _void]{
Set automation event internal base frame to start recording
}

@defproc[(StartAutomationEventRecording)
         _void]{
Start recording automation events (AutomationEventList must be set)
}

@defproc[(StopAutomationEventRecording)
         _void]{
Stop recording automation events
}

@defproc[(PlayAutomationEvent
          [event _AutomationEvent])
         _void]{
Play a recorded automation event
}

@defproc[(IsKeyPressed
          [key _int])
         _stdbool]{
Check if a key has been pressed once
}

@defproc[(IsKeyPressedRepeat
          [key _int])
         _stdbool]{
Check if a key has been pressed again
}

@defproc[(IsKeyDown
          [key _int])
         _stdbool]{
Check if a key is being pressed
}

@defproc[(IsKeyReleased
          [key _int])
         _stdbool]{
Check if a key has been released once
}

@defproc[(IsKeyUp
          [key _int])
         _stdbool]{
Check if a key is NOT being pressed
}

@defproc[(GetKeyPressed)
         _int]{
Get key pressed (keycode), call it multiple times for keys queued, returns 0 when the queue is empty
}

@defproc[(GetCharPressed)
         _int]{
Get char pressed (unicode), call it multiple times for chars queued, returns 0 when the queue is empty
}

@defproc[(SetExitKey
          [key _int])
         _void]{
Set a custom key to exit program (default is ESC)
}

@defproc[(IsGamepadAvailable
          [gamepad _int])
         _stdbool]{
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
         _stdbool]{
Check if a gamepad button has been pressed once
}

@defproc[(IsGamepadButtonDown
          [gamepad _int]
          [button _int])
         _stdbool]{
Check if a gamepad button is being pressed
}

@defproc[(IsGamepadButtonReleased
          [gamepad _int]
          [button _int])
         _stdbool]{
Check if a gamepad button has been released once
}

@defproc[(IsGamepadButtonUp
          [gamepad _int]
          [button _int])
         _stdbool]{
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

@defproc[(SetGamepadVibration
          [gamepad _int]
          [leftMotor _float]
          [rightMotor _float]
          [duration _float])
         _void]{
Set gamepad vibration for both motors (duration in seconds)
}

@defproc[(IsMouseButtonPressed
          [button _int])
         _stdbool]{
Check if a mouse button has been pressed once
}

@defproc[(IsMouseButtonDown
          [button _int])
         _stdbool]{
Check if a mouse button is being pressed
}

@defproc[(IsMouseButtonReleased
          [button _int])
         _stdbool]{
Check if a mouse button has been released once
}

@defproc[(IsMouseButtonUp
          [button _int])
         _stdbool]{
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
Get mouse wheel movement for X or Y, whichever is larger
}

@defproc[(GetMouseWheelMoveV)
         _Vector2]{
Get mouse wheel movement for both X and Y
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
          [gesture _uint])
         _stdbool]{
Check if a gesture have been detected
}

@defproc[(GetGestureDetected)
         _int]{
Get latest detected gesture
}

@defproc[(GetGestureHoldDuration)
         _float]{
Get gesture hold time in seconds
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

@defproc[(UpdateCamera
          [camera (_pointer-to _Camera)]
          [mode _int])
         _void]{
Update camera position for selected mode
}

@defproc[(UpdateCameraPro
          [camera (_pointer-to _Camera)]
          [movement _Vector3]
          [rotation _Vector3]
          [zoom _float])
         _void]{
Update camera movement/rotation
}

@defproc[(SetShapesTexture
          [texture _Texture2D]
          [source _Rectangle])
         _void]{
Set texture and rectangle to be used on shapes drawing
}

@defproc[(GetShapesTexture)
         _Texture2D]{
Get texture that is used for shapes drawing
}

@defproc[(GetShapesTextureRectangle)
         _Rectangle]{
Get texture source rectangle that is used for shapes drawing
}

@defproc[(DrawPixel
          [posX _int]
          [posY _int]
          [color _Color])
         _void]{
Draw a pixel using geometry [Can be slow, use with care]
}

@defproc[(DrawPixelV
          [position _Vector2]
          [color _Color])
         _void]{
Draw a pixel using geometry (Vector version) [Can be slow, use with care]
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
Draw a line (using gl lines)
}

@defproc[(DrawLineEx
          [startPos _Vector2]
          [endPos _Vector2]
          [thick _float]
          [color _Color])
         _void]{
Draw a line (using triangles/quads)
}

@defproc[(DrawLineStrip
          [points (_pointer-to _Vector2)]
          [pointCount _int]
          [color _Color])
         _void]{
Draw lines sequence (using gl lines)
}

@defproc[(DrawLineBezier
          [startPos _Vector2]
          [endPos _Vector2]
          [thick _float]
          [color _Color])
         _void]{
Draw line segment cubic-bezier in-out interpolation
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
          [inner _Color]
          [outer _Color])
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

@defproc[(DrawCircleLinesV
          [center _Vector2]
          [radius _float]
          [color _Color])
         _void]{
Draw circle outline (Vector version)
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
          [top _Color]
          [bottom _Color])
         _void]{
Draw a vertical-gradient-filled rectangle
}

@defproc[(DrawRectangleGradientH
          [posX _int]
          [posY _int]
          [width _int]
          [height _int]
          [left _Color]
          [right _Color])
         _void]{
Draw a horizontal-gradient-filled rectangle
}

@defproc[(DrawRectangleGradientEx
          [rec _Rectangle]
          [topLeft _Color]
          [bottomLeft _Color]
          [topRight _Color]
          [bottomRight _Color])
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
          [color _Color])
         _void]{
Draw rectangle lines with rounded edges
}

@defproc[(DrawRectangleRoundedLinesEx
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
          [points (_pointer-to _Vector2)]
          [pointCount _int]
          [color _Color])
         _void]{
Draw a triangle fan defined by points (first vertex is the center)
}

@defproc[(DrawTriangleStrip
          [points (_pointer-to _Vector2)]
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

@defproc[(DrawSplineLinear
          [points (_pointer-to _Vector2)]
          [pointCount _int]
          [thick _float]
          [color _Color])
         _void]{
Draw spline: Linear, minimum 2 points
}

@defproc[(DrawSplineBasis
          [points (_pointer-to _Vector2)]
          [pointCount _int]
          [thick _float]
          [color _Color])
         _void]{
Draw spline: B-Spline, minimum 4 points
}

@defproc[(DrawSplineCatmullRom
          [points (_pointer-to _Vector2)]
          [pointCount _int]
          [thick _float]
          [color _Color])
         _void]{
Draw spline: Catmull-Rom, minimum 4 points
}

@defproc[(DrawSplineBezierQuadratic
          [points (_pointer-to _Vector2)]
          [pointCount _int]
          [thick _float]
          [color _Color])
         _void]{
Draw spline: Quadratic Bezier, minimum 3 points (1 control point): [p1, c2, p3, c4...]
}

@defproc[(DrawSplineBezierCubic
          [points (_pointer-to _Vector2)]
          [pointCount _int]
          [thick _float]
          [color _Color])
         _void]{
Draw spline: Cubic Bezier, minimum 4 points (2 control points): [p1, c2, c3, p4, c5, c6...]
}

@defproc[(DrawSplineSegmentLinear
          [p1 _Vector2]
          [p2 _Vector2]
          [thick _float]
          [color _Color])
         _void]{
Draw spline segment: Linear, 2 points
}

@defproc[(DrawSplineSegmentBasis
          [p1 _Vector2]
          [p2 _Vector2]
          [p3 _Vector2]
          [p4 _Vector2]
          [thick _float]
          [color _Color])
         _void]{
Draw spline segment: B-Spline, 4 points
}

@defproc[(DrawSplineSegmentCatmullRom
          [p1 _Vector2]
          [p2 _Vector2]
          [p3 _Vector2]
          [p4 _Vector2]
          [thick _float]
          [color _Color])
         _void]{
Draw spline segment: Catmull-Rom, 4 points
}

@defproc[(DrawSplineSegmentBezierQuadratic
          [p1 _Vector2]
          [c2 _Vector2]
          [p3 _Vector2]
          [thick _float]
          [color _Color])
         _void]{
Draw spline segment: Quadratic Bezier, 2 points, 1 control point
}

@defproc[(DrawSplineSegmentBezierCubic
          [p1 _Vector2]
          [c2 _Vector2]
          [c3 _Vector2]
          [p4 _Vector2]
          [thick _float]
          [color _Color])
         _void]{
Draw spline segment: Cubic Bezier, 2 points, 2 control points
}

@defproc[(GetSplinePointLinear
          [startPos _Vector2]
          [endPos _Vector2]
          [t _float])
         _Vector2]{
Get (evaluate) spline point: Linear
}

@defproc[(GetSplinePointBasis
          [p1 _Vector2]
          [p2 _Vector2]
          [p3 _Vector2]
          [p4 _Vector2]
          [t _float])
         _Vector2]{
Get (evaluate) spline point: B-Spline
}

@defproc[(GetSplinePointCatmullRom
          [p1 _Vector2]
          [p2 _Vector2]
          [p3 _Vector2]
          [p4 _Vector2]
          [t _float])
         _Vector2]{
Get (evaluate) spline point: Catmull-Rom
}

@defproc[(GetSplinePointBezierQuad
          [p1 _Vector2]
          [c2 _Vector2]
          [p3 _Vector2]
          [t _float])
         _Vector2]{
Get (evaluate) spline point: Quadratic Bezier
}

@defproc[(GetSplinePointBezierCubic
          [p1 _Vector2]
          [c2 _Vector2]
          [c3 _Vector2]
          [p4 _Vector2]
          [t _float])
         _Vector2]{
Get (evaluate) spline point: Cubic Bezier
}

@defproc[(CheckCollisionRecs
          [rec1 _Rectangle]
          [rec2 _Rectangle])
         _stdbool]{
Check collision between two rectangles
}

@defproc[(CheckCollisionCircles
          [center1 _Vector2]
          [radius1 _float]
          [center2 _Vector2]
          [radius2 _float])
         _stdbool]{
Check collision between two circles
}

@defproc[(CheckCollisionCircleRec
          [center _Vector2]
          [radius _float]
          [rec _Rectangle])
         _stdbool]{
Check collision between circle and rectangle
}

@defproc[(CheckCollisionCircleLine
          [center _Vector2]
          [radius _float]
          [p1 _Vector2]
          [p2 _Vector2])
         _stdbool]{
Check if circle collides with a line created betweeen two points [p1] and [p2]
}

@defproc[(CheckCollisionPointRec
          [point _Vector2]
          [rec _Rectangle])
         _stdbool]{
Check if point is inside rectangle
}

@defproc[(CheckCollisionPointCircle
          [point _Vector2]
          [center _Vector2]
          [radius _float])
         _stdbool]{
Check if point is inside circle
}

@defproc[(CheckCollisionPointTriangle
          [point _Vector2]
          [p1 _Vector2]
          [p2 _Vector2]
          [p3 _Vector2])
         _stdbool]{
Check if point is inside a triangle
}

@defproc[(CheckCollisionPointLine
          [point _Vector2]
          [p1 _Vector2]
          [p2 _Vector2]
          [threshold _int])
         _stdbool]{
Check if point belongs to line created between two points [p1] and [p2] with defined margin in pixels [threshold]
}

@defproc[(CheckCollisionPointPoly
          [point _Vector2]
          [points (_pointer-to _Vector2)]
          [pointCount _int])
         _stdbool]{
Check if point is within a polygon described by array of vertices
}

@defproc[(CheckCollisionLines
          [startPos1 _Vector2]
          [endPos1 _Vector2]
          [startPos2 _Vector2]
          [endPos2 _Vector2]
          [collisionPoint (_pointer-to _Vector2)])
         _stdbool]{
Check the collision between two lines defined by two points each, returns collision point by reference
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
          [frames (_pointer-to _int)])
         _Image]{
Load image sequence from file (frames appended to image.data)
}

@defproc[(LoadImageAnimFromMemory
          [fileType _string]
          [fileData (_pointer-to _ubyte)]
          [dataSize _int]
          [frames (_pointer-to _int)])
         _Image]{
Load image sequence from memory buffer
}

@defproc[(LoadImageFromMemory
          [fileType _string]
          [fileData (_pointer-to _ubyte)]
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

@defproc[(IsImageValid
          [image _Image])
         _stdbool]{
Check if an image is valid (data and parameters)
}

@defproc[(UnloadImage
          [image _Image])
         _void]{
Unload image from CPU memory (RAM)
}

@defproc[(ExportImage
          [image _Image]
          [fileName _string])
         _stdbool]{
Export image data to file, returns true on success
}

@defproc[(ExportImageToMemory
          [image _Image]
          [fileType _string]
          [fileSize (_pointer-to _int)])
         (_pointer-to _ubyte)]{
Export image to memory buffer
}

@defproc[(ExportImageAsCode
          [image _Image]
          [fileName _string])
         _stdbool]{
Export image as code file defining an array of bytes, returns true on success
}

@defproc[(GenImageColor
          [width _int]
          [height _int]
          [color _Color])
         _Image]{
Generate image: plain color
}

@defproc[(GenImageGradientLinear
          [width _int]
          [height _int]
          [direction _int]
          [start _Color]
          [end _Color])
         _Image]{
Generate image: linear gradient, direction in degrees [0..360], 0=Vertical gradient
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

@defproc[(GenImageGradientSquare
          [width _int]
          [height _int]
          [density _float]
          [inner _Color]
          [outer _Color])
         _Image]{
Generate image: square gradient
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

@defproc[(GenImagePerlinNoise
          [width _int]
          [height _int]
          [offsetX _int]
          [offsetY _int]
          [scale _float])
         _Image]{
Generate image: perlin noise
}

@defproc[(GenImageCellular
          [width _int]
          [height _int]
          [tileSize _int])
         _Image]{
Generate image: cellular algorithm, bigger tileSize means bigger cells
}

@defproc[(GenImageText
          [width _int]
          [height _int]
          [text _string])
         _Image]{
Generate image: grayscale image from text data
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

@defproc[(ImageFromChannel
          [image _Image]
          [selectedChannel _int])
         _Image]{
Create an image from a selected channel of another image (GRAYSCALE)
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
          [image (_pointer-to _Image)]
          [newFormat _int])
         _void]{
Convert image data to desired format
}

@defproc[(ImageToPOT
          [image (_pointer-to _Image)]
          [fill _Color])
         _void]{
Convert image to POT (power-of-two)
}

@defproc[(ImageCrop
          [image (_pointer-to _Image)]
          [crop _Rectangle])
         _void]{
Crop an image to a defined rectangle
}

@defproc[(ImageAlphaCrop
          [image (_pointer-to _Image)]
          [threshold _float])
         _void]{
Crop image depending on alpha value
}

@defproc[(ImageAlphaClear
          [image (_pointer-to _Image)]
          [color _Color]
          [threshold _float])
         _void]{
Clear alpha channel to desired color
}

@defproc[(ImageAlphaMask
          [image (_pointer-to _Image)]
          [alphaMask _Image])
         _void]{
Apply alpha mask to image
}

@defproc[(ImageAlphaPremultiply
          [image (_pointer-to _Image)])
         _void]{
Premultiply alpha channel
}

@defproc[(ImageBlurGaussian
          [image (_pointer-to _Image)]
          [blurSize _int])
         _void]{
Apply Gaussian blur using a box blur approximation
}

@defproc[(ImageKernelConvolution
          [image (_pointer-to _Image)]
          [kernel (_pointer-to _float)]
          [kernelSize _int])
         _void]{
Apply custom square convolution kernel to image
}

@defproc[(ImageResize
          [image (_pointer-to _Image)]
          [newWidth _int]
          [newHeight _int])
         _void]{
Resize image (Bicubic scaling algorithm)
}

@defproc[(ImageResizeNN
          [image (_pointer-to _Image)]
          [newWidth _int]
          [newHeight _int])
         _void]{
Resize image (Nearest-Neighbor scaling algorithm)
}

@defproc[(ImageResizeCanvas
          [image (_pointer-to _Image)]
          [newWidth _int]
          [newHeight _int]
          [offsetX _int]
          [offsetY _int]
          [fill _Color])
         _void]{
Resize canvas and fill with color
}

@defproc[(ImageMipmaps
          [image (_pointer-to _Image)])
         _void]{
Compute all mipmap levels for a provided image
}

@defproc[(ImageDither
          [image (_pointer-to _Image)]
          [rBpp _int]
          [gBpp _int]
          [bBpp _int]
          [aBpp _int])
         _void]{
Dither image data to 16bpp or lower (Floyd-Steinberg dithering)
}

@defproc[(ImageFlipVertical
          [image (_pointer-to _Image)])
         _void]{
Flip image vertically
}

@defproc[(ImageFlipHorizontal
          [image (_pointer-to _Image)])
         _void]{
Flip image horizontally
}

@defproc[(ImageRotate
          [image (_pointer-to _Image)]
          [degrees _int])
         _void]{
Rotate image by input angle in degrees (-359 to 359)
}

@defproc[(ImageRotateCW
          [image (_pointer-to _Image)])
         _void]{
Rotate image clockwise 90deg
}

@defproc[(ImageRotateCCW
          [image (_pointer-to _Image)])
         _void]{
Rotate image counter-clockwise 90deg
}

@defproc[(ImageColorTint
          [image (_pointer-to _Image)]
          [color _Color])
         _void]{
Modify image color: tint
}

@defproc[(ImageColorInvert
          [image (_pointer-to _Image)])
         _void]{
Modify image color: invert
}

@defproc[(ImageColorGrayscale
          [image (_pointer-to _Image)])
         _void]{
Modify image color: grayscale
}

@defproc[(ImageColorContrast
          [image (_pointer-to _Image)]
          [contrast _float])
         _void]{
Modify image color: contrast (-100 to 100)
}

@defproc[(ImageColorBrightness
          [image (_pointer-to _Image)]
          [brightness _int])
         _void]{
Modify image color: brightness (-255 to 255)
}

@defproc[(ImageColorReplace
          [image (_pointer-to _Image)]
          [color _Color]
          [replace _Color])
         _void]{
Modify image color: replace color
}

@defproc[(LoadImageColors
          [image _Image])
         (_pointer-to _Color)]{
Load color data from image as a Color array (RGBA - 32bit)
}

@defproc[(LoadImagePalette
          [image _Image]
          [maxPaletteSize _int]
          [colorCount (_pointer-to _int)])
         (_pointer-to _Color)]{
Load colors palette from image as a Color array (RGBA - 32bit)
}

@defproc[(UnloadImageColors
          [colors (_pointer-to _Color)])
         _void]{
Unload color data loaded with LoadImageColors()
}

@defproc[(UnloadImagePalette
          [colors (_pointer-to _Color)])
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
          [dst (_pointer-to _Image)]
          [color _Color])
         _void]{
Clear image background with given color
}

@defproc[(ImageDrawPixel
          [dst (_pointer-to _Image)]
          [posX _int]
          [posY _int]
          [color _Color])
         _void]{
Draw pixel within an image
}

@defproc[(ImageDrawPixelV
          [dst (_pointer-to _Image)]
          [position _Vector2]
          [color _Color])
         _void]{
Draw pixel within an image (Vector version)
}

@defproc[(ImageDrawLine
          [dst (_pointer-to _Image)]
          [startPosX _int]
          [startPosY _int]
          [endPosX _int]
          [endPosY _int]
          [color _Color])
         _void]{
Draw line within an image
}

@defproc[(ImageDrawLineV
          [dst (_pointer-to _Image)]
          [start _Vector2]
          [end _Vector2]
          [color _Color])
         _void]{
Draw line within an image (Vector version)
}

@defproc[(ImageDrawLineEx
          [dst (_pointer-to _Image)]
          [start _Vector2]
          [end _Vector2]
          [thick _int]
          [color _Color])
         _void]{
Draw a line defining thickness within an image
}

@defproc[(ImageDrawCircle
          [dst (_pointer-to _Image)]
          [centerX _int]
          [centerY _int]
          [radius _int]
          [color _Color])
         _void]{
Draw a filled circle within an image
}

@defproc[(ImageDrawCircleV
          [dst (_pointer-to _Image)]
          [center _Vector2]
          [radius _int]
          [color _Color])
         _void]{
Draw a filled circle within an image (Vector version)
}

@defproc[(ImageDrawCircleLines
          [dst (_pointer-to _Image)]
          [centerX _int]
          [centerY _int]
          [radius _int]
          [color _Color])
         _void]{
Draw circle outline within an image
}

@defproc[(ImageDrawCircleLinesV
          [dst (_pointer-to _Image)]
          [center _Vector2]
          [radius _int]
          [color _Color])
         _void]{
Draw circle outline within an image (Vector version)
}

@defproc[(ImageDrawRectangle
          [dst (_pointer-to _Image)]
          [posX _int]
          [posY _int]
          [width _int]
          [height _int]
          [color _Color])
         _void]{
Draw rectangle within an image
}

@defproc[(ImageDrawRectangleV
          [dst (_pointer-to _Image)]
          [position _Vector2]
          [size _Vector2]
          [color _Color])
         _void]{
Draw rectangle within an image (Vector version)
}

@defproc[(ImageDrawRectangleRec
          [dst (_pointer-to _Image)]
          [rec _Rectangle]
          [color _Color])
         _void]{
Draw rectangle within an image
}

@defproc[(ImageDrawRectangleLines
          [dst (_pointer-to _Image)]
          [rec _Rectangle]
          [thick _int]
          [color _Color])
         _void]{
Draw rectangle lines within an image
}

@defproc[(ImageDrawTriangle
          [dst (_pointer-to _Image)]
          [v1 _Vector2]
          [v2 _Vector2]
          [v3 _Vector2]
          [color _Color])
         _void]{
Draw triangle within an image
}

@defproc[(ImageDrawTriangleEx
          [dst (_pointer-to _Image)]
          [v1 _Vector2]
          [v2 _Vector2]
          [v3 _Vector2]
          [c1 _Color]
          [c2 _Color]
          [c3 _Color])
         _void]{
Draw triangle with interpolated colors within an image
}

@defproc[(ImageDrawTriangleLines
          [dst (_pointer-to _Image)]
          [v1 _Vector2]
          [v2 _Vector2]
          [v3 _Vector2]
          [color _Color])
         _void]{
Draw triangle outline within an image
}

@defproc[(ImageDrawTriangleFan
          [dst (_pointer-to _Image)]
          [points (_pointer-to _Vector2)]
          [pointCount _int]
          [color _Color])
         _void]{
Draw a triangle fan defined by points within an image (first vertex is the center)
}

@defproc[(ImageDrawTriangleStrip
          [dst (_pointer-to _Image)]
          [points (_pointer-to _Vector2)]
          [pointCount _int]
          [color _Color])
         _void]{
Draw a triangle strip defined by points within an image
}

@defproc[(ImageDraw
          [dst (_pointer-to _Image)]
          [src _Image]
          [srcRec _Rectangle]
          [dstRec _Rectangle]
          [tint _Color])
         _void]{
Draw a source image within a destination image (tint applied to source)
}

@defproc[(ImageDrawText
          [dst (_pointer-to _Image)]
          [text _string]
          [posX _int]
          [posY _int]
          [fontSize _int]
          [color _Color])
         _void]{
Draw text (using default font) within an image (destination)
}

@defproc[(ImageDrawTextEx
          [dst (_pointer-to _Image)]
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

@defproc[(IsTextureValid
          [texture _Texture2D])
         _stdbool]{
Check if a texture is valid (loaded in GPU)
}

@defproc[(UnloadTexture
          [texture _Texture2D])
         _void]{
Unload texture from GPU memory (VRAM)
}

@defproc[(IsRenderTextureValid
          [target _RenderTexture2D])
         _stdbool]{
Check if a render texture is valid (loaded in GPU)
}

@defproc[(UnloadRenderTexture
          [target _RenderTexture2D])
         _void]{
Unload render texture from GPU memory (VRAM)
}

@defproc[(UpdateTexture
          [texture _Texture2D]
          [pixels (_pointer-to _void)])
         _void]{
Update GPU texture with new data
}

@defproc[(UpdateTextureRec
          [texture _Texture2D]
          [rec _Rectangle]
          [pixels (_pointer-to _void)])
         _void]{
Update GPU texture rectangle with new data
}

@defproc[(GenTextureMipmaps
          [texture (_pointer-to _Texture2D)])
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

@defproc[(ColorIsEqual
          [col1 _Color]
          [col2 _Color])
         _stdbool]{
Check if two colors are equal
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
Get hexadecimal value for a Color (0xRRGGBBAA)
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

@defproc[(ColorTint
          [color _Color]
          [tint _Color])
         _Color]{
Get color multiplied with another color
}

@defproc[(ColorBrightness
          [color _Color]
          [factor _float])
         _Color]{
Get color with brightness correction, brightness factor goes from -1.0f to 1.0f
}

@defproc[(ColorContrast
          [color _Color]
          [contrast _float])
         _Color]{
Get color with contrast correction, contrast values between -1.0f and 1.0f
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

@defproc[(ColorLerp
          [color1 _Color]
          [color2 _Color]
          [factor _float])
         _Color]{
Get color lerp interpolation between two colors, factor [0.0f..1.0f]
}

@defproc[(GetColor
          [hexValue _uint])
         _Color]{
Get Color structure from hexadecimal value
}

@defproc[(GetPixelColor
          [srcPtr (_pointer-to _void)]
          [format _int])
         _Color]{
Get Color from a source pixel pointer of certain format
}

@defproc[(SetPixelColor
          [dstPtr (_pointer-to _void)]
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
          [codepoints (_pointer-to _int)]
          [codepointCount _int])
         _Font]{
Load font from file with extended parameters, use NULL for codepoints and 0 for codepointCount to load the default character set, font size is provided in pixels height
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
          [fileData (_pointer-to _ubyte)]
          [dataSize _int]
          [fontSize _int]
          [codepoints (_pointer-to _int)]
          [codepointCount _int])
         _Font]{
Load font from memory buffer, fileType refers to extension: i.e. '.ttf'
}

@defproc[(IsFontValid
          [font _Font])
         _stdbool]{
Check if a font is valid (font data loaded, WARNING: GPU texture not checked)
}

@defproc[(LoadFontData
          [fileData (_pointer-to _ubyte)]
          [dataSize _int]
          [fontSize _int]
          [codepoints (_pointer-to _int)]
          [codepointCount _int]
          [type _int])
         (_pointer-to _GlyphInfo)]{
Load font data for further use
}

@defproc[(GenImageFontAtlas
          [glyphs (_pointer-to _GlyphInfo)]
          [glyphRecs (_pointer-to (_pointer-to _Rectangle))]
          [glyphCount _int]
          [fontSize _int]
          [padding _int]
          [packMethod _int])
         _Image]{
Generate image font atlas using chars info
}

@defproc[(UnloadFontData
          [glyphs (_pointer-to _GlyphInfo)]
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
         _stdbool]{
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
          [codepoints (_pointer-to _int)]
          [codepointCount _int]
          [position _Vector2]
          [fontSize _float]
          [spacing _float]
          [tint _Color])
         _void]{
Draw multiple character (codepoint)
}

@defproc[(SetTextLineSpacing
          [spacing _int])
         _void]{
Set vertical line spacing when drawing with line-breaks
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

@defproc[(LoadUTF8
          [codepoints (_pointer-to _int)]
          [length _int])
         (_pointer-to _byte)]{
Load UTF-8 text encoded from codepoints array
}

@defproc[(UnloadUTF8
          [text (_pointer-to _byte)])
         _void]{
Unload UTF-8 text encoded from codepoints array
}

@defproc[(LoadCodepoints
          [text _string]
          [count (_pointer-to _int)])
         (_pointer-to _int)]{
Load all codepoints from a UTF-8 text string, codepoints count returned by parameter
}

@defproc[(UnloadCodepoints
          [codepoints (_pointer-to _int)])
         _void]{
Unload codepoints data from memory
}

@defproc[(GetCodepointCount
          [text _string])
         _int]{
Get total number of codepoints in a UTF-8 encoded string
}

@defproc[(GetCodepoint
          [text _string]
          [codepointSize (_pointer-to _int)])
         _int]{
Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
}

@defproc[(GetCodepointNext
          [text _string]
          [codepointSize (_pointer-to _int)])
         _int]{
Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
}

@defproc[(GetCodepointPrevious
          [text _string]
          [codepointSize (_pointer-to _int)])
         _int]{
Get previous codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
}

@defproc[(CodepointToUTF8
          [codepoint _int]
          [utf8Size (_pointer-to _int)])
         _string]{
Encode one codepoint into UTF-8 byte array (array length returned as parameter)
}

@defproc[(TextCopy
          [dst (_pointer-to _byte)]
          [src _string])
         _int]{
Copy one string to another, returns bytes copied
}

@defproc[(TextIsEqual
          [text1 _string]
          [text2 _string])
         _stdbool]{
Check if two text string are equal
}

@defproc[(TextLength
          [text _string])
         _uint]{
Get text length, checks for '\0' ending
}

@defproc[(TextFormat
          [text _string])
         _string]{
Text formatting with variables (sprintf() style)
}

@defproc[(TextSubtext
          [text _string]
          [position _int]
          [length _int])
         _string]{
Get a piece of a text string
}

@defproc[(TextReplace
          [text _string]
          [replace _string]
          [by _string])
         (_pointer-to _byte)]{
Replace text string (WARNING: memory must be freed!)
}

@defproc[(TextInsert
          [text _string]
          [insert _string]
          [position _int])
         (_pointer-to _byte)]{
Insert text in a position (WARNING: memory must be freed!)
}

@defproc[(TextJoin
          [textList (_pointer-to (_pointer-to _byte))]
          [count _int]
          [delimiter _string])
         _string]{
Join text strings with delimiter
}

@defproc[(TextSplit
          [text _string]
          [delimiter _byte]
          [count (_pointer-to _int)])
         (_pointer-to (_pointer-to _byte))]{
Split text into multiple strings
}

@defproc[(TextAppend
          [text (_pointer-to _byte)]
          [append _string]
          [position (_pointer-to _int)])
         _void]{
Append text at specific position and move cursor!
}

@defproc[(TextFindIndex
          [text _string]
          [find _string])
         _int]{
Find first text occurrence within a string
}

@defproc[(TextToUpper
          [text _string])
         _string]{
Get upper case version of provided string
}

@defproc[(TextToLower
          [text _string])
         _string]{
Get lower case version of provided string
}

@defproc[(TextToPascal
          [text _string])
         _string]{
Get Pascal case notation version of provided string
}

@defproc[(TextToSnake
          [text _string])
         _string]{
Get Snake case notation version of provided string
}

@defproc[(TextToCamel
          [text _string])
         _string]{
Get Camel case notation version of provided string
}

@defproc[(TextToInteger
          [text _string])
         _int]{
Get integer value from text (negative values not supported)
}

@defproc[(TextToFloat
          [text _string])
         _float]{
Get float value from text (negative values not supported)
}

@defproc[(DrawLine3D
          [startPos _Vector3]
          [endPos _Vector3]
          [color _Color])
         _void]{
Draw a line in 3D world space
}

@defproc[(DrawPoint3D
          [position _Vector3]
          [color _Color])
         _void]{
Draw a point in 3D space, actually a small line
}

@defproc[(DrawCircle3D
          [center _Vector3]
          [radius _float]
          [rotationAxis _Vector3]
          [rotationAngle _float]
          [color _Color])
         _void]{
Draw a circle in 3D world space
}

@defproc[(DrawTriangle3D
          [v1 _Vector3]
          [v2 _Vector3]
          [v3 _Vector3]
          [color _Color])
         _void]{
Draw a color-filled triangle (vertex in counter-clockwise order!)
}

@defproc[(DrawTriangleStrip3D
          [points (_pointer-to _Vector3)]
          [pointCount _int]
          [color _Color])
         _void]{
Draw a triangle strip defined by points
}

@defproc[(DrawCube
          [position _Vector3]
          [width _float]
          [height _float]
          [length _float]
          [color _Color])
         _void]{
Draw cube
}

@defproc[(DrawCubeV
          [position _Vector3]
          [size _Vector3]
          [color _Color])
         _void]{
Draw cube (Vector version)
}

@defproc[(DrawCubeWires
          [position _Vector3]
          [width _float]
          [height _float]
          [length _float]
          [color _Color])
         _void]{
Draw cube wires
}

@defproc[(DrawCubeWiresV
          [position _Vector3]
          [size _Vector3]
          [color _Color])
         _void]{
Draw cube wires (Vector version)
}

@defproc[(DrawSphere
          [centerPos _Vector3]
          [radius _float]
          [color _Color])
         _void]{
Draw sphere
}

@defproc[(DrawSphereEx
          [centerPos _Vector3]
          [radius _float]
          [rings _int]
          [slices _int]
          [color _Color])
         _void]{
Draw sphere with extended parameters
}

@defproc[(DrawSphereWires
          [centerPos _Vector3]
          [radius _float]
          [rings _int]
          [slices _int]
          [color _Color])
         _void]{
Draw sphere wires
}

@defproc[(DrawCylinder
          [position _Vector3]
          [radiusTop _float]
          [radiusBottom _float]
          [height _float]
          [slices _int]
          [color _Color])
         _void]{
Draw a cylinder/cone
}

@defproc[(DrawCylinderEx
          [startPos _Vector3]
          [endPos _Vector3]
          [startRadius _float]
          [endRadius _float]
          [sides _int]
          [color _Color])
         _void]{
Draw a cylinder with base at startPos and top at endPos
}

@defproc[(DrawCylinderWires
          [position _Vector3]
          [radiusTop _float]
          [radiusBottom _float]
          [height _float]
          [slices _int]
          [color _Color])
         _void]{
Draw a cylinder/cone wires
}

@defproc[(DrawCylinderWiresEx
          [startPos _Vector3]
          [endPos _Vector3]
          [startRadius _float]
          [endRadius _float]
          [sides _int]
          [color _Color])
         _void]{
Draw a cylinder wires with base at startPos and top at endPos
}

@defproc[(DrawCapsule
          [startPos _Vector3]
          [endPos _Vector3]
          [radius _float]
          [slices _int]
          [rings _int]
          [color _Color])
         _void]{
Draw a capsule with the center of its sphere caps at startPos and endPos
}

@defproc[(DrawCapsuleWires
          [startPos _Vector3]
          [endPos _Vector3]
          [radius _float]
          [slices _int]
          [rings _int]
          [color _Color])
         _void]{
Draw capsule wireframe with the center of its sphere caps at startPos and endPos
}

@defproc[(DrawPlane
          [centerPos _Vector3]
          [size _Vector2]
          [color _Color])
         _void]{
Draw a plane XZ
}

@defproc[(DrawRay
          [ray _Ray]
          [color _Color])
         _void]{
Draw a ray line
}

@defproc[(DrawGrid
          [slices _int]
          [spacing _float])
         _void]{
Draw a grid (centered at (0, 0, 0))
}

@defproc[(LoadModel
          [fileName _string])
         _Model]{
Load model from files (meshes and materials)
}

@defproc[(LoadModelFromMesh
          [mesh _Mesh])
         _Model]{
Load model from generated mesh (default material)
}

@defproc[(IsModelValid
          [model _Model])
         _stdbool]{
Check if a model is valid (loaded in GPU, VAO/VBOs)
}

@defproc[(UnloadModel
          [model _Model])
         _void]{
Unload model (including meshes) from memory (RAM and/or VRAM)
}

@defproc[(GetModelBoundingBox
          [model _Model])
         _BoundingBox]{
Compute model bounding box limits (considers all meshes)
}

@defproc[(DrawModel
          [model _Model]
          [position _Vector3]
          [scale _float]
          [tint _Color])
         _void]{
Draw a model (with texture if set)
}

@defproc[(DrawModelEx
          [model _Model]
          [position _Vector3]
          [rotationAxis _Vector3]
          [rotationAngle _float]
          [scale _Vector3]
          [tint _Color])
         _void]{
Draw a model with extended parameters
}

@defproc[(DrawModelWires
          [model _Model]
          [position _Vector3]
          [scale _float]
          [tint _Color])
         _void]{
Draw a model wires (with texture if set)
}

@defproc[(DrawModelWiresEx
          [model _Model]
          [position _Vector3]
          [rotationAxis _Vector3]
          [rotationAngle _float]
          [scale _Vector3]
          [tint _Color])
         _void]{
Draw a model wires (with texture if set) with extended parameters
}

@defproc[(DrawModelPoints
          [model _Model]
          [position _Vector3]
          [scale _float]
          [tint _Color])
         _void]{
Draw a model as points
}

@defproc[(DrawModelPointsEx
          [model _Model]
          [position _Vector3]
          [rotationAxis _Vector3]
          [rotationAngle _float]
          [scale _Vector3]
          [tint _Color])
         _void]{
Draw a model as points with extended parameters
}

@defproc[(DrawBoundingBox
          [box _BoundingBox]
          [color _Color])
         _void]{
Draw bounding box (wires)
}

@defproc[(DrawBillboard
          [camera _Camera]
          [texture _Texture2D]
          [position _Vector3]
          [scale _float]
          [tint _Color])
         _void]{
Draw a billboard texture
}

@defproc[(DrawBillboardRec
          [camera _Camera]
          [texture _Texture2D]
          [source _Rectangle]
          [position _Vector3]
          [size _Vector2]
          [tint _Color])
         _void]{
Draw a billboard texture defined by source
}

@defproc[(DrawBillboardPro
          [camera _Camera]
          [texture _Texture2D]
          [source _Rectangle]
          [position _Vector3]
          [up _Vector3]
          [size _Vector2]
          [origin _Vector2]
          [rotation _float]
          [tint _Color])
         _void]{
Draw a billboard texture defined by source and rotation
}

@defproc[(UploadMesh
          [mesh (_pointer-to _Mesh)]
          [dynamic _stdbool])
         _void]{
Upload mesh vertex data in GPU and provide VAO/VBO ids
}

@defproc[(UpdateMeshBuffer
          [mesh _Mesh]
          [index _int]
          [data (_pointer-to _void)]
          [dataSize _int]
          [offset _int])
         _void]{
Update mesh vertex data in GPU for a specific buffer index
}

@defproc[(UnloadMesh
          [mesh _Mesh])
         _void]{
Unload mesh data from CPU and GPU
}

@defproc[(DrawMesh
          [mesh _Mesh]
          [material _Material]
          [transform _Matrix])
         _void]{
Draw a 3d mesh with material and transform
}

@defproc[(DrawMeshInstanced
          [mesh _Mesh]
          [material _Material]
          [transforms (_pointer-to _Matrix)]
          [instances _int])
         _void]{
Draw multiple mesh instances with material and different transforms
}

@defproc[(GetMeshBoundingBox
          [mesh _Mesh])
         _BoundingBox]{
Compute mesh bounding box limits
}

@defproc[(GenMeshTangents
          [mesh (_pointer-to _Mesh)])
         _void]{
Compute mesh tangents
}

@defproc[(ExportMesh
          [mesh _Mesh]
          [fileName _string])
         _stdbool]{
Export mesh data to file, returns true on success
}

@defproc[(ExportMeshAsCode
          [mesh _Mesh]
          [fileName _string])
         _stdbool]{
Export mesh as code file (.h) defining multiple arrays of vertex attributes
}

@defproc[(GenMeshPoly
          [sides _int]
          [radius _float])
         _Mesh]{
Generate polygonal mesh
}

@defproc[(GenMeshPlane
          [width _float]
          [length _float]
          [resX _int]
          [resZ _int])
         _Mesh]{
Generate plane mesh (with subdivisions)
}

@defproc[(GenMeshCube
          [width _float]
          [height _float]
          [length _float])
         _Mesh]{
Generate cuboid mesh
}

@defproc[(GenMeshSphere
          [radius _float]
          [rings _int]
          [slices _int])
         _Mesh]{
Generate sphere mesh (standard sphere)
}

@defproc[(GenMeshHemiSphere
          [radius _float]
          [rings _int]
          [slices _int])
         _Mesh]{
Generate half-sphere mesh (no bottom cap)
}

@defproc[(GenMeshCylinder
          [radius _float]
          [height _float]
          [slices _int])
         _Mesh]{
Generate cylinder mesh
}

@defproc[(GenMeshCone
          [radius _float]
          [height _float]
          [slices _int])
         _Mesh]{
Generate cone/pyramid mesh
}

@defproc[(GenMeshTorus
          [radius _float]
          [size _float]
          [radSeg _int]
          [sides _int])
         _Mesh]{
Generate torus mesh
}

@defproc[(GenMeshKnot
          [radius _float]
          [size _float]
          [radSeg _int]
          [sides _int])
         _Mesh]{
Generate trefoil knot mesh
}

@defproc[(GenMeshHeightmap
          [heightmap _Image]
          [size _Vector3])
         _Mesh]{
Generate heightmap mesh from image data
}

@defproc[(GenMeshCubicmap
          [cubicmap _Image]
          [cubeSize _Vector3])
         _Mesh]{
Generate cubes-based map mesh from image data
}

@defproc[(LoadMaterials
          [fileName _string]
          [materialCount (_pointer-to _int)])
         (_pointer-to _Material)]{
Load materials from model file
}

@defproc[(LoadMaterialDefault)
         _Material]{
Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)
}

@defproc[(IsMaterialValid
          [material _Material])
         _stdbool]{
Check if a material is valid (shader assigned, map textures loaded in GPU)
}

@defproc[(UnloadMaterial
          [material _Material])
         _void]{
Unload material from GPU memory (VRAM)
}

@defproc[(SetMaterialTexture
          [material (_pointer-to _Material)]
          [mapType _int]
          [texture _Texture2D])
         _void]{
Set texture for a material map type (MATERIAL_MAP_DIFFUSE, MATERIAL_MAP_SPECULAR...)
}

@defproc[(SetModelMeshMaterial
          [model (_pointer-to _Model)]
          [meshId _int]
          [materialId _int])
         _void]{
Set material for a mesh
}

@defproc[(LoadModelAnimations
          [fileName _string]
          [animCount (_pointer-to _int)])
         (_pointer-to _ModelAnimation)]{
Load model animations from file
}

@defproc[(UpdateModelAnimation
          [model _Model]
          [anim _ModelAnimation]
          [frame _int])
         _void]{
Update model animation pose (CPU)
}

@defproc[(UpdateModelAnimationBones
          [model _Model]
          [anim _ModelAnimation]
          [frame _int])
         _void]{
Update model animation mesh bone matrices (GPU skinning)
}

@defproc[(UnloadModelAnimation
          [anim _ModelAnimation])
         _void]{
Unload animation data
}

@defproc[(UnloadModelAnimations
          [animations (_pointer-to _ModelAnimation)]
          [animCount _int])
         _void]{
Unload animation array data
}

@defproc[(IsModelAnimationValid
          [model _Model]
          [anim _ModelAnimation])
         _stdbool]{
Check model animation skeleton match
}

@defproc[(CheckCollisionSpheres
          [center1 _Vector3]
          [radius1 _float]
          [center2 _Vector3]
          [radius2 _float])
         _stdbool]{
Check collision between two spheres
}

@defproc[(CheckCollisionBoxes
          [box1 _BoundingBox]
          [box2 _BoundingBox])
         _stdbool]{
Check collision between two bounding boxes
}

@defproc[(CheckCollisionBoxSphere
          [box _BoundingBox]
          [center _Vector3]
          [radius _float])
         _stdbool]{
Check collision between box and sphere
}

@defproc[(GetRayCollisionSphere
          [ray _Ray]
          [center _Vector3]
          [radius _float])
         _RayCollision]{
Get collision info between ray and sphere
}

@defproc[(GetRayCollisionBox
          [ray _Ray]
          [box _BoundingBox])
         _RayCollision]{
Get collision info between ray and box
}

@defproc[(GetRayCollisionMesh
          [ray _Ray]
          [mesh _Mesh]
          [transform _Matrix])
         _RayCollision]{
Get collision info between ray and mesh
}

@defproc[(GetRayCollisionTriangle
          [ray _Ray]
          [p1 _Vector3]
          [p2 _Vector3]
          [p3 _Vector3])
         _RayCollision]{
Get collision info between ray and triangle
}

@defproc[(GetRayCollisionQuad
          [ray _Ray]
          [p1 _Vector3]
          [p2 _Vector3]
          [p3 _Vector3]
          [p4 _Vector3])
         _RayCollision]{
Get collision info between ray and quad
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
         _stdbool]{
Check if audio device has been initialized successfully
}

@defproc[(SetMasterVolume
          [volume _float])
         _void]{
Set master volume (listener)
}

@defproc[(GetMasterVolume)
         _float]{
Get master volume (listener)
}

@defproc[(LoadWave
          [fileName _string])
         _Wave]{
Load wave data from file
}

@defproc[(LoadWaveFromMemory
          [fileType _string]
          [fileData (_pointer-to _ubyte)]
          [dataSize _int])
         _Wave]{
Load wave from memory buffer, fileType refers to extension: i.e. '.wav'
}

@defproc[(IsWaveValid
          [wave _Wave])
         _stdbool]{
Checks if wave data is valid (data loaded and parameters)
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

@defproc[(LoadSoundAlias
          [source _Sound])
         _Sound]{
Create a new sound that shares the same sample data as the source sound, does not own the sound data
}

@defproc[(IsSoundValid
          [sound _Sound])
         _stdbool]{
Checks if a sound is valid (data loaded and buffers initialized)
}

@defproc[(UpdateSound
          [sound _Sound]
          [data (_pointer-to _void)]
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

@defproc[(UnloadSoundAlias
          [alias _Sound])
         _void]{
Unload a sound alias (does not deallocate sample data)
}

@defproc[(ExportWave
          [wave _Wave]
          [fileName _string])
         _stdbool]{
Export wave data to file, returns true on success
}

@defproc[(ExportWaveAsCode
          [wave _Wave]
          [fileName _string])
         _stdbool]{
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

@defproc[(IsSoundPlaying
          [sound _Sound])
         _stdbool]{
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
          [wave (_pointer-to _Wave)]
          [initFrame _int]
          [finalFrame _int])
         _void]{
Crop a wave to defined frames range
}

@defproc[(WaveFormat
          [wave (_pointer-to _Wave)]
          [sampleRate _int]
          [sampleSize _int]
          [channels _int])
         _void]{
Convert wave data to desired format
}

@defproc[(LoadWaveSamples
          [wave _Wave])
         (_pointer-to _float)]{
Load samples data from wave as a 32bit float data array
}

@defproc[(UnloadWaveSamples
          [samples (_pointer-to _float)])
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
          [data (_pointer-to _ubyte)]
          [dataSize _int])
         _Music]{
Load music stream from data
}

@defproc[(IsMusicValid
          [music _Music])
         _stdbool]{
Checks if a music stream is valid (context and buffers initialized)
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
         _stdbool]{
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

@defproc[(IsAudioStreamValid
          [stream _AudioStream])
         _stdbool]{
Checks if an audio stream is valid (buffers initialized)
}

@defproc[(UnloadAudioStream
          [stream _AudioStream])
         _void]{
Unload audio stream and free memory
}

@defproc[(UpdateAudioStream
          [stream _AudioStream]
          [data (_pointer-to _void)]
          [frameCount _int])
         _void]{
Update audio stream buffers with data
}

@defproc[(IsAudioStreamProcessed
          [stream _AudioStream])
         _stdbool]{
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
         _stdbool]{
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
Attach audio stream processor to stream, receives the samples as 'float'
}

@defproc[(DetachAudioStreamProcessor
          [stream _AudioStream]
          [processor _AudioCallback])
         _void]{
Detach audio stream processor from stream
}

@defproc[(AttachAudioMixedProcessor
          [processor _AudioCallback])
         _void]{
Attach audio stream processor to the entire audio pipeline, receives the samples as 'float'
}

@defproc[(DetachAudioMixedProcessor
          [processor _AudioCallback])
         _void]{
Detach audio stream processor from the entire audio pipeline
}
