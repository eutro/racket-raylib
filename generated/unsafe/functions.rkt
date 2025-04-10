#lang racket/base

(require ffi/unsafe ffi/unsafe/define raylib/generated/structs raylib/support)

(define-ffi-definer define-raylib raylib-ffi-lib
  #:provide provide-protected
  #:default-make-fail raylib-make-not-available)

;; Initialize window and OpenGL context
(define-raylib InitWindow
  (_fun
   [width : _int]
   [height : _int]
   [title : _string]
   -> _void))

;; Close window and unload OpenGL context
(define-raylib CloseWindow
  (_fun
   -> _void))

;; Check if application should close (KEY_ESCAPE pressed or windows close icon clicked)
(define-raylib WindowShouldClose
  (_fun
   -> _stdbool))

;; Check if window has been initialized successfully
(define-raylib IsWindowReady
  (_fun
   -> _stdbool))

;; Check if window is currently fullscreen
(define-raylib IsWindowFullscreen
  (_fun
   -> _stdbool))

;; Check if window is currently hidden
(define-raylib IsWindowHidden
  (_fun
   -> _stdbool))

;; Check if window is currently minimized
(define-raylib IsWindowMinimized
  (_fun
   -> _stdbool))

;; Check if window is currently maximized
(define-raylib IsWindowMaximized
  (_fun
   -> _stdbool))

;; Check if window is currently focused
(define-raylib IsWindowFocused
  (_fun
   -> _stdbool))

;; Check if window has been resized last frame
(define-raylib IsWindowResized
  (_fun
   -> _stdbool))

;; Check if one specific window flag is enabled
(define-raylib IsWindowState
  (_fun
   [flag : _uint]
   -> _stdbool))

;; Set window configuration state using flags
(define-raylib SetWindowState
  (_fun
   [flags : _uint]
   -> _void))

;; Clear window configuration state flags
(define-raylib ClearWindowState
  (_fun
   [flags : _uint]
   -> _void))

;; Toggle window state: fullscreen/windowed, resizes monitor to match window resolution
(define-raylib ToggleFullscreen
  (_fun
   -> _void))

;; Toggle window state: borderless windowed, resizes window to match monitor resolution
(define-raylib ToggleBorderlessWindowed
  (_fun
   -> _void))

;; Set window state: maximized, if resizable
(define-raylib MaximizeWindow
  (_fun
   -> _void))

;; Set window state: minimized, if resizable
(define-raylib MinimizeWindow
  (_fun
   -> _void))

;; Set window state: not minimized/maximized
(define-raylib RestoreWindow
  (_fun
   -> _void))

;; Set icon for window (single image, RGBA 32bit)
(define-raylib SetWindowIcon
  (_fun
   [image : _Image]
   -> _void))

;; Set icon for window (multiple images, RGBA 32bit)
(define-raylib SetWindowIcons
  (_fun
   [images : (_pointer-to _Image)]
   [count : _int]
   -> _void))

;; Set title for window
(define-raylib SetWindowTitle
  (_fun
   [title : _string]
   -> _void))

;; Set window position on screen
(define-raylib SetWindowPosition
  (_fun
   [x : _int]
   [y : _int]
   -> _void))

;; Set monitor for the current window
(define-raylib SetWindowMonitor
  (_fun
   [monitor : _int]
   -> _void))

;; Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
(define-raylib SetWindowMinSize
  (_fun
   [width : _int]
   [height : _int]
   -> _void))

;; Set window maximum dimensions (for FLAG_WINDOW_RESIZABLE)
(define-raylib SetWindowMaxSize
  (_fun
   [width : _int]
   [height : _int]
   -> _void))

;; Set window dimensions
(define-raylib SetWindowSize
  (_fun
   [width : _int]
   [height : _int]
   -> _void))

;; Set window opacity [0.0f..1.0f]
(define-raylib SetWindowOpacity
  (_fun
   [opacity : _float]
   -> _void))

;; Set window focused
(define-raylib SetWindowFocused
  (_fun
   -> _void))

;; Get native window handle
(define-raylib GetWindowHandle
  (_fun
   -> (_pointer-to _void)))

;; Get current screen width
(define-raylib GetScreenWidth
  (_fun
   -> _int))

;; Get current screen height
(define-raylib GetScreenHeight
  (_fun
   -> _int))

;; Get current render width (it considers HiDPI)
(define-raylib GetRenderWidth
  (_fun
   -> _int))

;; Get current render height (it considers HiDPI)
(define-raylib GetRenderHeight
  (_fun
   -> _int))

;; Get number of connected monitors
(define-raylib GetMonitorCount
  (_fun
   -> _int))

;; Get current monitor where window is placed
(define-raylib GetCurrentMonitor
  (_fun
   -> _int))

;; Get specified monitor position
(define-raylib GetMonitorPosition
  (_fun
   [monitor : _int]
   -> _Vector2))

;; Get specified monitor width (current video mode used by monitor)
(define-raylib GetMonitorWidth
  (_fun
   [monitor : _int]
   -> _int))

;; Get specified monitor height (current video mode used by monitor)
(define-raylib GetMonitorHeight
  (_fun
   [monitor : _int]
   -> _int))

;; Get specified monitor physical width in millimetres
(define-raylib GetMonitorPhysicalWidth
  (_fun
   [monitor : _int]
   -> _int))

;; Get specified monitor physical height in millimetres
(define-raylib GetMonitorPhysicalHeight
  (_fun
   [monitor : _int]
   -> _int))

;; Get specified monitor refresh rate
(define-raylib GetMonitorRefreshRate
  (_fun
   [monitor : _int]
   -> _int))

;; Get window position XY on monitor
(define-raylib GetWindowPosition
  (_fun
   -> _Vector2))

;; Get window scale DPI factor
(define-raylib GetWindowScaleDPI
  (_fun
   -> _Vector2))

;; Get the human-readable, UTF-8 encoded name of the specified monitor
(define-raylib GetMonitorName
  (_fun
   [monitor : _int]
   -> _string))

;; Set clipboard text content
(define-raylib SetClipboardText
  (_fun
   [text : _string]
   -> _void))

;; Get clipboard text content
(define-raylib GetClipboardText
  (_fun
   -> _string))

;; Get clipboard image content
(define-raylib GetClipboardImage
  (_fun
   -> _Image))

;; Enable waiting for events on EndDrawing(), no automatic event polling
(define-raylib EnableEventWaiting
  (_fun
   -> _void))

;; Disable waiting for events on EndDrawing(), automatic events polling
(define-raylib DisableEventWaiting
  (_fun
   -> _void))

;; Shows cursor
(define-raylib ShowCursor
  (_fun
   -> _void))

;; Hides cursor
(define-raylib HideCursor
  (_fun
   -> _void))

;; Check if cursor is not visible
(define-raylib IsCursorHidden
  (_fun
   -> _stdbool))

;; Enables cursor (unlock cursor)
(define-raylib EnableCursor
  (_fun
   -> _void))

;; Disables cursor (lock cursor)
(define-raylib DisableCursor
  (_fun
   -> _void))

;; Check if cursor is on the screen
(define-raylib IsCursorOnScreen
  (_fun
   -> _stdbool))

;; Set background color (framebuffer clear color)
(define-raylib ClearBackground
  (_fun
   [color : _Color]
   -> _void))

;; Setup canvas (framebuffer) to start drawing
(define-raylib BeginDrawing
  (_fun
   -> _void))

;; End canvas drawing and swap buffers (double buffering)
(define-raylib EndDrawing
  (_fun
   -> _void))

;; Begin 2D mode with custom camera (2D)
(define-raylib BeginMode2D
  (_fun
   [camera : _Camera2D]
   -> _void))

;; Ends 2D mode with custom camera
(define-raylib EndMode2D
  (_fun
   -> _void))

;; Begin 3D mode with custom camera (3D)
(define-raylib BeginMode3D
  (_fun
   [camera : _Camera3D]
   -> _void))

;; Ends 3D mode and returns to default 2D orthographic mode
(define-raylib EndMode3D
  (_fun
   -> _void))

;; Begin drawing to render texture
(define-raylib BeginTextureMode
  (_fun
   [target : _RenderTexture2D]
   -> _void))

;; Ends drawing to render texture
(define-raylib EndTextureMode
  (_fun
   -> _void))

;; Begin custom shader drawing
(define-raylib BeginShaderMode
  (_fun
   [shader : _Shader]
   -> _void))

;; End custom shader drawing (use default shader)
(define-raylib EndShaderMode
  (_fun
   -> _void))

;; Begin blending mode (alpha, additive, multiplied, subtract, custom)
(define-raylib BeginBlendMode
  (_fun
   [mode : _int]
   -> _void))

;; End blending mode (reset to default: alpha blending)
(define-raylib EndBlendMode
  (_fun
   -> _void))

;; Begin scissor mode (define screen area for following drawing)
(define-raylib BeginScissorMode
  (_fun
   [x : _int]
   [y : _int]
   [width : _int]
   [height : _int]
   -> _void))

;; End scissor mode
(define-raylib EndScissorMode
  (_fun
   -> _void))

;; Begin stereo rendering (requires VR simulator)
(define-raylib BeginVrStereoMode
  (_fun
   [config : _VrStereoConfig]
   -> _void))

;; End stereo rendering (requires VR simulator)
(define-raylib EndVrStereoMode
  (_fun
   -> _void))

;; Load VR stereo config for VR simulator device parameters
(define-raylib LoadVrStereoConfig
  (_fun
   [device : _VrDeviceInfo]
   -> _VrStereoConfig))

;; Unload VR stereo config
(define-raylib UnloadVrStereoConfig
  (_fun
   [config : _VrStereoConfig]
   -> _void))

;; Load shader from files and bind default locations
(define-raylib LoadShader
  (_fun
   [vsFileName : _string]
   [fsFileName : _string]
   -> _Shader))

;; Load shader from code strings and bind default locations
(define-raylib LoadShaderFromMemory
  (_fun
   [vsCode : _string]
   [fsCode : _string]
   -> _Shader))

;; Check if a shader is valid (loaded on GPU)
(define-raylib IsShaderValid
  (_fun
   [shader : _Shader]
   -> _stdbool))

;; Get shader uniform location
(define-raylib GetShaderLocation
  (_fun
   [shader : _Shader]
   [uniformName : _string]
   -> _int))

;; Get shader attribute location
(define-raylib GetShaderLocationAttrib
  (_fun
   [shader : _Shader]
   [attribName : _string]
   -> _int))

;; Set shader uniform value
(define-raylib SetShaderValue
  (_fun
   [shader : _Shader]
   [locIndex : _int]
   [value : (_pointer-to _void)]
   [uniformType : _int]
   -> _void))

;; Set shader uniform value vector
(define-raylib SetShaderValueV
  (_fun
   [shader : _Shader]
   [locIndex : _int]
   [value : (_pointer-to _void)]
   [uniformType : _int]
   [count : _int]
   -> _void))

;; Set shader uniform value (matrix 4x4)
(define-raylib SetShaderValueMatrix
  (_fun
   [shader : _Shader]
   [locIndex : _int]
   [mat : _Matrix]
   -> _void))

;; Set shader uniform value for texture (sampler2d)
(define-raylib SetShaderValueTexture
  (_fun
   [shader : _Shader]
   [locIndex : _int]
   [texture : _Texture2D]
   -> _void))

;; Unload shader from GPU memory (VRAM)
(define-raylib UnloadShader
  (_fun
   [shader : _Shader]
   -> _void))

;; Get a ray trace from screen position (i.e mouse)
(define-raylib GetScreenToWorldRay
  (_fun
   [position : _Vector2]
   [camera : _Camera]
   -> _Ray))

;; Get a ray trace from screen position (i.e mouse) in a viewport
(define-raylib GetScreenToWorldRayEx
  (_fun
   [position : _Vector2]
   [camera : _Camera]
   [width : _int]
   [height : _int]
   -> _Ray))

;; Get the screen space position for a 3d world space position
(define-raylib GetWorldToScreen
  (_fun
   [position : _Vector3]
   [camera : _Camera]
   -> _Vector2))

;; Get size position for a 3d world space position
(define-raylib GetWorldToScreenEx
  (_fun
   [position : _Vector3]
   [camera : _Camera]
   [width : _int]
   [height : _int]
   -> _Vector2))

;; Get the screen space position for a 2d camera world space position
(define-raylib GetWorldToScreen2D
  (_fun
   [position : _Vector2]
   [camera : _Camera2D]
   -> _Vector2))

;; Get the world space position for a 2d camera screen space position
(define-raylib GetScreenToWorld2D
  (_fun
   [position : _Vector2]
   [camera : _Camera2D]
   -> _Vector2))

;; Get camera transform matrix (view matrix)
(define-raylib GetCameraMatrix
  (_fun
   [camera : _Camera]
   -> _Matrix))

;; Get camera 2d transform matrix
(define-raylib GetCameraMatrix2D
  (_fun
   [camera : _Camera2D]
   -> _Matrix))

;; Set target FPS (maximum)
(define-raylib SetTargetFPS
  (_fun
   [fps : _int]
   -> _void))

;; Get time in seconds for last frame drawn (delta time)
(define-raylib GetFrameTime
  (_fun
   -> _float))

;; Get elapsed time in seconds since InitWindow()
(define-raylib GetTime
  (_fun
   -> _double))

;; Get current FPS
(define-raylib GetFPS
  (_fun
   -> _int))

;; Swap back buffer with front buffer (screen drawing)
(define-raylib SwapScreenBuffer
  (_fun
   -> _void))

;; Register all input events
(define-raylib PollInputEvents
  (_fun
   -> _void))

;; Wait for some time (halt program execution)
(define-raylib WaitTime
  (_fun
   [seconds : _double]
   -> _void))

;; Set the seed for the random number generator
(define-raylib SetRandomSeed
  (_fun
   [seed : _uint]
   -> _void))

;; Get a random value between min and max (both included)
(define-raylib GetRandomValue
  (_fun
   [min : _int]
   [max : _int]
   -> _int))

;; Load random values sequence, no values repeated
(define-raylib LoadRandomSequence
  (_fun
   [count : _uint]
   [min : _int]
   [max : _int]
   -> (_pointer-to _int)))

;; Unload random values sequence
(define-raylib UnloadRandomSequence
  (_fun
   [sequence : (_pointer-to _int)]
   -> _void))

;; Takes a screenshot of current screen (filename extension defines format)
(define-raylib TakeScreenshot
  (_fun
   [fileName : _string]
   -> _void))

;; Setup init configuration flags (view FLAGS)
(define-raylib SetConfigFlags
  (_fun
   [flags : _uint]
   -> _void))

;; Open URL with default system browser (if available)
(define-raylib OpenURL
  (_fun
   [url : _string]
   -> _void))

;; Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR...)
(define-raylib TraceLog
  (_fun
   #:varargs-after 2
   [logLevel : _int]
   [text : _string]
   ;; ... varargs
   -> _void))

;; Set the current threshold (minimum) log level
(define-raylib SetTraceLogLevel
  (_fun
   [logLevel : _int]
   -> _void))

;; Internal memory allocator
(define-raylib MemAlloc
  (_fun
   [size : _uint]
   -> (_pointer-to _void)))

;; Internal memory reallocator
(define-raylib MemRealloc
  (_fun
   [ptr : (_pointer-to _void)]
   [size : _uint]
   -> (_pointer-to _void)))

;; Internal memory free
(define-raylib MemFree
  (_fun
   [ptr : (_pointer-to _void)]
   -> _void))

;; Set custom trace log
(define-raylib SetTraceLogCallback
  (_fun
   [callback : _TraceLogCallback]
   -> _void))

;; Set custom file binary data loader
(define-raylib SetLoadFileDataCallback
  (_fun
   [callback : _LoadFileDataCallback]
   -> _void))

;; Set custom file binary data saver
(define-raylib SetSaveFileDataCallback
  (_fun
   [callback : _SaveFileDataCallback]
   -> _void))

;; Set custom file text data loader
(define-raylib SetLoadFileTextCallback
  (_fun
   [callback : _LoadFileTextCallback]
   -> _void))

;; Set custom file text data saver
(define-raylib SetSaveFileTextCallback
  (_fun
   [callback : _SaveFileTextCallback]
   -> _void))

;; Load file data as byte array (read)
(define-raylib LoadFileData
  (_fun
   [fileName : _string]
   [dataSize : (_pointer-to _int)]
   -> (_pointer-to _ubyte)))

;; Unload file data allocated by LoadFileData()
(define-raylib UnloadFileData
  (_fun
   [data : (_pointer-to _ubyte)]
   -> _void))

;; Save data to file from byte array (write), returns true on success
(define-raylib SaveFileData
  (_fun
   [fileName : _string]
   [data : (_pointer-to _void)]
   [dataSize : _int]
   -> _stdbool))

;; Export data to code (.h), returns true on success
(define-raylib ExportDataAsCode
  (_fun
   [data : (_pointer-to _ubyte)]
   [dataSize : _int]
   [fileName : _string]
   -> _stdbool))

;; Load text data from file (read), returns a '\0' terminated string
(define-raylib LoadFileText
  (_fun
   [fileName : _string]
   -> (_pointer-to _byte)))

;; Unload file text data allocated by LoadFileText()
(define-raylib UnloadFileText
  (_fun
   [text : (_pointer-to _byte)]
   -> _void))

;; Save text data to file (write), string must be '\0' terminated, returns true on success
(define-raylib SaveFileText
  (_fun
   [fileName : _string]
   [text : (_pointer-to _byte)]
   -> _stdbool))

;; Check if file exists
(define-raylib FileExists
  (_fun
   [fileName : _string]
   -> _stdbool))

;; Check if a directory path exists
(define-raylib DirectoryExists
  (_fun
   [dirPath : _string]
   -> _stdbool))

;; Check file extension (including point: .png, .wav)
(define-raylib IsFileExtension
  (_fun
   [fileName : _string]
   [ext : _string]
   -> _stdbool))

;; Get file length in bytes (NOTE: GetFileSize() conflicts with windows.h)
(define-raylib GetFileLength
  (_fun
   [fileName : _string]
   -> _int))

;; Get pointer to extension for a filename string (includes dot: '.png')
(define-raylib GetFileExtension
  (_fun
   [fileName : _string]
   -> _string))

;; Get pointer to filename for a path string
(define-raylib GetFileName
  (_fun
   [filePath : _string]
   -> _string))

;; Get filename string without extension (uses static string)
(define-raylib GetFileNameWithoutExt
  (_fun
   [filePath : _string]
   -> _string))

;; Get full path for a given fileName with path (uses static string)
(define-raylib GetDirectoryPath
  (_fun
   [filePath : _string]
   -> _string))

;; Get previous directory path for a given path (uses static string)
(define-raylib GetPrevDirectoryPath
  (_fun
   [dirPath : _string]
   -> _string))

;; Get current working directory (uses static string)
(define-raylib GetWorkingDirectory
  (_fun
   -> _string))

;; Get the directory of the running application (uses static string)
(define-raylib GetApplicationDirectory
  (_fun
   -> _string))

;; Create directories (including full path requested), returns 0 on success
(define-raylib MakeDirectory
  (_fun
   [dirPath : _string]
   -> _int))

;; Change working directory, return true on success
(define-raylib ChangeDirectory
  (_fun
   [dir : _string]
   -> _stdbool))

;; Check if a given path is a file or a directory
(define-raylib IsPathFile
  (_fun
   [path : _string]
   -> _stdbool))

;; Check if fileName is valid for the platform/OS
(define-raylib IsFileNameValid
  (_fun
   [fileName : _string]
   -> _stdbool))

;; Load directory filepaths
(define-raylib LoadDirectoryFiles
  (_fun
   [dirPath : _string]
   -> _FilePathList))

;; Load directory filepaths with extension filtering and recursive directory scan. Use 'DIR' in the filter string to include directories in the result
(define-raylib LoadDirectoryFilesEx
  (_fun
   [basePath : _string]
   [filter : _string]
   [scanSubdirs : _stdbool]
   -> _FilePathList))

;; Unload filepaths
(define-raylib UnloadDirectoryFiles
  (_fun
   [files : _FilePathList]
   -> _void))

;; Check if a file has been dropped into window
(define-raylib IsFileDropped
  (_fun
   -> _stdbool))

;; Load dropped filepaths
(define-raylib LoadDroppedFiles
  (_fun
   -> _FilePathList))

;; Unload dropped filepaths
(define-raylib UnloadDroppedFiles
  (_fun
   [files : _FilePathList]
   -> _void))

;; Get file modification time (last write time)
(define-raylib GetFileModTime
  (_fun
   [fileName : _string]
   -> _long))

;; Compress data (DEFLATE algorithm), memory must be MemFree()
(define-raylib CompressData
  (_fun
   [data : (_pointer-to _ubyte)]
   [dataSize : _int]
   [compDataSize : (_pointer-to _int)]
   -> (_pointer-to _ubyte)))

;; Decompress data (DEFLATE algorithm), memory must be MemFree()
(define-raylib DecompressData
  (_fun
   [compData : (_pointer-to _ubyte)]
   [compDataSize : _int]
   [dataSize : (_pointer-to _int)]
   -> (_pointer-to _ubyte)))

;; Encode data to Base64 string, memory must be MemFree()
(define-raylib EncodeDataBase64
  (_fun
   [data : (_pointer-to _ubyte)]
   [dataSize : _int]
   [outputSize : (_pointer-to _int)]
   -> (_pointer-to _byte)))

;; Decode Base64 string data, memory must be MemFree()
(define-raylib DecodeDataBase64
  (_fun
   [data : (_pointer-to _ubyte)]
   [outputSize : (_pointer-to _int)]
   -> (_pointer-to _ubyte)))

;; Compute CRC32 hash code
(define-raylib ComputeCRC32
  (_fun
   [data : (_pointer-to _ubyte)]
   [dataSize : _int]
   -> _uint))

;; Compute MD5 hash code, returns static int[4] (16 bytes)
(define-raylib ComputeMD5
  (_fun
   [data : (_pointer-to _ubyte)]
   [dataSize : _int]
   -> (_pointer-to _uint)))

;; Compute SHA1 hash code, returns static int[5] (20 bytes)
(define-raylib ComputeSHA1
  (_fun
   [data : (_pointer-to _ubyte)]
   [dataSize : _int]
   -> (_pointer-to _uint)))

;; Load automation events list from file, NULL for empty list, capacity = MAX_AUTOMATION_EVENTS
(define-raylib LoadAutomationEventList
  (_fun
   [fileName : _string]
   -> _AutomationEventList))

;; Unload automation events list from file
(define-raylib UnloadAutomationEventList
  (_fun
   [list : _AutomationEventList]
   -> _void))

;; Export automation events list as text file
(define-raylib ExportAutomationEventList
  (_fun
   [list : _AutomationEventList]
   [fileName : _string]
   -> _stdbool))

;; Set automation event list to record to
(define-raylib SetAutomationEventList
  (_fun
   [list : (_pointer-to _AutomationEventList)]
   -> _void))

;; Set automation event internal base frame to start recording
(define-raylib SetAutomationEventBaseFrame
  (_fun
   [frame : _int]
   -> _void))

;; Start recording automation events (AutomationEventList must be set)
(define-raylib StartAutomationEventRecording
  (_fun
   -> _void))

;; Stop recording automation events
(define-raylib StopAutomationEventRecording
  (_fun
   -> _void))

;; Play a recorded automation event
(define-raylib PlayAutomationEvent
  (_fun
   [event : _AutomationEvent]
   -> _void))

;; Check if a key has been pressed once
(define-raylib IsKeyPressed
  (_fun
   [key : _int]
   -> _stdbool))

;; Check if a key has been pressed again
(define-raylib IsKeyPressedRepeat
  (_fun
   [key : _int]
   -> _stdbool))

;; Check if a key is being pressed
(define-raylib IsKeyDown
  (_fun
   [key : _int]
   -> _stdbool))

;; Check if a key has been released once
(define-raylib IsKeyReleased
  (_fun
   [key : _int]
   -> _stdbool))

;; Check if a key is NOT being pressed
(define-raylib IsKeyUp
  (_fun
   [key : _int]
   -> _stdbool))

;; Get key pressed (keycode), call it multiple times for keys queued, returns 0 when the queue is empty
(define-raylib GetKeyPressed
  (_fun
   -> _int))

;; Get char pressed (unicode), call it multiple times for chars queued, returns 0 when the queue is empty
(define-raylib GetCharPressed
  (_fun
   -> _int))

;; Set a custom key to exit program (default is ESC)
(define-raylib SetExitKey
  (_fun
   [key : _int]
   -> _void))

;; Check if a gamepad is available
(define-raylib IsGamepadAvailable
  (_fun
   [gamepad : _int]
   -> _stdbool))

;; Get gamepad internal name id
(define-raylib GetGamepadName
  (_fun
   [gamepad : _int]
   -> _string))

;; Check if a gamepad button has been pressed once
(define-raylib IsGamepadButtonPressed
  (_fun
   [gamepad : _int]
   [button : _int]
   -> _stdbool))

;; Check if a gamepad button is being pressed
(define-raylib IsGamepadButtonDown
  (_fun
   [gamepad : _int]
   [button : _int]
   -> _stdbool))

;; Check if a gamepad button has been released once
(define-raylib IsGamepadButtonReleased
  (_fun
   [gamepad : _int]
   [button : _int]
   -> _stdbool))

;; Check if a gamepad button is NOT being pressed
(define-raylib IsGamepadButtonUp
  (_fun
   [gamepad : _int]
   [button : _int]
   -> _stdbool))

;; Get the last gamepad button pressed
(define-raylib GetGamepadButtonPressed
  (_fun
   -> _int))

;; Get gamepad axis count for a gamepad
(define-raylib GetGamepadAxisCount
  (_fun
   [gamepad : _int]
   -> _int))

;; Get axis movement value for a gamepad axis
(define-raylib GetGamepadAxisMovement
  (_fun
   [gamepad : _int]
   [axis : _int]
   -> _float))

;; Set internal gamepad mappings (SDL_GameControllerDB)
(define-raylib SetGamepadMappings
  (_fun
   [mappings : _string]
   -> _int))

;; Set gamepad vibration for both motors (duration in seconds)
(define-raylib SetGamepadVibration
  (_fun
   [gamepad : _int]
   [leftMotor : _float]
   [rightMotor : _float]
   [duration : _float]
   -> _void))

;; Check if a mouse button has been pressed once
(define-raylib IsMouseButtonPressed
  (_fun
   [button : _int]
   -> _stdbool))

;; Check if a mouse button is being pressed
(define-raylib IsMouseButtonDown
  (_fun
   [button : _int]
   -> _stdbool))

;; Check if a mouse button has been released once
(define-raylib IsMouseButtonReleased
  (_fun
   [button : _int]
   -> _stdbool))

;; Check if a mouse button is NOT being pressed
(define-raylib IsMouseButtonUp
  (_fun
   [button : _int]
   -> _stdbool))

;; Get mouse position X
(define-raylib GetMouseX
  (_fun
   -> _int))

;; Get mouse position Y
(define-raylib GetMouseY
  (_fun
   -> _int))

;; Get mouse position XY
(define-raylib GetMousePosition
  (_fun
   -> _Vector2))

;; Get mouse delta between frames
(define-raylib GetMouseDelta
  (_fun
   -> _Vector2))

;; Set mouse position XY
(define-raylib SetMousePosition
  (_fun
   [x : _int]
   [y : _int]
   -> _void))

;; Set mouse offset
(define-raylib SetMouseOffset
  (_fun
   [offsetX : _int]
   [offsetY : _int]
   -> _void))

;; Set mouse scaling
(define-raylib SetMouseScale
  (_fun
   [scaleX : _float]
   [scaleY : _float]
   -> _void))

;; Get mouse wheel movement for X or Y, whichever is larger
(define-raylib GetMouseWheelMove
  (_fun
   -> _float))

;; Get mouse wheel movement for both X and Y
(define-raylib GetMouseWheelMoveV
  (_fun
   -> _Vector2))

;; Set mouse cursor
(define-raylib SetMouseCursor
  (_fun
   [cursor : _int]
   -> _void))

;; Get touch position X for touch point 0 (relative to screen size)
(define-raylib GetTouchX
  (_fun
   -> _int))

;; Get touch position Y for touch point 0 (relative to screen size)
(define-raylib GetTouchY
  (_fun
   -> _int))

;; Get touch position XY for a touch point index (relative to screen size)
(define-raylib GetTouchPosition
  (_fun
   [index : _int]
   -> _Vector2))

;; Get touch point identifier for given index
(define-raylib GetTouchPointId
  (_fun
   [index : _int]
   -> _int))

;; Get number of touch points
(define-raylib GetTouchPointCount
  (_fun
   -> _int))

;; Enable a set of gestures using flags
(define-raylib SetGesturesEnabled
  (_fun
   [flags : _uint]
   -> _void))

;; Check if a gesture have been detected
(define-raylib IsGestureDetected
  (_fun
   [gesture : _uint]
   -> _stdbool))

;; Get latest detected gesture
(define-raylib GetGestureDetected
  (_fun
   -> _int))

;; Get gesture hold time in seconds
(define-raylib GetGestureHoldDuration
  (_fun
   -> _float))

;; Get gesture drag vector
(define-raylib GetGestureDragVector
  (_fun
   -> _Vector2))

;; Get gesture drag angle
(define-raylib GetGestureDragAngle
  (_fun
   -> _float))

;; Get gesture pinch delta
(define-raylib GetGesturePinchVector
  (_fun
   -> _Vector2))

;; Get gesture pinch angle
(define-raylib GetGesturePinchAngle
  (_fun
   -> _float))

;; Update camera position for selected mode
(define-raylib UpdateCamera
  (_fun
   [camera : (_pointer-to _Camera)]
   [mode : _int]
   -> _void))

;; Update camera movement/rotation
(define-raylib UpdateCameraPro
  (_fun
   [camera : (_pointer-to _Camera)]
   [movement : _Vector3]
   [rotation : _Vector3]
   [zoom : _float]
   -> _void))

;; Set texture and rectangle to be used on shapes drawing
(define-raylib SetShapesTexture
  (_fun
   [texture : _Texture2D]
   [source : _Rectangle]
   -> _void))

;; Get texture that is used for shapes drawing
(define-raylib GetShapesTexture
  (_fun
   -> _Texture2D))

;; Get texture source rectangle that is used for shapes drawing
(define-raylib GetShapesTextureRectangle
  (_fun
   -> _Rectangle))

;; Draw a pixel using geometry [Can be slow, use with care]
(define-raylib DrawPixel
  (_fun
   [posX : _int]
   [posY : _int]
   [color : _Color]
   -> _void))

;; Draw a pixel using geometry (Vector version) [Can be slow, use with care]
(define-raylib DrawPixelV
  (_fun
   [position : _Vector2]
   [color : _Color]
   -> _void))

;; Draw a line
(define-raylib DrawLine
  (_fun
   [startPosX : _int]
   [startPosY : _int]
   [endPosX : _int]
   [endPosY : _int]
   [color : _Color]
   -> _void))

;; Draw a line (using gl lines)
(define-raylib DrawLineV
  (_fun
   [startPos : _Vector2]
   [endPos : _Vector2]
   [color : _Color]
   -> _void))

;; Draw a line (using triangles/quads)
(define-raylib DrawLineEx
  (_fun
   [startPos : _Vector2]
   [endPos : _Vector2]
   [thick : _float]
   [color : _Color]
   -> _void))

;; Draw lines sequence (using gl lines)
(define-raylib DrawLineStrip
  (_fun
   [points : (_pointer-to _Vector2)]
   [pointCount : _int]
   [color : _Color]
   -> _void))

;; Draw line segment cubic-bezier in-out interpolation
(define-raylib DrawLineBezier
  (_fun
   [startPos : _Vector2]
   [endPos : _Vector2]
   [thick : _float]
   [color : _Color]
   -> _void))

;; Draw a color-filled circle
(define-raylib DrawCircle
  (_fun
   [centerX : _int]
   [centerY : _int]
   [radius : _float]
   [color : _Color]
   -> _void))

;; Draw a piece of a circle
(define-raylib DrawCircleSector
  (_fun
   [center : _Vector2]
   [radius : _float]
   [startAngle : _float]
   [endAngle : _float]
   [segments : _int]
   [color : _Color]
   -> _void))

;; Draw circle sector outline
(define-raylib DrawCircleSectorLines
  (_fun
   [center : _Vector2]
   [radius : _float]
   [startAngle : _float]
   [endAngle : _float]
   [segments : _int]
   [color : _Color]
   -> _void))

;; Draw a gradient-filled circle
(define-raylib DrawCircleGradient
  (_fun
   [centerX : _int]
   [centerY : _int]
   [radius : _float]
   [inner : _Color]
   [outer : _Color]
   -> _void))

;; Draw a color-filled circle (Vector version)
(define-raylib DrawCircleV
  (_fun
   [center : _Vector2]
   [radius : _float]
   [color : _Color]
   -> _void))

;; Draw circle outline
(define-raylib DrawCircleLines
  (_fun
   [centerX : _int]
   [centerY : _int]
   [radius : _float]
   [color : _Color]
   -> _void))

;; Draw circle outline (Vector version)
(define-raylib DrawCircleLinesV
  (_fun
   [center : _Vector2]
   [radius : _float]
   [color : _Color]
   -> _void))

;; Draw ellipse
(define-raylib DrawEllipse
  (_fun
   [centerX : _int]
   [centerY : _int]
   [radiusH : _float]
   [radiusV : _float]
   [color : _Color]
   -> _void))

;; Draw ellipse outline
(define-raylib DrawEllipseLines
  (_fun
   [centerX : _int]
   [centerY : _int]
   [radiusH : _float]
   [radiusV : _float]
   [color : _Color]
   -> _void))

;; Draw ring
(define-raylib DrawRing
  (_fun
   [center : _Vector2]
   [innerRadius : _float]
   [outerRadius : _float]
   [startAngle : _float]
   [endAngle : _float]
   [segments : _int]
   [color : _Color]
   -> _void))

;; Draw ring outline
(define-raylib DrawRingLines
  (_fun
   [center : _Vector2]
   [innerRadius : _float]
   [outerRadius : _float]
   [startAngle : _float]
   [endAngle : _float]
   [segments : _int]
   [color : _Color]
   -> _void))

;; Draw a color-filled rectangle
(define-raylib DrawRectangle
  (_fun
   [posX : _int]
   [posY : _int]
   [width : _int]
   [height : _int]
   [color : _Color]
   -> _void))

;; Draw a color-filled rectangle (Vector version)
(define-raylib DrawRectangleV
  (_fun
   [position : _Vector2]
   [size : _Vector2]
   [color : _Color]
   -> _void))

;; Draw a color-filled rectangle
(define-raylib DrawRectangleRec
  (_fun
   [rec : _Rectangle]
   [color : _Color]
   -> _void))

;; Draw a color-filled rectangle with pro parameters
(define-raylib DrawRectanglePro
  (_fun
   [rec : _Rectangle]
   [origin : _Vector2]
   [rotation : _float]
   [color : _Color]
   -> _void))

;; Draw a vertical-gradient-filled rectangle
(define-raylib DrawRectangleGradientV
  (_fun
   [posX : _int]
   [posY : _int]
   [width : _int]
   [height : _int]
   [top : _Color]
   [bottom : _Color]
   -> _void))

;; Draw a horizontal-gradient-filled rectangle
(define-raylib DrawRectangleGradientH
  (_fun
   [posX : _int]
   [posY : _int]
   [width : _int]
   [height : _int]
   [left : _Color]
   [right : _Color]
   -> _void))

;; Draw a gradient-filled rectangle with custom vertex colors
(define-raylib DrawRectangleGradientEx
  (_fun
   [rec : _Rectangle]
   [topLeft : _Color]
   [bottomLeft : _Color]
   [topRight : _Color]
   [bottomRight : _Color]
   -> _void))

;; Draw rectangle outline
(define-raylib DrawRectangleLines
  (_fun
   [posX : _int]
   [posY : _int]
   [width : _int]
   [height : _int]
   [color : _Color]
   -> _void))

;; Draw rectangle outline with extended parameters
(define-raylib DrawRectangleLinesEx
  (_fun
   [rec : _Rectangle]
   [lineThick : _float]
   [color : _Color]
   -> _void))

;; Draw rectangle with rounded edges
(define-raylib DrawRectangleRounded
  (_fun
   [rec : _Rectangle]
   [roundness : _float]
   [segments : _int]
   [color : _Color]
   -> _void))

;; Draw rectangle lines with rounded edges
(define-raylib DrawRectangleRoundedLines
  (_fun
   [rec : _Rectangle]
   [roundness : _float]
   [segments : _int]
   [color : _Color]
   -> _void))

;; Draw rectangle with rounded edges outline
(define-raylib DrawRectangleRoundedLinesEx
  (_fun
   [rec : _Rectangle]
   [roundness : _float]
   [segments : _int]
   [lineThick : _float]
   [color : _Color]
   -> _void))

;; Draw a color-filled triangle (vertex in counter-clockwise order!)
(define-raylib DrawTriangle
  (_fun
   [v1 : _Vector2]
   [v2 : _Vector2]
   [v3 : _Vector2]
   [color : _Color]
   -> _void))

;; Draw triangle outline (vertex in counter-clockwise order!)
(define-raylib DrawTriangleLines
  (_fun
   [v1 : _Vector2]
   [v2 : _Vector2]
   [v3 : _Vector2]
   [color : _Color]
   -> _void))

;; Draw a triangle fan defined by points (first vertex is the center)
(define-raylib DrawTriangleFan
  (_fun
   [points : (_pointer-to _Vector2)]
   [pointCount : _int]
   [color : _Color]
   -> _void))

;; Draw a triangle strip defined by points
(define-raylib DrawTriangleStrip
  (_fun
   [points : (_pointer-to _Vector2)]
   [pointCount : _int]
   [color : _Color]
   -> _void))

;; Draw a regular polygon (Vector version)
(define-raylib DrawPoly
  (_fun
   [center : _Vector2]
   [sides : _int]
   [radius : _float]
   [rotation : _float]
   [color : _Color]
   -> _void))

;; Draw a polygon outline of n sides
(define-raylib DrawPolyLines
  (_fun
   [center : _Vector2]
   [sides : _int]
   [radius : _float]
   [rotation : _float]
   [color : _Color]
   -> _void))

;; Draw a polygon outline of n sides with extended parameters
(define-raylib DrawPolyLinesEx
  (_fun
   [center : _Vector2]
   [sides : _int]
   [radius : _float]
   [rotation : _float]
   [lineThick : _float]
   [color : _Color]
   -> _void))

;; Draw spline: Linear, minimum 2 points
(define-raylib DrawSplineLinear
  (_fun
   [points : (_pointer-to _Vector2)]
   [pointCount : _int]
   [thick : _float]
   [color : _Color]
   -> _void))

;; Draw spline: B-Spline, minimum 4 points
(define-raylib DrawSplineBasis
  (_fun
   [points : (_pointer-to _Vector2)]
   [pointCount : _int]
   [thick : _float]
   [color : _Color]
   -> _void))

;; Draw spline: Catmull-Rom, minimum 4 points
(define-raylib DrawSplineCatmullRom
  (_fun
   [points : (_pointer-to _Vector2)]
   [pointCount : _int]
   [thick : _float]
   [color : _Color]
   -> _void))

;; Draw spline: Quadratic Bezier, minimum 3 points (1 control point): [p1, c2, p3, c4...]
(define-raylib DrawSplineBezierQuadratic
  (_fun
   [points : (_pointer-to _Vector2)]
   [pointCount : _int]
   [thick : _float]
   [color : _Color]
   -> _void))

;; Draw spline: Cubic Bezier, minimum 4 points (2 control points): [p1, c2, c3, p4, c5, c6...]
(define-raylib DrawSplineBezierCubic
  (_fun
   [points : (_pointer-to _Vector2)]
   [pointCount : _int]
   [thick : _float]
   [color : _Color]
   -> _void))

;; Draw spline segment: Linear, 2 points
(define-raylib DrawSplineSegmentLinear
  (_fun
   [p1 : _Vector2]
   [p2 : _Vector2]
   [thick : _float]
   [color : _Color]
   -> _void))

;; Draw spline segment: B-Spline, 4 points
(define-raylib DrawSplineSegmentBasis
  (_fun
   [p1 : _Vector2]
   [p2 : _Vector2]
   [p3 : _Vector2]
   [p4 : _Vector2]
   [thick : _float]
   [color : _Color]
   -> _void))

;; Draw spline segment: Catmull-Rom, 4 points
(define-raylib DrawSplineSegmentCatmullRom
  (_fun
   [p1 : _Vector2]
   [p2 : _Vector2]
   [p3 : _Vector2]
   [p4 : _Vector2]
   [thick : _float]
   [color : _Color]
   -> _void))

;; Draw spline segment: Quadratic Bezier, 2 points, 1 control point
(define-raylib DrawSplineSegmentBezierQuadratic
  (_fun
   [p1 : _Vector2]
   [c2 : _Vector2]
   [p3 : _Vector2]
   [thick : _float]
   [color : _Color]
   -> _void))

;; Draw spline segment: Cubic Bezier, 2 points, 2 control points
(define-raylib DrawSplineSegmentBezierCubic
  (_fun
   [p1 : _Vector2]
   [c2 : _Vector2]
   [c3 : _Vector2]
   [p4 : _Vector2]
   [thick : _float]
   [color : _Color]
   -> _void))

;; Get (evaluate) spline point: Linear
(define-raylib GetSplinePointLinear
  (_fun
   [startPos : _Vector2]
   [endPos : _Vector2]
   [t : _float]
   -> _Vector2))

;; Get (evaluate) spline point: B-Spline
(define-raylib GetSplinePointBasis
  (_fun
   [p1 : _Vector2]
   [p2 : _Vector2]
   [p3 : _Vector2]
   [p4 : _Vector2]
   [t : _float]
   -> _Vector2))

;; Get (evaluate) spline point: Catmull-Rom
(define-raylib GetSplinePointCatmullRom
  (_fun
   [p1 : _Vector2]
   [p2 : _Vector2]
   [p3 : _Vector2]
   [p4 : _Vector2]
   [t : _float]
   -> _Vector2))

;; Get (evaluate) spline point: Quadratic Bezier
(define-raylib GetSplinePointBezierQuad
  (_fun
   [p1 : _Vector2]
   [c2 : _Vector2]
   [p3 : _Vector2]
   [t : _float]
   -> _Vector2))

;; Get (evaluate) spline point: Cubic Bezier
(define-raylib GetSplinePointBezierCubic
  (_fun
   [p1 : _Vector2]
   [c2 : _Vector2]
   [c3 : _Vector2]
   [p4 : _Vector2]
   [t : _float]
   -> _Vector2))

;; Check collision between two rectangles
(define-raylib CheckCollisionRecs
  (_fun
   [rec1 : _Rectangle]
   [rec2 : _Rectangle]
   -> _stdbool))

;; Check collision between two circles
(define-raylib CheckCollisionCircles
  (_fun
   [center1 : _Vector2]
   [radius1 : _float]
   [center2 : _Vector2]
   [radius2 : _float]
   -> _stdbool))

;; Check collision between circle and rectangle
(define-raylib CheckCollisionCircleRec
  (_fun
   [center : _Vector2]
   [radius : _float]
   [rec : _Rectangle]
   -> _stdbool))

;; Check if circle collides with a line created betweeen two points [p1] and [p2]
(define-raylib CheckCollisionCircleLine
  (_fun
   [center : _Vector2]
   [radius : _float]
   [p1 : _Vector2]
   [p2 : _Vector2]
   -> _stdbool))

;; Check if point is inside rectangle
(define-raylib CheckCollisionPointRec
  (_fun
   [point : _Vector2]
   [rec : _Rectangle]
   -> _stdbool))

;; Check if point is inside circle
(define-raylib CheckCollisionPointCircle
  (_fun
   [point : _Vector2]
   [center : _Vector2]
   [radius : _float]
   -> _stdbool))

;; Check if point is inside a triangle
(define-raylib CheckCollisionPointTriangle
  (_fun
   [point : _Vector2]
   [p1 : _Vector2]
   [p2 : _Vector2]
   [p3 : _Vector2]
   -> _stdbool))

;; Check if point belongs to line created between two points [p1] and [p2] with defined margin in pixels [threshold]
(define-raylib CheckCollisionPointLine
  (_fun
   [point : _Vector2]
   [p1 : _Vector2]
   [p2 : _Vector2]
   [threshold : _int]
   -> _stdbool))

;; Check if point is within a polygon described by array of vertices
(define-raylib CheckCollisionPointPoly
  (_fun
   [point : _Vector2]
   [points : (_pointer-to _Vector2)]
   [pointCount : _int]
   -> _stdbool))

;; Check the collision between two lines defined by two points each, returns collision point by reference
(define-raylib CheckCollisionLines
  (_fun
   [startPos1 : _Vector2]
   [endPos1 : _Vector2]
   [startPos2 : _Vector2]
   [endPos2 : _Vector2]
   [collisionPoint : (_pointer-to _Vector2)]
   -> _stdbool))

;; Get collision rectangle for two rectangles collision
(define-raylib GetCollisionRec
  (_fun
   [rec1 : _Rectangle]
   [rec2 : _Rectangle]
   -> _Rectangle))

;; Load image from file into CPU memory (RAM)
(define-raylib LoadImage
  (_fun
   [fileName : _string]
   -> _Image))

;; Load image from RAW file data
(define-raylib LoadImageRaw
  (_fun
   [fileName : _string]
   [width : _int]
   [height : _int]
   [format : _int]
   [headerSize : _int]
   -> _Image))

;; Load image sequence from file (frames appended to image.data)
(define-raylib LoadImageAnim
  (_fun
   [fileName : _string]
   [frames : (_pointer-to _int)]
   -> _Image))

;; Load image sequence from memory buffer
(define-raylib LoadImageAnimFromMemory
  (_fun
   [fileType : _string]
   [fileData : (_pointer-to _ubyte)]
   [dataSize : _int]
   [frames : (_pointer-to _int)]
   -> _Image))

;; Load image from memory buffer, fileType refers to extension: i.e. '.png'
(define-raylib LoadImageFromMemory
  (_fun
   [fileType : _string]
   [fileData : (_pointer-to _ubyte)]
   [dataSize : _int]
   -> _Image))

;; Load image from GPU texture data
(define-raylib LoadImageFromTexture
  (_fun
   [texture : _Texture2D]
   -> _Image))

;; Load image from screen buffer and (screenshot)
(define-raylib LoadImageFromScreen
  (_fun
   -> _Image))

;; Check if an image is valid (data and parameters)
(define-raylib IsImageValid
  (_fun
   [image : _Image]
   -> _stdbool))

;; Unload image from CPU memory (RAM)
(define-raylib UnloadImage
  (_fun
   [image : _Image]
   -> _void))

;; Export image data to file, returns true on success
(define-raylib ExportImage
  (_fun
   [image : _Image]
   [fileName : _string]
   -> _stdbool))

;; Export image to memory buffer
(define-raylib ExportImageToMemory
  (_fun
   [image : _Image]
   [fileType : _string]
   [fileSize : (_pointer-to _int)]
   -> (_pointer-to _ubyte)))

;; Export image as code file defining an array of bytes, returns true on success
(define-raylib ExportImageAsCode
  (_fun
   [image : _Image]
   [fileName : _string]
   -> _stdbool))

;; Generate image: plain color
(define-raylib GenImageColor
  (_fun
   [width : _int]
   [height : _int]
   [color : _Color]
   -> _Image))

;; Generate image: linear gradient, direction in degrees [0..360], 0=Vertical gradient
(define-raylib GenImageGradientLinear
  (_fun
   [width : _int]
   [height : _int]
   [direction : _int]
   [start : _Color]
   [end : _Color]
   -> _Image))

;; Generate image: radial gradient
(define-raylib GenImageGradientRadial
  (_fun
   [width : _int]
   [height : _int]
   [density : _float]
   [inner : _Color]
   [outer : _Color]
   -> _Image))

;; Generate image: square gradient
(define-raylib GenImageGradientSquare
  (_fun
   [width : _int]
   [height : _int]
   [density : _float]
   [inner : _Color]
   [outer : _Color]
   -> _Image))

;; Generate image: checked
(define-raylib GenImageChecked
  (_fun
   [width : _int]
   [height : _int]
   [checksX : _int]
   [checksY : _int]
   [col1 : _Color]
   [col2 : _Color]
   -> _Image))

;; Generate image: white noise
(define-raylib GenImageWhiteNoise
  (_fun
   [width : _int]
   [height : _int]
   [factor : _float]
   -> _Image))

;; Generate image: perlin noise
(define-raylib GenImagePerlinNoise
  (_fun
   [width : _int]
   [height : _int]
   [offsetX : _int]
   [offsetY : _int]
   [scale : _float]
   -> _Image))

;; Generate image: cellular algorithm, bigger tileSize means bigger cells
(define-raylib GenImageCellular
  (_fun
   [width : _int]
   [height : _int]
   [tileSize : _int]
   -> _Image))

;; Generate image: grayscale image from text data
(define-raylib GenImageText
  (_fun
   [width : _int]
   [height : _int]
   [text : _string]
   -> _Image))

;; Create an image duplicate (useful for transformations)
(define-raylib ImageCopy
  (_fun
   [image : _Image]
   -> _Image))

;; Create an image from another image piece
(define-raylib ImageFromImage
  (_fun
   [image : _Image]
   [rec : _Rectangle]
   -> _Image))

;; Create an image from a selected channel of another image (GRAYSCALE)
(define-raylib ImageFromChannel
  (_fun
   [image : _Image]
   [selectedChannel : _int]
   -> _Image))

;; Create an image from text (default font)
(define-raylib ImageText
  (_fun
   [text : _string]
   [fontSize : _int]
   [color : _Color]
   -> _Image))

;; Create an image from text (custom sprite font)
(define-raylib ImageTextEx
  (_fun
   [font : _Font]
   [text : _string]
   [fontSize : _float]
   [spacing : _float]
   [tint : _Color]
   -> _Image))

;; Convert image data to desired format
(define-raylib ImageFormat
  (_fun
   [image : (_pointer-to _Image)]
   [newFormat : _int]
   -> _void))

;; Convert image to POT (power-of-two)
(define-raylib ImageToPOT
  (_fun
   [image : (_pointer-to _Image)]
   [fill : _Color]
   -> _void))

;; Crop an image to a defined rectangle
(define-raylib ImageCrop
  (_fun
   [image : (_pointer-to _Image)]
   [crop : _Rectangle]
   -> _void))

;; Crop image depending on alpha value
(define-raylib ImageAlphaCrop
  (_fun
   [image : (_pointer-to _Image)]
   [threshold : _float]
   -> _void))

;; Clear alpha channel to desired color
(define-raylib ImageAlphaClear
  (_fun
   [image : (_pointer-to _Image)]
   [color : _Color]
   [threshold : _float]
   -> _void))

;; Apply alpha mask to image
(define-raylib ImageAlphaMask
  (_fun
   [image : (_pointer-to _Image)]
   [alphaMask : _Image]
   -> _void))

;; Premultiply alpha channel
(define-raylib ImageAlphaPremultiply
  (_fun
   [image : (_pointer-to _Image)]
   -> _void))

;; Apply Gaussian blur using a box blur approximation
(define-raylib ImageBlurGaussian
  (_fun
   [image : (_pointer-to _Image)]
   [blurSize : _int]
   -> _void))

;; Apply custom square convolution kernel to image
(define-raylib ImageKernelConvolution
  (_fun
   [image : (_pointer-to _Image)]
   [kernel : (_pointer-to _float)]
   [kernelSize : _int]
   -> _void))

;; Resize image (Bicubic scaling algorithm)
(define-raylib ImageResize
  (_fun
   [image : (_pointer-to _Image)]
   [newWidth : _int]
   [newHeight : _int]
   -> _void))

;; Resize image (Nearest-Neighbor scaling algorithm)
(define-raylib ImageResizeNN
  (_fun
   [image : (_pointer-to _Image)]
   [newWidth : _int]
   [newHeight : _int]
   -> _void))

;; Resize canvas and fill with color
(define-raylib ImageResizeCanvas
  (_fun
   [image : (_pointer-to _Image)]
   [newWidth : _int]
   [newHeight : _int]
   [offsetX : _int]
   [offsetY : _int]
   [fill : _Color]
   -> _void))

;; Compute all mipmap levels for a provided image
(define-raylib ImageMipmaps
  (_fun
   [image : (_pointer-to _Image)]
   -> _void))

;; Dither image data to 16bpp or lower (Floyd-Steinberg dithering)
(define-raylib ImageDither
  (_fun
   [image : (_pointer-to _Image)]
   [rBpp : _int]
   [gBpp : _int]
   [bBpp : _int]
   [aBpp : _int]
   -> _void))

;; Flip image vertically
(define-raylib ImageFlipVertical
  (_fun
   [image : (_pointer-to _Image)]
   -> _void))

;; Flip image horizontally
(define-raylib ImageFlipHorizontal
  (_fun
   [image : (_pointer-to _Image)]
   -> _void))

;; Rotate image by input angle in degrees (-359 to 359)
(define-raylib ImageRotate
  (_fun
   [image : (_pointer-to _Image)]
   [degrees : _int]
   -> _void))

;; Rotate image clockwise 90deg
(define-raylib ImageRotateCW
  (_fun
   [image : (_pointer-to _Image)]
   -> _void))

;; Rotate image counter-clockwise 90deg
(define-raylib ImageRotateCCW
  (_fun
   [image : (_pointer-to _Image)]
   -> _void))

;; Modify image color: tint
(define-raylib ImageColorTint
  (_fun
   [image : (_pointer-to _Image)]
   [color : _Color]
   -> _void))

;; Modify image color: invert
(define-raylib ImageColorInvert
  (_fun
   [image : (_pointer-to _Image)]
   -> _void))

;; Modify image color: grayscale
(define-raylib ImageColorGrayscale
  (_fun
   [image : (_pointer-to _Image)]
   -> _void))

;; Modify image color: contrast (-100 to 100)
(define-raylib ImageColorContrast
  (_fun
   [image : (_pointer-to _Image)]
   [contrast : _float]
   -> _void))

;; Modify image color: brightness (-255 to 255)
(define-raylib ImageColorBrightness
  (_fun
   [image : (_pointer-to _Image)]
   [brightness : _int]
   -> _void))

;; Modify image color: replace color
(define-raylib ImageColorReplace
  (_fun
   [image : (_pointer-to _Image)]
   [color : _Color]
   [replace : _Color]
   -> _void))

;; Load color data from image as a Color array (RGBA - 32bit)
(define-raylib LoadImageColors
  (_fun
   [image : _Image]
   -> (_pointer-to _Color)))

;; Load colors palette from image as a Color array (RGBA - 32bit)
(define-raylib LoadImagePalette
  (_fun
   [image : _Image]
   [maxPaletteSize : _int]
   [colorCount : (_pointer-to _int)]
   -> (_pointer-to _Color)))

;; Unload color data loaded with LoadImageColors()
(define-raylib UnloadImageColors
  (_fun
   [colors : (_pointer-to _Color)]
   -> _void))

;; Unload colors palette loaded with LoadImagePalette()
(define-raylib UnloadImagePalette
  (_fun
   [colors : (_pointer-to _Color)]
   -> _void))

;; Get image alpha border rectangle
(define-raylib GetImageAlphaBorder
  (_fun
   [image : _Image]
   [threshold : _float]
   -> _Rectangle))

;; Get image pixel color at (x, y) position
(define-raylib GetImageColor
  (_fun
   [image : _Image]
   [x : _int]
   [y : _int]
   -> _Color))

;; Clear image background with given color
(define-raylib ImageClearBackground
  (_fun
   [dst : (_pointer-to _Image)]
   [color : _Color]
   -> _void))

;; Draw pixel within an image
(define-raylib ImageDrawPixel
  (_fun
   [dst : (_pointer-to _Image)]
   [posX : _int]
   [posY : _int]
   [color : _Color]
   -> _void))

;; Draw pixel within an image (Vector version)
(define-raylib ImageDrawPixelV
  (_fun
   [dst : (_pointer-to _Image)]
   [position : _Vector2]
   [color : _Color]
   -> _void))

;; Draw line within an image
(define-raylib ImageDrawLine
  (_fun
   [dst : (_pointer-to _Image)]
   [startPosX : _int]
   [startPosY : _int]
   [endPosX : _int]
   [endPosY : _int]
   [color : _Color]
   -> _void))

;; Draw line within an image (Vector version)
(define-raylib ImageDrawLineV
  (_fun
   [dst : (_pointer-to _Image)]
   [start : _Vector2]
   [end : _Vector2]
   [color : _Color]
   -> _void))

;; Draw a line defining thickness within an image
(define-raylib ImageDrawLineEx
  (_fun
   [dst : (_pointer-to _Image)]
   [start : _Vector2]
   [end : _Vector2]
   [thick : _int]
   [color : _Color]
   -> _void))

;; Draw a filled circle within an image
(define-raylib ImageDrawCircle
  (_fun
   [dst : (_pointer-to _Image)]
   [centerX : _int]
   [centerY : _int]
   [radius : _int]
   [color : _Color]
   -> _void))

;; Draw a filled circle within an image (Vector version)
(define-raylib ImageDrawCircleV
  (_fun
   [dst : (_pointer-to _Image)]
   [center : _Vector2]
   [radius : _int]
   [color : _Color]
   -> _void))

;; Draw circle outline within an image
(define-raylib ImageDrawCircleLines
  (_fun
   [dst : (_pointer-to _Image)]
   [centerX : _int]
   [centerY : _int]
   [radius : _int]
   [color : _Color]
   -> _void))

;; Draw circle outline within an image (Vector version)
(define-raylib ImageDrawCircleLinesV
  (_fun
   [dst : (_pointer-to _Image)]
   [center : _Vector2]
   [radius : _int]
   [color : _Color]
   -> _void))

;; Draw rectangle within an image
(define-raylib ImageDrawRectangle
  (_fun
   [dst : (_pointer-to _Image)]
   [posX : _int]
   [posY : _int]
   [width : _int]
   [height : _int]
   [color : _Color]
   -> _void))

;; Draw rectangle within an image (Vector version)
(define-raylib ImageDrawRectangleV
  (_fun
   [dst : (_pointer-to _Image)]
   [position : _Vector2]
   [size : _Vector2]
   [color : _Color]
   -> _void))

;; Draw rectangle within an image
(define-raylib ImageDrawRectangleRec
  (_fun
   [dst : (_pointer-to _Image)]
   [rec : _Rectangle]
   [color : _Color]
   -> _void))

;; Draw rectangle lines within an image
(define-raylib ImageDrawRectangleLines
  (_fun
   [dst : (_pointer-to _Image)]
   [rec : _Rectangle]
   [thick : _int]
   [color : _Color]
   -> _void))

;; Draw triangle within an image
(define-raylib ImageDrawTriangle
  (_fun
   [dst : (_pointer-to _Image)]
   [v1 : _Vector2]
   [v2 : _Vector2]
   [v3 : _Vector2]
   [color : _Color]
   -> _void))

;; Draw triangle with interpolated colors within an image
(define-raylib ImageDrawTriangleEx
  (_fun
   [dst : (_pointer-to _Image)]
   [v1 : _Vector2]
   [v2 : _Vector2]
   [v3 : _Vector2]
   [c1 : _Color]
   [c2 : _Color]
   [c3 : _Color]
   -> _void))

;; Draw triangle outline within an image
(define-raylib ImageDrawTriangleLines
  (_fun
   [dst : (_pointer-to _Image)]
   [v1 : _Vector2]
   [v2 : _Vector2]
   [v3 : _Vector2]
   [color : _Color]
   -> _void))

;; Draw a triangle fan defined by points within an image (first vertex is the center)
(define-raylib ImageDrawTriangleFan
  (_fun
   [dst : (_pointer-to _Image)]
   [points : (_pointer-to _Vector2)]
   [pointCount : _int]
   [color : _Color]
   -> _void))

;; Draw a triangle strip defined by points within an image
(define-raylib ImageDrawTriangleStrip
  (_fun
   [dst : (_pointer-to _Image)]
   [points : (_pointer-to _Vector2)]
   [pointCount : _int]
   [color : _Color]
   -> _void))

;; Draw a source image within a destination image (tint applied to source)
(define-raylib ImageDraw
  (_fun
   [dst : (_pointer-to _Image)]
   [src : _Image]
   [srcRec : _Rectangle]
   [dstRec : _Rectangle]
   [tint : _Color]
   -> _void))

;; Draw text (using default font) within an image (destination)
(define-raylib ImageDrawText
  (_fun
   [dst : (_pointer-to _Image)]
   [text : _string]
   [posX : _int]
   [posY : _int]
   [fontSize : _int]
   [color : _Color]
   -> _void))

;; Draw text (custom sprite font) within an image (destination)
(define-raylib ImageDrawTextEx
  (_fun
   [dst : (_pointer-to _Image)]
   [font : _Font]
   [text : _string]
   [position : _Vector2]
   [fontSize : _float]
   [spacing : _float]
   [tint : _Color]
   -> _void))

;; Load texture from file into GPU memory (VRAM)
(define-raylib LoadTexture
  (_fun
   [fileName : _string]
   -> _Texture2D))

;; Load texture from image data
(define-raylib LoadTextureFromImage
  (_fun
   [image : _Image]
   -> _Texture2D))

;; Load cubemap from image, multiple image cubemap layouts supported
(define-raylib LoadTextureCubemap
  (_fun
   [image : _Image]
   [layout : _int]
   -> _TextureCubemap))

;; Load texture for rendering (framebuffer)
(define-raylib LoadRenderTexture
  (_fun
   [width : _int]
   [height : _int]
   -> _RenderTexture2D))

;; Check if a texture is valid (loaded in GPU)
(define-raylib IsTextureValid
  (_fun
   [texture : _Texture2D]
   -> _stdbool))

;; Unload texture from GPU memory (VRAM)
(define-raylib UnloadTexture
  (_fun
   [texture : _Texture2D]
   -> _void))

;; Check if a render texture is valid (loaded in GPU)
(define-raylib IsRenderTextureValid
  (_fun
   [target : _RenderTexture2D]
   -> _stdbool))

;; Unload render texture from GPU memory (VRAM)
(define-raylib UnloadRenderTexture
  (_fun
   [target : _RenderTexture2D]
   -> _void))

;; Update GPU texture with new data
(define-raylib UpdateTexture
  (_fun
   [texture : _Texture2D]
   [pixels : (_pointer-to _void)]
   -> _void))

;; Update GPU texture rectangle with new data
(define-raylib UpdateTextureRec
  (_fun
   [texture : _Texture2D]
   [rec : _Rectangle]
   [pixels : (_pointer-to _void)]
   -> _void))

;; Generate GPU mipmaps for a texture
(define-raylib GenTextureMipmaps
  (_fun
   [texture : (_pointer-to _Texture2D)]
   -> _void))

;; Set texture scaling filter mode
(define-raylib SetTextureFilter
  (_fun
   [texture : _Texture2D]
   [filter : _int]
   -> _void))

;; Set texture wrapping mode
(define-raylib SetTextureWrap
  (_fun
   [texture : _Texture2D]
   [wrap : _int]
   -> _void))

;; Draw a Texture2D
(define-raylib DrawTexture
  (_fun
   [texture : _Texture2D]
   [posX : _int]
   [posY : _int]
   [tint : _Color]
   -> _void))

;; Draw a Texture2D with position defined as Vector2
(define-raylib DrawTextureV
  (_fun
   [texture : _Texture2D]
   [position : _Vector2]
   [tint : _Color]
   -> _void))

;; Draw a Texture2D with extended parameters
(define-raylib DrawTextureEx
  (_fun
   [texture : _Texture2D]
   [position : _Vector2]
   [rotation : _float]
   [scale : _float]
   [tint : _Color]
   -> _void))

;; Draw a part of a texture defined by a rectangle
(define-raylib DrawTextureRec
  (_fun
   [texture : _Texture2D]
   [source : _Rectangle]
   [position : _Vector2]
   [tint : _Color]
   -> _void))

;; Draw a part of a texture defined by a rectangle with 'pro' parameters
(define-raylib DrawTexturePro
  (_fun
   [texture : _Texture2D]
   [source : _Rectangle]
   [dest : _Rectangle]
   [origin : _Vector2]
   [rotation : _float]
   [tint : _Color]
   -> _void))

;; Draws a texture (or part of it) that stretches or shrinks nicely
(define-raylib DrawTextureNPatch
  (_fun
   [texture : _Texture2D]
   [nPatchInfo : _NPatchInfo]
   [dest : _Rectangle]
   [origin : _Vector2]
   [rotation : _float]
   [tint : _Color]
   -> _void))

;; Check if two colors are equal
(define-raylib ColorIsEqual
  (_fun
   [col1 : _Color]
   [col2 : _Color]
   -> _stdbool))

;; Get color with alpha applied, alpha goes from 0.0f to 1.0f
(define-raylib Fade
  (_fun
   [color : _Color]
   [alpha : _float]
   -> _Color))

;; Get hexadecimal value for a Color (0xRRGGBBAA)
(define-raylib ColorToInt
  (_fun
   [color : _Color]
   -> _int))

;; Get Color normalized as float [0..1]
(define-raylib ColorNormalize
  (_fun
   [color : _Color]
   -> _Vector4))

;; Get Color from normalized values [0..1]
(define-raylib ColorFromNormalized
  (_fun
   [normalized : _Vector4]
   -> _Color))

;; Get HSV values for a Color, hue [0..360], saturation/value [0..1]
(define-raylib ColorToHSV
  (_fun
   [color : _Color]
   -> _Vector3))

;; Get a Color from HSV values, hue [0..360], saturation/value [0..1]
(define-raylib ColorFromHSV
  (_fun
   [hue : _float]
   [saturation : _float]
   [value : _float]
   -> _Color))

;; Get color multiplied with another color
(define-raylib ColorTint
  (_fun
   [color : _Color]
   [tint : _Color]
   -> _Color))

;; Get color with brightness correction, brightness factor goes from -1.0f to 1.0f
(define-raylib ColorBrightness
  (_fun
   [color : _Color]
   [factor : _float]
   -> _Color))

;; Get color with contrast correction, contrast values between -1.0f and 1.0f
(define-raylib ColorContrast
  (_fun
   [color : _Color]
   [contrast : _float]
   -> _Color))

;; Get color with alpha applied, alpha goes from 0.0f to 1.0f
(define-raylib ColorAlpha
  (_fun
   [color : _Color]
   [alpha : _float]
   -> _Color))

;; Get src alpha-blended into dst color with tint
(define-raylib ColorAlphaBlend
  (_fun
   [dst : _Color]
   [src : _Color]
   [tint : _Color]
   -> _Color))

;; Get color lerp interpolation between two colors, factor [0.0f..1.0f]
(define-raylib ColorLerp
  (_fun
   [color1 : _Color]
   [color2 : _Color]
   [factor : _float]
   -> _Color))

;; Get Color structure from hexadecimal value
(define-raylib GetColor
  (_fun
   [hexValue : _uint]
   -> _Color))

;; Get Color from a source pixel pointer of certain format
(define-raylib GetPixelColor
  (_fun
   [srcPtr : (_pointer-to _void)]
   [format : _int]
   -> _Color))

;; Set color formatted into destination pixel pointer
(define-raylib SetPixelColor
  (_fun
   [dstPtr : (_pointer-to _void)]
   [color : _Color]
   [format : _int]
   -> _void))

;; Get pixel data size in bytes for certain format
(define-raylib GetPixelDataSize
  (_fun
   [width : _int]
   [height : _int]
   [format : _int]
   -> _int))

;; Get the default Font
(define-raylib GetFontDefault
  (_fun
   -> _Font))

;; Load font from file into GPU memory (VRAM)
(define-raylib LoadFont
  (_fun
   [fileName : _string]
   -> _Font))

;; Load font from file with extended parameters, use NULL for codepoints and 0 for codepointCount to load the default character set, font size is provided in pixels height
(define-raylib LoadFontEx
  (_fun
   [fileName : _string]
   [fontSize : _int]
   [codepoints : (_pointer-to _int)]
   [codepointCount : _int]
   -> _Font))

;; Load font from Image (XNA style)
(define-raylib LoadFontFromImage
  (_fun
   [image : _Image]
   [key : _Color]
   [firstChar : _int]
   -> _Font))

;; Load font from memory buffer, fileType refers to extension: i.e. '.ttf'
(define-raylib LoadFontFromMemory
  (_fun
   [fileType : _string]
   [fileData : (_pointer-to _ubyte)]
   [dataSize : _int]
   [fontSize : _int]
   [codepoints : (_pointer-to _int)]
   [codepointCount : _int]
   -> _Font))

;; Check if a font is valid (font data loaded, WARNING: GPU texture not checked)
(define-raylib IsFontValid
  (_fun
   [font : _Font]
   -> _stdbool))

;; Load font data for further use
(define-raylib LoadFontData
  (_fun
   [fileData : (_pointer-to _ubyte)]
   [dataSize : _int]
   [fontSize : _int]
   [codepoints : (_pointer-to _int)]
   [codepointCount : _int]
   [type : _int]
   -> (_pointer-to _GlyphInfo)))

;; Generate image font atlas using chars info
(define-raylib GenImageFontAtlas
  (_fun
   [glyphs : (_pointer-to _GlyphInfo)]
   [glyphRecs : (_pointer-to (_pointer-to _Rectangle))]
   [glyphCount : _int]
   [fontSize : _int]
   [padding : _int]
   [packMethod : _int]
   -> _Image))

;; Unload font chars info data (RAM)
(define-raylib UnloadFontData
  (_fun
   [glyphs : (_pointer-to _GlyphInfo)]
   [glyphCount : _int]
   -> _void))

;; Unload font from GPU memory (VRAM)
(define-raylib UnloadFont
  (_fun
   [font : _Font]
   -> _void))

;; Export font as code file, returns true on success
(define-raylib ExportFontAsCode
  (_fun
   [font : _Font]
   [fileName : _string]
   -> _stdbool))

;; Draw current FPS
(define-raylib DrawFPS
  (_fun
   [posX : _int]
   [posY : _int]
   -> _void))

;; Draw text (using default font)
(define-raylib DrawText
  (_fun
   [text : _string]
   [posX : _int]
   [posY : _int]
   [fontSize : _int]
   [color : _Color]
   -> _void))

;; Draw text using font and additional parameters
(define-raylib DrawTextEx
  (_fun
   [font : _Font]
   [text : _string]
   [position : _Vector2]
   [fontSize : _float]
   [spacing : _float]
   [tint : _Color]
   -> _void))

;; Draw text using Font and pro parameters (rotation)
(define-raylib DrawTextPro
  (_fun
   [font : _Font]
   [text : _string]
   [position : _Vector2]
   [origin : _Vector2]
   [rotation : _float]
   [fontSize : _float]
   [spacing : _float]
   [tint : _Color]
   -> _void))

;; Draw one character (codepoint)
(define-raylib DrawTextCodepoint
  (_fun
   [font : _Font]
   [codepoint : _int]
   [position : _Vector2]
   [fontSize : _float]
   [tint : _Color]
   -> _void))

;; Draw multiple character (codepoint)
(define-raylib DrawTextCodepoints
  (_fun
   [font : _Font]
   [codepoints : (_pointer-to _int)]
   [codepointCount : _int]
   [position : _Vector2]
   [fontSize : _float]
   [spacing : _float]
   [tint : _Color]
   -> _void))

;; Set vertical line spacing when drawing with line-breaks
(define-raylib SetTextLineSpacing
  (_fun
   [spacing : _int]
   -> _void))

;; Measure string width for default font
(define-raylib MeasureText
  (_fun
   [text : _string]
   [fontSize : _int]
   -> _int))

;; Measure string size for Font
(define-raylib MeasureTextEx
  (_fun
   [font : _Font]
   [text : _string]
   [fontSize : _float]
   [spacing : _float]
   -> _Vector2))

;; Get glyph index position in font for a codepoint (unicode character), fallback to '?' if not found
(define-raylib GetGlyphIndex
  (_fun
   [font : _Font]
   [codepoint : _int]
   -> _int))

;; Get glyph font info data for a codepoint (unicode character), fallback to '?' if not found
(define-raylib GetGlyphInfo
  (_fun
   [font : _Font]
   [codepoint : _int]
   -> _GlyphInfo))

;; Get glyph rectangle in font atlas for a codepoint (unicode character), fallback to '?' if not found
(define-raylib GetGlyphAtlasRec
  (_fun
   [font : _Font]
   [codepoint : _int]
   -> _Rectangle))

;; Load UTF-8 text encoded from codepoints array
(define-raylib LoadUTF8
  (_fun
   [codepoints : (_pointer-to _int)]
   [length : _int]
   -> (_pointer-to _byte)))

;; Unload UTF-8 text encoded from codepoints array
(define-raylib UnloadUTF8
  (_fun
   [text : (_pointer-to _byte)]
   -> _void))

;; Load all codepoints from a UTF-8 text string, codepoints count returned by parameter
(define-raylib LoadCodepoints
  (_fun
   [text : _string]
   [count : (_pointer-to _int)]
   -> (_pointer-to _int)))

;; Unload codepoints data from memory
(define-raylib UnloadCodepoints
  (_fun
   [codepoints : (_pointer-to _int)]
   -> _void))

;; Get total number of codepoints in a UTF-8 encoded string
(define-raylib GetCodepointCount
  (_fun
   [text : _string]
   -> _int))

;; Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
(define-raylib GetCodepoint
  (_fun
   [text : _string]
   [codepointSize : (_pointer-to _int)]
   -> _int))

;; Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
(define-raylib GetCodepointNext
  (_fun
   [text : _string]
   [codepointSize : (_pointer-to _int)]
   -> _int))

;; Get previous codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
(define-raylib GetCodepointPrevious
  (_fun
   [text : _string]
   [codepointSize : (_pointer-to _int)]
   -> _int))

;; Encode one codepoint into UTF-8 byte array (array length returned as parameter)
(define-raylib CodepointToUTF8
  (_fun
   [codepoint : _int]
   [utf8Size : (_pointer-to _int)]
   -> _string))

;; Copy one string to another, returns bytes copied
(define-raylib TextCopy
  (_fun
   [dst : (_pointer-to _byte)]
   [src : _string]
   -> _int))

;; Check if two text string are equal
(define-raylib TextIsEqual
  (_fun
   [text1 : _string]
   [text2 : _string]
   -> _stdbool))

;; Get text length, checks for '\0' ending
(define-raylib TextLength
  (_fun
   [text : _string]
   -> _uint))

;; Text formatting with variables (sprintf() style)
(define-raylib TextFormat
  (_fun
   #:varargs-after 1
   [text : _string]
   ;; ... varargs
   -> _string))

;; Get a piece of a text string
(define-raylib TextSubtext
  (_fun
   [text : _string]
   [position : _int]
   [length : _int]
   -> _string))

;; Replace text string (WARNING: memory must be freed!)
(define-raylib TextReplace
  (_fun
   [text : _string]
   [replace : _string]
   [by : _string]
   -> (_pointer-to _byte)))

;; Insert text in a position (WARNING: memory must be freed!)
(define-raylib TextInsert
  (_fun
   [text : _string]
   [insert : _string]
   [position : _int]
   -> (_pointer-to _byte)))

;; Join text strings with delimiter
(define-raylib TextJoin
  (_fun
   [textList : (_pointer-to (_pointer-to _byte))]
   [count : _int]
   [delimiter : _string]
   -> _string))

;; Split text into multiple strings
(define-raylib TextSplit
  (_fun
   [text : _string]
   [delimiter : _byte]
   [count : (_pointer-to _int)]
   -> (_pointer-to (_pointer-to _byte))))

;; Append text at specific position and move cursor!
(define-raylib TextAppend
  (_fun
   [text : (_pointer-to _byte)]
   [append : _string]
   [position : (_pointer-to _int)]
   -> _void))

;; Find first text occurrence within a string
(define-raylib TextFindIndex
  (_fun
   [text : _string]
   [find : _string]
   -> _int))

;; Get upper case version of provided string
(define-raylib TextToUpper
  (_fun
   [text : _string]
   -> _string))

;; Get lower case version of provided string
(define-raylib TextToLower
  (_fun
   [text : _string]
   -> _string))

;; Get Pascal case notation version of provided string
(define-raylib TextToPascal
  (_fun
   [text : _string]
   -> _string))

;; Get Snake case notation version of provided string
(define-raylib TextToSnake
  (_fun
   [text : _string]
   -> _string))

;; Get Camel case notation version of provided string
(define-raylib TextToCamel
  (_fun
   [text : _string]
   -> _string))

;; Get integer value from text (negative values not supported)
(define-raylib TextToInteger
  (_fun
   [text : _string]
   -> _int))

;; Get float value from text (negative values not supported)
(define-raylib TextToFloat
  (_fun
   [text : _string]
   -> _float))

;; Draw a line in 3D world space
(define-raylib DrawLine3D
  (_fun
   [startPos : _Vector3]
   [endPos : _Vector3]
   [color : _Color]
   -> _void))

;; Draw a point in 3D space, actually a small line
(define-raylib DrawPoint3D
  (_fun
   [position : _Vector3]
   [color : _Color]
   -> _void))

;; Draw a circle in 3D world space
(define-raylib DrawCircle3D
  (_fun
   [center : _Vector3]
   [radius : _float]
   [rotationAxis : _Vector3]
   [rotationAngle : _float]
   [color : _Color]
   -> _void))

;; Draw a color-filled triangle (vertex in counter-clockwise order!)
(define-raylib DrawTriangle3D
  (_fun
   [v1 : _Vector3]
   [v2 : _Vector3]
   [v3 : _Vector3]
   [color : _Color]
   -> _void))

;; Draw a triangle strip defined by points
(define-raylib DrawTriangleStrip3D
  (_fun
   [points : (_pointer-to _Vector3)]
   [pointCount : _int]
   [color : _Color]
   -> _void))

;; Draw cube
(define-raylib DrawCube
  (_fun
   [position : _Vector3]
   [width : _float]
   [height : _float]
   [length : _float]
   [color : _Color]
   -> _void))

;; Draw cube (Vector version)
(define-raylib DrawCubeV
  (_fun
   [position : _Vector3]
   [size : _Vector3]
   [color : _Color]
   -> _void))

;; Draw cube wires
(define-raylib DrawCubeWires
  (_fun
   [position : _Vector3]
   [width : _float]
   [height : _float]
   [length : _float]
   [color : _Color]
   -> _void))

;; Draw cube wires (Vector version)
(define-raylib DrawCubeWiresV
  (_fun
   [position : _Vector3]
   [size : _Vector3]
   [color : _Color]
   -> _void))

;; Draw sphere
(define-raylib DrawSphere
  (_fun
   [centerPos : _Vector3]
   [radius : _float]
   [color : _Color]
   -> _void))

;; Draw sphere with extended parameters
(define-raylib DrawSphereEx
  (_fun
   [centerPos : _Vector3]
   [radius : _float]
   [rings : _int]
   [slices : _int]
   [color : _Color]
   -> _void))

;; Draw sphere wires
(define-raylib DrawSphereWires
  (_fun
   [centerPos : _Vector3]
   [radius : _float]
   [rings : _int]
   [slices : _int]
   [color : _Color]
   -> _void))

;; Draw a cylinder/cone
(define-raylib DrawCylinder
  (_fun
   [position : _Vector3]
   [radiusTop : _float]
   [radiusBottom : _float]
   [height : _float]
   [slices : _int]
   [color : _Color]
   -> _void))

;; Draw a cylinder with base at startPos and top at endPos
(define-raylib DrawCylinderEx
  (_fun
   [startPos : _Vector3]
   [endPos : _Vector3]
   [startRadius : _float]
   [endRadius : _float]
   [sides : _int]
   [color : _Color]
   -> _void))

;; Draw a cylinder/cone wires
(define-raylib DrawCylinderWires
  (_fun
   [position : _Vector3]
   [radiusTop : _float]
   [radiusBottom : _float]
   [height : _float]
   [slices : _int]
   [color : _Color]
   -> _void))

;; Draw a cylinder wires with base at startPos and top at endPos
(define-raylib DrawCylinderWiresEx
  (_fun
   [startPos : _Vector3]
   [endPos : _Vector3]
   [startRadius : _float]
   [endRadius : _float]
   [sides : _int]
   [color : _Color]
   -> _void))

;; Draw a capsule with the center of its sphere caps at startPos and endPos
(define-raylib DrawCapsule
  (_fun
   [startPos : _Vector3]
   [endPos : _Vector3]
   [radius : _float]
   [slices : _int]
   [rings : _int]
   [color : _Color]
   -> _void))

;; Draw capsule wireframe with the center of its sphere caps at startPos and endPos
(define-raylib DrawCapsuleWires
  (_fun
   [startPos : _Vector3]
   [endPos : _Vector3]
   [radius : _float]
   [slices : _int]
   [rings : _int]
   [color : _Color]
   -> _void))

;; Draw a plane XZ
(define-raylib DrawPlane
  (_fun
   [centerPos : _Vector3]
   [size : _Vector2]
   [color : _Color]
   -> _void))

;; Draw a ray line
(define-raylib DrawRay
  (_fun
   [ray : _Ray]
   [color : _Color]
   -> _void))

;; Draw a grid (centered at (0, 0, 0))
(define-raylib DrawGrid
  (_fun
   [slices : _int]
   [spacing : _float]
   -> _void))

;; Load model from files (meshes and materials)
(define-raylib LoadModel
  (_fun
   [fileName : _string]
   -> _Model))

;; Load model from generated mesh (default material)
(define-raylib LoadModelFromMesh
  (_fun
   [mesh : _Mesh]
   -> _Model))

;; Check if a model is valid (loaded in GPU, VAO/VBOs)
(define-raylib IsModelValid
  (_fun
   [model : _Model]
   -> _stdbool))

;; Unload model (including meshes) from memory (RAM and/or VRAM)
(define-raylib UnloadModel
  (_fun
   [model : _Model]
   -> _void))

;; Compute model bounding box limits (considers all meshes)
(define-raylib GetModelBoundingBox
  (_fun
   [model : _Model]
   -> _BoundingBox))

;; Draw a model (with texture if set)
(define-raylib DrawModel
  (_fun
   [model : _Model]
   [position : _Vector3]
   [scale : _float]
   [tint : _Color]
   -> _void))

;; Draw a model with extended parameters
(define-raylib DrawModelEx
  (_fun
   [model : _Model]
   [position : _Vector3]
   [rotationAxis : _Vector3]
   [rotationAngle : _float]
   [scale : _Vector3]
   [tint : _Color]
   -> _void))

;; Draw a model wires (with texture if set)
(define-raylib DrawModelWires
  (_fun
   [model : _Model]
   [position : _Vector3]
   [scale : _float]
   [tint : _Color]
   -> _void))

;; Draw a model wires (with texture if set) with extended parameters
(define-raylib DrawModelWiresEx
  (_fun
   [model : _Model]
   [position : _Vector3]
   [rotationAxis : _Vector3]
   [rotationAngle : _float]
   [scale : _Vector3]
   [tint : _Color]
   -> _void))

;; Draw a model as points
(define-raylib DrawModelPoints
  (_fun
   [model : _Model]
   [position : _Vector3]
   [scale : _float]
   [tint : _Color]
   -> _void))

;; Draw a model as points with extended parameters
(define-raylib DrawModelPointsEx
  (_fun
   [model : _Model]
   [position : _Vector3]
   [rotationAxis : _Vector3]
   [rotationAngle : _float]
   [scale : _Vector3]
   [tint : _Color]
   -> _void))

;; Draw bounding box (wires)
(define-raylib DrawBoundingBox
  (_fun
   [box : _BoundingBox]
   [color : _Color]
   -> _void))

;; Draw a billboard texture
(define-raylib DrawBillboard
  (_fun
   [camera : _Camera]
   [texture : _Texture2D]
   [position : _Vector3]
   [scale : _float]
   [tint : _Color]
   -> _void))

;; Draw a billboard texture defined by source
(define-raylib DrawBillboardRec
  (_fun
   [camera : _Camera]
   [texture : _Texture2D]
   [source : _Rectangle]
   [position : _Vector3]
   [size : _Vector2]
   [tint : _Color]
   -> _void))

;; Draw a billboard texture defined by source and rotation
(define-raylib DrawBillboardPro
  (_fun
   [camera : _Camera]
   [texture : _Texture2D]
   [source : _Rectangle]
   [position : _Vector3]
   [up : _Vector3]
   [size : _Vector2]
   [origin : _Vector2]
   [rotation : _float]
   [tint : _Color]
   -> _void))

;; Upload mesh vertex data in GPU and provide VAO/VBO ids
(define-raylib UploadMesh
  (_fun
   [mesh : (_pointer-to _Mesh)]
   [dynamic : _stdbool]
   -> _void))

;; Update mesh vertex data in GPU for a specific buffer index
(define-raylib UpdateMeshBuffer
  (_fun
   [mesh : _Mesh]
   [index : _int]
   [data : (_pointer-to _void)]
   [dataSize : _int]
   [offset : _int]
   -> _void))

;; Unload mesh data from CPU and GPU
(define-raylib UnloadMesh
  (_fun
   [mesh : _Mesh]
   -> _void))

;; Draw a 3d mesh with material and transform
(define-raylib DrawMesh
  (_fun
   [mesh : _Mesh]
   [material : _Material]
   [transform : _Matrix]
   -> _void))

;; Draw multiple mesh instances with material and different transforms
(define-raylib DrawMeshInstanced
  (_fun
   [mesh : _Mesh]
   [material : _Material]
   [transforms : (_pointer-to _Matrix)]
   [instances : _int]
   -> _void))

;; Compute mesh bounding box limits
(define-raylib GetMeshBoundingBox
  (_fun
   [mesh : _Mesh]
   -> _BoundingBox))

;; Compute mesh tangents
(define-raylib GenMeshTangents
  (_fun
   [mesh : (_pointer-to _Mesh)]
   -> _void))

;; Export mesh data to file, returns true on success
(define-raylib ExportMesh
  (_fun
   [mesh : _Mesh]
   [fileName : _string]
   -> _stdbool))

;; Export mesh as code file (.h) defining multiple arrays of vertex attributes
(define-raylib ExportMeshAsCode
  (_fun
   [mesh : _Mesh]
   [fileName : _string]
   -> _stdbool))

;; Generate polygonal mesh
(define-raylib GenMeshPoly
  (_fun
   [sides : _int]
   [radius : _float]
   -> _Mesh))

;; Generate plane mesh (with subdivisions)
(define-raylib GenMeshPlane
  (_fun
   [width : _float]
   [length : _float]
   [resX : _int]
   [resZ : _int]
   -> _Mesh))

;; Generate cuboid mesh
(define-raylib GenMeshCube
  (_fun
   [width : _float]
   [height : _float]
   [length : _float]
   -> _Mesh))

;; Generate sphere mesh (standard sphere)
(define-raylib GenMeshSphere
  (_fun
   [radius : _float]
   [rings : _int]
   [slices : _int]
   -> _Mesh))

;; Generate half-sphere mesh (no bottom cap)
(define-raylib GenMeshHemiSphere
  (_fun
   [radius : _float]
   [rings : _int]
   [slices : _int]
   -> _Mesh))

;; Generate cylinder mesh
(define-raylib GenMeshCylinder
  (_fun
   [radius : _float]
   [height : _float]
   [slices : _int]
   -> _Mesh))

;; Generate cone/pyramid mesh
(define-raylib GenMeshCone
  (_fun
   [radius : _float]
   [height : _float]
   [slices : _int]
   -> _Mesh))

;; Generate torus mesh
(define-raylib GenMeshTorus
  (_fun
   [radius : _float]
   [size : _float]
   [radSeg : _int]
   [sides : _int]
   -> _Mesh))

;; Generate trefoil knot mesh
(define-raylib GenMeshKnot
  (_fun
   [radius : _float]
   [size : _float]
   [radSeg : _int]
   [sides : _int]
   -> _Mesh))

;; Generate heightmap mesh from image data
(define-raylib GenMeshHeightmap
  (_fun
   [heightmap : _Image]
   [size : _Vector3]
   -> _Mesh))

;; Generate cubes-based map mesh from image data
(define-raylib GenMeshCubicmap
  (_fun
   [cubicmap : _Image]
   [cubeSize : _Vector3]
   -> _Mesh))

;; Load materials from model file
(define-raylib LoadMaterials
  (_fun
   [fileName : _string]
   [materialCount : (_pointer-to _int)]
   -> (_pointer-to _Material)))

;; Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)
(define-raylib LoadMaterialDefault
  (_fun
   -> _Material))

;; Check if a material is valid (shader assigned, map textures loaded in GPU)
(define-raylib IsMaterialValid
  (_fun
   [material : _Material]
   -> _stdbool))

;; Unload material from GPU memory (VRAM)
(define-raylib UnloadMaterial
  (_fun
   [material : _Material]
   -> _void))

;; Set texture for a material map type (MATERIAL_MAP_DIFFUSE, MATERIAL_MAP_SPECULAR...)
(define-raylib SetMaterialTexture
  (_fun
   [material : (_pointer-to _Material)]
   [mapType : _int]
   [texture : _Texture2D]
   -> _void))

;; Set material for a mesh
(define-raylib SetModelMeshMaterial
  (_fun
   [model : (_pointer-to _Model)]
   [meshId : _int]
   [materialId : _int]
   -> _void))

;; Load model animations from file
(define-raylib LoadModelAnimations
  (_fun
   [fileName : _string]
   [animCount : (_pointer-to _int)]
   -> (_pointer-to _ModelAnimation)))

;; Update model animation pose (CPU)
(define-raylib UpdateModelAnimation
  (_fun
   [model : _Model]
   [anim : _ModelAnimation]
   [frame : _int]
   -> _void))

;; Update model animation mesh bone matrices (GPU skinning)
(define-raylib UpdateModelAnimationBones
  (_fun
   [model : _Model]
   [anim : _ModelAnimation]
   [frame : _int]
   -> _void))

;; Unload animation data
(define-raylib UnloadModelAnimation
  (_fun
   [anim : _ModelAnimation]
   -> _void))

;; Unload animation array data
(define-raylib UnloadModelAnimations
  (_fun
   [animations : (_pointer-to _ModelAnimation)]
   [animCount : _int]
   -> _void))

;; Check model animation skeleton match
(define-raylib IsModelAnimationValid
  (_fun
   [model : _Model]
   [anim : _ModelAnimation]
   -> _stdbool))

;; Check collision between two spheres
(define-raylib CheckCollisionSpheres
  (_fun
   [center1 : _Vector3]
   [radius1 : _float]
   [center2 : _Vector3]
   [radius2 : _float]
   -> _stdbool))

;; Check collision between two bounding boxes
(define-raylib CheckCollisionBoxes
  (_fun
   [box1 : _BoundingBox]
   [box2 : _BoundingBox]
   -> _stdbool))

;; Check collision between box and sphere
(define-raylib CheckCollisionBoxSphere
  (_fun
   [box : _BoundingBox]
   [center : _Vector3]
   [radius : _float]
   -> _stdbool))

;; Get collision info between ray and sphere
(define-raylib GetRayCollisionSphere
  (_fun
   [ray : _Ray]
   [center : _Vector3]
   [radius : _float]
   -> _RayCollision))

;; Get collision info between ray and box
(define-raylib GetRayCollisionBox
  (_fun
   [ray : _Ray]
   [box : _BoundingBox]
   -> _RayCollision))

;; Get collision info between ray and mesh
(define-raylib GetRayCollisionMesh
  (_fun
   [ray : _Ray]
   [mesh : _Mesh]
   [transform : _Matrix]
   -> _RayCollision))

;; Get collision info between ray and triangle
(define-raylib GetRayCollisionTriangle
  (_fun
   [ray : _Ray]
   [p1 : _Vector3]
   [p2 : _Vector3]
   [p3 : _Vector3]
   -> _RayCollision))

;; Get collision info between ray and quad
(define-raylib GetRayCollisionQuad
  (_fun
   [ray : _Ray]
   [p1 : _Vector3]
   [p2 : _Vector3]
   [p3 : _Vector3]
   [p4 : _Vector3]
   -> _RayCollision))

;; Initialize audio device and context
(define-raylib InitAudioDevice
  (_fun
   -> _void))

;; Close the audio device and context
(define-raylib CloseAudioDevice
  (_fun
   -> _void))

;; Check if audio device has been initialized successfully
(define-raylib IsAudioDeviceReady
  (_fun
   -> _stdbool))

;; Set master volume (listener)
(define-raylib SetMasterVolume
  (_fun
   [volume : _float]
   -> _void))

;; Get master volume (listener)
(define-raylib GetMasterVolume
  (_fun
   -> _float))

;; Load wave data from file
(define-raylib LoadWave
  (_fun
   [fileName : _string]
   -> _Wave))

;; Load wave from memory buffer, fileType refers to extension: i.e. '.wav'
(define-raylib LoadWaveFromMemory
  (_fun
   [fileType : _string]
   [fileData : (_pointer-to _ubyte)]
   [dataSize : _int]
   -> _Wave))

;; Checks if wave data is valid (data loaded and parameters)
(define-raylib IsWaveValid
  (_fun
   [wave : _Wave]
   -> _stdbool))

;; Load sound from file
(define-raylib LoadSound
  (_fun
   [fileName : _string]
   -> _Sound))

;; Load sound from wave data
(define-raylib LoadSoundFromWave
  (_fun
   [wave : _Wave]
   -> _Sound))

;; Create a new sound that shares the same sample data as the source sound, does not own the sound data
(define-raylib LoadSoundAlias
  (_fun
   [source : _Sound]
   -> _Sound))

;; Checks if a sound is valid (data loaded and buffers initialized)
(define-raylib IsSoundValid
  (_fun
   [sound : _Sound]
   -> _stdbool))

;; Update sound buffer with new data
(define-raylib UpdateSound
  (_fun
   [sound : _Sound]
   [data : (_pointer-to _void)]
   [sampleCount : _int]
   -> _void))

;; Unload wave data
(define-raylib UnloadWave
  (_fun
   [wave : _Wave]
   -> _void))

;; Unload sound
(define-raylib UnloadSound
  (_fun
   [sound : _Sound]
   -> _void))

;; Unload a sound alias (does not deallocate sample data)
(define-raylib UnloadSoundAlias
  (_fun
   [alias : _Sound]
   -> _void))

;; Export wave data to file, returns true on success
(define-raylib ExportWave
  (_fun
   [wave : _Wave]
   [fileName : _string]
   -> _stdbool))

;; Export wave sample data to code (.h), returns true on success
(define-raylib ExportWaveAsCode
  (_fun
   [wave : _Wave]
   [fileName : _string]
   -> _stdbool))

;; Play a sound
(define-raylib PlaySound
  (_fun
   [sound : _Sound]
   -> _void))

;; Stop playing a sound
(define-raylib StopSound
  (_fun
   [sound : _Sound]
   -> _void))

;; Pause a sound
(define-raylib PauseSound
  (_fun
   [sound : _Sound]
   -> _void))

;; Resume a paused sound
(define-raylib ResumeSound
  (_fun
   [sound : _Sound]
   -> _void))

;; Check if a sound is currently playing
(define-raylib IsSoundPlaying
  (_fun
   [sound : _Sound]
   -> _stdbool))

;; Set volume for a sound (1.0 is max level)
(define-raylib SetSoundVolume
  (_fun
   [sound : _Sound]
   [volume : _float]
   -> _void))

;; Set pitch for a sound (1.0 is base level)
(define-raylib SetSoundPitch
  (_fun
   [sound : _Sound]
   [pitch : _float]
   -> _void))

;; Set pan for a sound (0.5 is center)
(define-raylib SetSoundPan
  (_fun
   [sound : _Sound]
   [pan : _float]
   -> _void))

;; Copy a wave to a new wave
(define-raylib WaveCopy
  (_fun
   [wave : _Wave]
   -> _Wave))

;; Crop a wave to defined frames range
(define-raylib WaveCrop
  (_fun
   [wave : (_pointer-to _Wave)]
   [initFrame : _int]
   [finalFrame : _int]
   -> _void))

;; Convert wave data to desired format
(define-raylib WaveFormat
  (_fun
   [wave : (_pointer-to _Wave)]
   [sampleRate : _int]
   [sampleSize : _int]
   [channels : _int]
   -> _void))

;; Load samples data from wave as a 32bit float data array
(define-raylib LoadWaveSamples
  (_fun
   [wave : _Wave]
   -> (_pointer-to _float)))

;; Unload samples data loaded with LoadWaveSamples()
(define-raylib UnloadWaveSamples
  (_fun
   [samples : (_pointer-to _float)]
   -> _void))

;; Load music stream from file
(define-raylib LoadMusicStream
  (_fun
   [fileName : _string]
   -> _Music))

;; Load music stream from data
(define-raylib LoadMusicStreamFromMemory
  (_fun
   [fileType : _string]
   [data : (_pointer-to _ubyte)]
   [dataSize : _int]
   -> _Music))

;; Checks if a music stream is valid (context and buffers initialized)
(define-raylib IsMusicValid
  (_fun
   [music : _Music]
   -> _stdbool))

;; Unload music stream
(define-raylib UnloadMusicStream
  (_fun
   [music : _Music]
   -> _void))

;; Start music playing
(define-raylib PlayMusicStream
  (_fun
   [music : _Music]
   -> _void))

;; Check if music is playing
(define-raylib IsMusicStreamPlaying
  (_fun
   [music : _Music]
   -> _stdbool))

;; Updates buffers for music streaming
(define-raylib UpdateMusicStream
  (_fun
   [music : _Music]
   -> _void))

;; Stop music playing
(define-raylib StopMusicStream
  (_fun
   [music : _Music]
   -> _void))

;; Pause music playing
(define-raylib PauseMusicStream
  (_fun
   [music : _Music]
   -> _void))

;; Resume playing paused music
(define-raylib ResumeMusicStream
  (_fun
   [music : _Music]
   -> _void))

;; Seek music to a position (in seconds)
(define-raylib SeekMusicStream
  (_fun
   [music : _Music]
   [position : _float]
   -> _void))

;; Set volume for music (1.0 is max level)
(define-raylib SetMusicVolume
  (_fun
   [music : _Music]
   [volume : _float]
   -> _void))

;; Set pitch for a music (1.0 is base level)
(define-raylib SetMusicPitch
  (_fun
   [music : _Music]
   [pitch : _float]
   -> _void))

;; Set pan for a music (0.5 is center)
(define-raylib SetMusicPan
  (_fun
   [music : _Music]
   [pan : _float]
   -> _void))

;; Get music time length (in seconds)
(define-raylib GetMusicTimeLength
  (_fun
   [music : _Music]
   -> _float))

;; Get current music time played (in seconds)
(define-raylib GetMusicTimePlayed
  (_fun
   [music : _Music]
   -> _float))

;; Load audio stream (to stream raw audio pcm data)
(define-raylib LoadAudioStream
  (_fun
   [sampleRate : _uint]
   [sampleSize : _uint]
   [channels : _uint]
   -> _AudioStream))

;; Checks if an audio stream is valid (buffers initialized)
(define-raylib IsAudioStreamValid
  (_fun
   [stream : _AudioStream]
   -> _stdbool))

;; Unload audio stream and free memory
(define-raylib UnloadAudioStream
  (_fun
   [stream : _AudioStream]
   -> _void))

;; Update audio stream buffers with data
(define-raylib UpdateAudioStream
  (_fun
   [stream : _AudioStream]
   [data : (_pointer-to _void)]
   [frameCount : _int]
   -> _void))

;; Check if any audio stream buffers requires refill
(define-raylib IsAudioStreamProcessed
  (_fun
   [stream : _AudioStream]
   -> _stdbool))

;; Play audio stream
(define-raylib PlayAudioStream
  (_fun
   [stream : _AudioStream]
   -> _void))

;; Pause audio stream
(define-raylib PauseAudioStream
  (_fun
   [stream : _AudioStream]
   -> _void))

;; Resume audio stream
(define-raylib ResumeAudioStream
  (_fun
   [stream : _AudioStream]
   -> _void))

;; Check if audio stream is playing
(define-raylib IsAudioStreamPlaying
  (_fun
   [stream : _AudioStream]
   -> _stdbool))

;; Stop audio stream
(define-raylib StopAudioStream
  (_fun
   [stream : _AudioStream]
   -> _void))

;; Set volume for audio stream (1.0 is max level)
(define-raylib SetAudioStreamVolume
  (_fun
   [stream : _AudioStream]
   [volume : _float]
   -> _void))

;; Set pitch for audio stream (1.0 is base level)
(define-raylib SetAudioStreamPitch
  (_fun
   [stream : _AudioStream]
   [pitch : _float]
   -> _void))

;; Set pan for audio stream (0.5 is centered)
(define-raylib SetAudioStreamPan
  (_fun
   [stream : _AudioStream]
   [pan : _float]
   -> _void))

;; Default size for new audio streams
(define-raylib SetAudioStreamBufferSizeDefault
  (_fun
   [size : _int]
   -> _void))

;; Audio thread callback to request new data
(define-raylib SetAudioStreamCallback
  (_fun
   [stream : _AudioStream]
   [callback : _AudioCallback]
   -> _void))

;; Attach audio stream processor to stream, receives the samples as 'float'
(define-raylib AttachAudioStreamProcessor
  (_fun
   [stream : _AudioStream]
   [processor : _AudioCallback]
   -> _void))

;; Detach audio stream processor from stream
(define-raylib DetachAudioStreamProcessor
  (_fun
   [stream : _AudioStream]
   [processor : _AudioCallback]
   -> _void))

;; Attach audio stream processor to the entire audio pipeline, receives the samples as 'float'
(define-raylib AttachAudioMixedProcessor
  (_fun
   [processor : _AudioCallback]
   -> _void))

;; Detach audio stream processor from the entire audio pipeline
(define-raylib DetachAudioMixedProcessor
  (_fun
   [processor : _AudioCallback]
   -> _void))
