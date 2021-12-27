#lang racket/base

(require ffi/unsafe)

(provide (all-defined-out))

;; System/Window config flags
(define _ConfigFlags
  (_enum '(FLAG_VSYNC_HINT = 64
           FLAG_FULLSCREEN_MODE = 2
           FLAG_WINDOW_RESIZABLE = 4
           FLAG_WINDOW_UNDECORATED = 8
           FLAG_WINDOW_HIDDEN = 128
           FLAG_WINDOW_MINIMIZED = 512
           FLAG_WINDOW_MAXIMIZED = 1024
           FLAG_WINDOW_UNFOCUSED = 2048
           FLAG_WINDOW_TOPMOST = 4096
           FLAG_WINDOW_ALWAYS_RUN = 256
           FLAG_WINDOW_TRANSPARENT = 16
           FLAG_WINDOW_HIGHDPI = 8192
           FLAG_MSAA_4X_HINT = 32
           FLAG_INTERLACED_HINT = 65536
           )))
(define FLAG_VSYNC_HINT 64) ; Set to try enabling V-Sync on GPU
(define FLAG_FULLSCREEN_MODE 2) ; Set to run program in fullscreen
(define FLAG_WINDOW_RESIZABLE 4) ; Set to allow resizable window
(define FLAG_WINDOW_UNDECORATED 8) ; Set to disable window decoration (frame and buttons)
(define FLAG_WINDOW_HIDDEN 128) ; Set to hide window
(define FLAG_WINDOW_MINIMIZED 512) ; Set to minimize window (iconify)
(define FLAG_WINDOW_MAXIMIZED 1024) ; Set to maximize window (expanded to monitor)
(define FLAG_WINDOW_UNFOCUSED 2048) ; Set to window non focused
(define FLAG_WINDOW_TOPMOST 4096) ; Set to window always on top
(define FLAG_WINDOW_ALWAYS_RUN 256) ; Set to allow windows running while minimized
(define FLAG_WINDOW_TRANSPARENT 16) ; Set to allow transparent framebuffer
(define FLAG_WINDOW_HIGHDPI 8192) ; Set to support HighDPI
(define FLAG_MSAA_4X_HINT 32) ; Set to try enabling MSAA 4X
(define FLAG_INTERLACED_HINT 65536) ; Set to try enabling interlaced video format (for V3D)

;; Trace log level
(define _TraceLogLevel
  (_enum '(LOG_ALL = 0
           LOG_TRACE = 1
           LOG_DEBUG = 2
           LOG_INFO = 3
           LOG_WARNING = 4
           LOG_ERROR = 5
           LOG_FATAL = 6
           LOG_NONE = 7
           )))
(define LOG_ALL 0) ; Display all logs
(define LOG_TRACE 1) ; Trace logging, intended for internal use only
(define LOG_DEBUG 2) ; Debug logging, used for internal debugging, it should be disabled on release builds
(define LOG_INFO 3) ; Info logging, used for program execution info
(define LOG_WARNING 4) ; Warning logging, used on recoverable failures
(define LOG_ERROR 5) ; Error logging, used on unrecoverable failures
(define LOG_FATAL 6) ; Fatal logging, used to abort program: exit(EXIT_FAILURE)
(define LOG_NONE 7) ; Disable logging

;; Keyboard keys (US keyboard layout)
(define _KeyboardKey
  (_enum '(KEY_NULL = 0
           KEY_APOSTROPHE = 39
           KEY_COMMA = 44
           KEY_MINUS = 45
           KEY_PERIOD = 46
           KEY_SLASH = 47
           KEY_ZERO = 48
           KEY_ONE = 49
           KEY_TWO = 50
           KEY_THREE = 51
           KEY_FOUR = 52
           KEY_FIVE = 53
           KEY_SIX = 54
           KEY_SEVEN = 55
           KEY_EIGHT = 56
           KEY_NINE = 57
           KEY_SEMICOLON = 59
           KEY_EQUAL = 61
           KEY_A = 65
           KEY_B = 66
           KEY_C = 67
           KEY_D = 68
           KEY_E = 69
           KEY_F = 70
           KEY_G = 71
           KEY_H = 72
           KEY_I = 73
           KEY_J = 74
           KEY_K = 75
           KEY_L = 76
           KEY_M = 77
           KEY_N = 78
           KEY_O = 79
           KEY_P = 80
           KEY_Q = 81
           KEY_R = 82
           KEY_S = 83
           KEY_T = 84
           KEY_U = 85
           KEY_V = 86
           KEY_W = 87
           KEY_X = 88
           KEY_Y = 89
           KEY_Z = 90
           KEY_LEFT_BRACKET = 91
           KEY_BACKSLASH = 92
           KEY_RIGHT_BRACKET = 93
           KEY_GRAVE = 96
           KEY_SPACE = 32
           KEY_ESCAPE = 256
           KEY_ENTER = 257
           KEY_TAB = 258
           KEY_BACKSPACE = 259
           KEY_INSERT = 260
           KEY_DELETE = 261
           KEY_RIGHT = 262
           KEY_LEFT = 263
           KEY_DOWN = 264
           KEY_UP = 265
           KEY_PAGE_UP = 266
           KEY_PAGE_DOWN = 267
           KEY_HOME = 268
           KEY_END = 269
           KEY_CAPS_LOCK = 280
           KEY_SCROLL_LOCK = 281
           KEY_NUM_LOCK = 282
           KEY_PRINT_SCREEN = 283
           KEY_PAUSE = 284
           KEY_F1 = 290
           KEY_F2 = 291
           KEY_F3 = 292
           KEY_F4 = 293
           KEY_F5 = 294
           KEY_F6 = 295
           KEY_F7 = 296
           KEY_F8 = 297
           KEY_F9 = 298
           KEY_F10 = 299
           KEY_F11 = 300
           KEY_F12 = 301
           KEY_LEFT_SHIFT = 340
           KEY_LEFT_CONTROL = 341
           KEY_LEFT_ALT = 342
           KEY_LEFT_SUPER = 343
           KEY_RIGHT_SHIFT = 344
           KEY_RIGHT_CONTROL = 345
           KEY_RIGHT_ALT = 346
           KEY_RIGHT_SUPER = 347
           KEY_KB_MENU = 348
           KEY_KP_0 = 320
           KEY_KP_1 = 321
           KEY_KP_2 = 322
           KEY_KP_3 = 323
           KEY_KP_4 = 324
           KEY_KP_5 = 325
           KEY_KP_6 = 326
           KEY_KP_7 = 327
           KEY_KP_8 = 328
           KEY_KP_9 = 329
           KEY_KP_DECIMAL = 330
           KEY_KP_DIVIDE = 331
           KEY_KP_MULTIPLY = 332
           KEY_KP_SUBTRACT = 333
           KEY_KP_ADD = 334
           KEY_KP_ENTER = 335
           KEY_KP_EQUAL = 336
           KEY_BACK = 4
           KEY_MENU = 82
           KEY_VOLUME_UP = 24
           KEY_VOLUME_DOWN = 25
           )))
(define KEY_NULL 0) ; Key: NULL, used for no key pressed
(define KEY_APOSTROPHE 39) ; Key: '
(define KEY_COMMA 44) ; Key: ,
(define KEY_MINUS 45) ; Key: -
(define KEY_PERIOD 46) ; Key: .
(define KEY_SLASH 47) ; Key: /
(define KEY_ZERO 48) ; Key: 0
(define KEY_ONE 49) ; Key: 1
(define KEY_TWO 50) ; Key: 2
(define KEY_THREE 51) ; Key: 3
(define KEY_FOUR 52) ; Key: 4
(define KEY_FIVE 53) ; Key: 5
(define KEY_SIX 54) ; Key: 6
(define KEY_SEVEN 55) ; Key: 7
(define KEY_EIGHT 56) ; Key: 8
(define KEY_NINE 57) ; Key: 9
(define KEY_SEMICOLON 59) ; Key: ;
(define KEY_EQUAL 61) ; Key: =
(define KEY_A 65) ; Key: A | a
(define KEY_B 66) ; Key: B | b
(define KEY_C 67) ; Key: C | c
(define KEY_D 68) ; Key: D | d
(define KEY_E 69) ; Key: E | e
(define KEY_F 70) ; Key: F | f
(define KEY_G 71) ; Key: G | g
(define KEY_H 72) ; Key: H | h
(define KEY_I 73) ; Key: I | i
(define KEY_J 74) ; Key: J | j
(define KEY_K 75) ; Key: K | k
(define KEY_L 76) ; Key: L | l
(define KEY_M 77) ; Key: M | m
(define KEY_N 78) ; Key: N | n
(define KEY_O 79) ; Key: O | o
(define KEY_P 80) ; Key: P | p
(define KEY_Q 81) ; Key: Q | q
(define KEY_R 82) ; Key: R | r
(define KEY_S 83) ; Key: S | s
(define KEY_T 84) ; Key: T | t
(define KEY_U 85) ; Key: U | u
(define KEY_V 86) ; Key: V | v
(define KEY_W 87) ; Key: W | w
(define KEY_X 88) ; Key: X | x
(define KEY_Y 89) ; Key: Y | y
(define KEY_Z 90) ; Key: Z | z
(define KEY_LEFT_BRACKET 91) ; Key: [
(define KEY_BACKSLASH 92) ; Key: '\'
(define KEY_RIGHT_BRACKET 93) ; Key: ]
(define KEY_GRAVE 96) ; Key: `
(define KEY_SPACE 32) ; Key: Space
(define KEY_ESCAPE 256) ; Key: Esc
(define KEY_ENTER 257) ; Key: Enter
(define KEY_TAB 258) ; Key: Tab
(define KEY_BACKSPACE 259) ; Key: Backspace
(define KEY_INSERT 260) ; Key: Ins
(define KEY_DELETE 261) ; Key: Del
(define KEY_RIGHT 262) ; Key: Cursor right
(define KEY_LEFT 263) ; Key: Cursor left
(define KEY_DOWN 264) ; Key: Cursor down
(define KEY_UP 265) ; Key: Cursor up
(define KEY_PAGE_UP 266) ; Key: Page up
(define KEY_PAGE_DOWN 267) ; Key: Page down
(define KEY_HOME 268) ; Key: Home
(define KEY_END 269) ; Key: End
(define KEY_CAPS_LOCK 280) ; Key: Caps lock
(define KEY_SCROLL_LOCK 281) ; Key: Scroll down
(define KEY_NUM_LOCK 282) ; Key: Num lock
(define KEY_PRINT_SCREEN 283) ; Key: Print screen
(define KEY_PAUSE 284) ; Key: Pause
(define KEY_F1 290) ; Key: F1
(define KEY_F2 291) ; Key: F2
(define KEY_F3 292) ; Key: F3
(define KEY_F4 293) ; Key: F4
(define KEY_F5 294) ; Key: F5
(define KEY_F6 295) ; Key: F6
(define KEY_F7 296) ; Key: F7
(define KEY_F8 297) ; Key: F8
(define KEY_F9 298) ; Key: F9
(define KEY_F10 299) ; Key: F10
(define KEY_F11 300) ; Key: F11
(define KEY_F12 301) ; Key: F12
(define KEY_LEFT_SHIFT 340) ; Key: Shift left
(define KEY_LEFT_CONTROL 341) ; Key: Control left
(define KEY_LEFT_ALT 342) ; Key: Alt left
(define KEY_LEFT_SUPER 343) ; Key: Super left
(define KEY_RIGHT_SHIFT 344) ; Key: Shift right
(define KEY_RIGHT_CONTROL 345) ; Key: Control right
(define KEY_RIGHT_ALT 346) ; Key: Alt right
(define KEY_RIGHT_SUPER 347) ; Key: Super right
(define KEY_KB_MENU 348) ; Key: KB menu
(define KEY_KP_0 320) ; Key: Keypad 0
(define KEY_KP_1 321) ; Key: Keypad 1
(define KEY_KP_2 322) ; Key: Keypad 2
(define KEY_KP_3 323) ; Key: Keypad 3
(define KEY_KP_4 324) ; Key: Keypad 4
(define KEY_KP_5 325) ; Key: Keypad 5
(define KEY_KP_6 326) ; Key: Keypad 6
(define KEY_KP_7 327) ; Key: Keypad 7
(define KEY_KP_8 328) ; Key: Keypad 8
(define KEY_KP_9 329) ; Key: Keypad 9
(define KEY_KP_DECIMAL 330) ; Key: Keypad .
(define KEY_KP_DIVIDE 331) ; Key: Keypad /
(define KEY_KP_MULTIPLY 332) ; Key: Keypad *
(define KEY_KP_SUBTRACT 333) ; Key: Keypad -
(define KEY_KP_ADD 334) ; Key: Keypad +
(define KEY_KP_ENTER 335) ; Key: Keypad Enter
(define KEY_KP_EQUAL 336) ; Key: Keypad =
(define KEY_BACK 4) ; Key: Android back button
(define KEY_MENU 82) ; Key: Android menu button
(define KEY_VOLUME_UP 24) ; Key: Android volume up button
(define KEY_VOLUME_DOWN 25) ; Key: Android volume down button

;; Mouse buttons
(define _MouseButton
  (_enum '(MOUSE_BUTTON_LEFT = 0
           MOUSE_BUTTON_RIGHT = 1
           MOUSE_BUTTON_MIDDLE = 2
           MOUSE_BUTTON_SIDE = 3
           MOUSE_BUTTON_EXTRA = 4
           MOUSE_BUTTON_FORWARD = 5
           MOUSE_BUTTON_BACK = 6
           )))
(define MOUSE_BUTTON_LEFT 0) ; Mouse button left
(define MOUSE_BUTTON_RIGHT 1) ; Mouse button right
(define MOUSE_BUTTON_MIDDLE 2) ; Mouse button middle (pressed wheel)
(define MOUSE_BUTTON_SIDE 3) ; Mouse button side (advanced mouse device)
(define MOUSE_BUTTON_EXTRA 4) ; Mouse button extra (advanced mouse device)
(define MOUSE_BUTTON_FORWARD 5) ; Mouse button fordward (advanced mouse device)
(define MOUSE_BUTTON_BACK 6) ; Mouse button back (advanced mouse device)

;; Mouse cursor
(define _MouseCursor
  (_enum '(MOUSE_CURSOR_DEFAULT = 0
           MOUSE_CURSOR_ARROW = 1
           MOUSE_CURSOR_IBEAM = 2
           MOUSE_CURSOR_CROSSHAIR = 3
           MOUSE_CURSOR_POINTING_HAND = 4
           MOUSE_CURSOR_RESIZE_EW = 5
           MOUSE_CURSOR_RESIZE_NS = 6
           MOUSE_CURSOR_RESIZE_NWSE = 7
           MOUSE_CURSOR_RESIZE_NESW = 8
           MOUSE_CURSOR_RESIZE_ALL = 9
           MOUSE_CURSOR_NOT_ALLOWED = 10
           )))
(define MOUSE_CURSOR_DEFAULT 0) ; Default pointer shape
(define MOUSE_CURSOR_ARROW 1) ; Arrow shape
(define MOUSE_CURSOR_IBEAM 2) ; Text writing cursor shape
(define MOUSE_CURSOR_CROSSHAIR 3) ; Cross shape
(define MOUSE_CURSOR_POINTING_HAND 4) ; Pointing hand cursor
(define MOUSE_CURSOR_RESIZE_EW 5) ; Horizontal resize/move arrow shape
(define MOUSE_CURSOR_RESIZE_NS 6) ; Vertical resize/move arrow shape
(define MOUSE_CURSOR_RESIZE_NWSE 7) ; Top-left to bottom-right diagonal resize/move arrow shape
(define MOUSE_CURSOR_RESIZE_NESW 8) ; The top-right to bottom-left diagonal resize/move arrow shape
(define MOUSE_CURSOR_RESIZE_ALL 9) ; The omni-directional resize/move cursor shape
(define MOUSE_CURSOR_NOT_ALLOWED 10) ; The operation-not-allowed shape

;; Gamepad buttons
(define _GamepadButton
  (_enum '(GAMEPAD_BUTTON_UNKNOWN = 0
           GAMEPAD_BUTTON_LEFT_FACE_UP = 1
           GAMEPAD_BUTTON_LEFT_FACE_RIGHT = 2
           GAMEPAD_BUTTON_LEFT_FACE_DOWN = 3
           GAMEPAD_BUTTON_LEFT_FACE_LEFT = 4
           GAMEPAD_BUTTON_RIGHT_FACE_UP = 5
           GAMEPAD_BUTTON_RIGHT_FACE_RIGHT = 6
           GAMEPAD_BUTTON_RIGHT_FACE_DOWN = 7
           GAMEPAD_BUTTON_RIGHT_FACE_LEFT = 8
           GAMEPAD_BUTTON_LEFT_TRIGGER_1 = 9
           GAMEPAD_BUTTON_LEFT_TRIGGER_2 = 10
           GAMEPAD_BUTTON_RIGHT_TRIGGER_1 = 11
           GAMEPAD_BUTTON_RIGHT_TRIGGER_2 = 12
           GAMEPAD_BUTTON_MIDDLE_LEFT = 13
           GAMEPAD_BUTTON_MIDDLE = 14
           GAMEPAD_BUTTON_MIDDLE_RIGHT = 15
           GAMEPAD_BUTTON_LEFT_THUMB = 16
           GAMEPAD_BUTTON_RIGHT_THUMB = 17
           )))
(define GAMEPAD_BUTTON_UNKNOWN 0) ; Unknown button, just for error checking
(define GAMEPAD_BUTTON_LEFT_FACE_UP 1) ; Gamepad left DPAD up button
(define GAMEPAD_BUTTON_LEFT_FACE_RIGHT 2) ; Gamepad left DPAD right button
(define GAMEPAD_BUTTON_LEFT_FACE_DOWN 3) ; Gamepad left DPAD down button
(define GAMEPAD_BUTTON_LEFT_FACE_LEFT 4) ; Gamepad left DPAD left button
(define GAMEPAD_BUTTON_RIGHT_FACE_UP 5) ; Gamepad right button up (i.e. PS3: Triangle, Xbox: Y)
(define GAMEPAD_BUTTON_RIGHT_FACE_RIGHT 6) ; Gamepad right button right (i.e. PS3: Square, Xbox: X)
(define GAMEPAD_BUTTON_RIGHT_FACE_DOWN 7) ; Gamepad right button down (i.e. PS3: Cross, Xbox: A)
(define GAMEPAD_BUTTON_RIGHT_FACE_LEFT 8) ; Gamepad right button left (i.e. PS3: Circle, Xbox: B)
(define GAMEPAD_BUTTON_LEFT_TRIGGER_1 9) ; Gamepad top/back trigger left (first), it could be a trailing button
(define GAMEPAD_BUTTON_LEFT_TRIGGER_2 10) ; Gamepad top/back trigger left (second), it could be a trailing button
(define GAMEPAD_BUTTON_RIGHT_TRIGGER_1 11) ; Gamepad top/back trigger right (one), it could be a trailing button
(define GAMEPAD_BUTTON_RIGHT_TRIGGER_2 12) ; Gamepad top/back trigger right (second), it could be a trailing button
(define GAMEPAD_BUTTON_MIDDLE_LEFT 13) ; Gamepad center buttons, left one (i.e. PS3: Select)
(define GAMEPAD_BUTTON_MIDDLE 14) ; Gamepad center buttons, middle one (i.e. PS3: PS, Xbox: XBOX)
(define GAMEPAD_BUTTON_MIDDLE_RIGHT 15) ; Gamepad center buttons, right one (i.e. PS3: Start)
(define GAMEPAD_BUTTON_LEFT_THUMB 16) ; Gamepad joystick pressed button left
(define GAMEPAD_BUTTON_RIGHT_THUMB 17) ; Gamepad joystick pressed button right

;; Gamepad axis
(define _GamepadAxis
  (_enum '(GAMEPAD_AXIS_LEFT_X = 0
           GAMEPAD_AXIS_LEFT_Y = 1
           GAMEPAD_AXIS_RIGHT_X = 2
           GAMEPAD_AXIS_RIGHT_Y = 3
           GAMEPAD_AXIS_LEFT_TRIGGER = 4
           GAMEPAD_AXIS_RIGHT_TRIGGER = 5
           )))
(define GAMEPAD_AXIS_LEFT_X 0) ; Gamepad left stick X axis
(define GAMEPAD_AXIS_LEFT_Y 1) ; Gamepad left stick Y axis
(define GAMEPAD_AXIS_RIGHT_X 2) ; Gamepad right stick X axis
(define GAMEPAD_AXIS_RIGHT_Y 3) ; Gamepad right stick Y axis
(define GAMEPAD_AXIS_LEFT_TRIGGER 4) ; Gamepad back trigger left, pressure level: [1..-1]
(define GAMEPAD_AXIS_RIGHT_TRIGGER 5) ; Gamepad back trigger right, pressure level: [1..-1]

;; Material map index
(define _MaterialMapIndex
  (_enum '(MATERIAL_MAP_ALBEDO = 0
           MATERIAL_MAP_METALNESS = 1
           MATERIAL_MAP_NORMAL = 2
           MATERIAL_MAP_ROUGHNESS = 3
           MATERIAL_MAP_OCCLUSION = 4
           MATERIAL_MAP_EMISSION = 5
           MATERIAL_MAP_HEIGHT = 6
           MATERIAL_MAP_CUBEMAP = 7
           MATERIAL_MAP_IRRADIANCE = 8
           MATERIAL_MAP_PREFILTER = 9
           MATERIAL_MAP_BRDF = 10
           )))
(define MATERIAL_MAP_ALBEDO 0) ; Albedo material (same as: MATERIAL_MAP_DIFFUSE)
(define MATERIAL_MAP_METALNESS 1) ; Metalness material (same as: MATERIAL_MAP_SPECULAR)
(define MATERIAL_MAP_NORMAL 2) ; Normal material
(define MATERIAL_MAP_ROUGHNESS 3) ; Roughness material
(define MATERIAL_MAP_OCCLUSION 4) ; Ambient occlusion material
(define MATERIAL_MAP_EMISSION 5) ; Emission material
(define MATERIAL_MAP_HEIGHT 6) ; Heightmap material
(define MATERIAL_MAP_CUBEMAP 7) ; Cubemap material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
(define MATERIAL_MAP_IRRADIANCE 8) ; Irradiance material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
(define MATERIAL_MAP_PREFILTER 9) ; Prefilter material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
(define MATERIAL_MAP_BRDF 10) ; Brdf material

;; Shader location index
(define _ShaderLocationIndex
  (_enum '(SHADER_LOC_VERTEX_POSITION = 0
           SHADER_LOC_VERTEX_TEXCOORD01 = 1
           SHADER_LOC_VERTEX_TEXCOORD02 = 2
           SHADER_LOC_VERTEX_NORMAL = 3
           SHADER_LOC_VERTEX_TANGENT = 4
           SHADER_LOC_VERTEX_COLOR = 5
           SHADER_LOC_MATRIX_MVP = 6
           SHADER_LOC_MATRIX_VIEW = 7
           SHADER_LOC_MATRIX_PROJECTION = 8
           SHADER_LOC_MATRIX_MODEL = 9
           SHADER_LOC_MATRIX_NORMAL = 10
           SHADER_LOC_VECTOR_VIEW = 11
           SHADER_LOC_COLOR_DIFFUSE = 12
           SHADER_LOC_COLOR_SPECULAR = 13
           SHADER_LOC_COLOR_AMBIENT = 14
           SHADER_LOC_MAP_ALBEDO = 15
           SHADER_LOC_MAP_METALNESS = 16
           SHADER_LOC_MAP_NORMAL = 17
           SHADER_LOC_MAP_ROUGHNESS = 18
           SHADER_LOC_MAP_OCCLUSION = 19
           SHADER_LOC_MAP_EMISSION = 20
           SHADER_LOC_MAP_HEIGHT = 21
           SHADER_LOC_MAP_CUBEMAP = 22
           SHADER_LOC_MAP_IRRADIANCE = 23
           SHADER_LOC_MAP_PREFILTER = 24
           SHADER_LOC_MAP_BRDF = 25
           )))
(define SHADER_LOC_VERTEX_POSITION 0) ; Shader location: vertex attribute: position
(define SHADER_LOC_VERTEX_TEXCOORD01 1) ; Shader location: vertex attribute: texcoord01
(define SHADER_LOC_VERTEX_TEXCOORD02 2) ; Shader location: vertex attribute: texcoord02
(define SHADER_LOC_VERTEX_NORMAL 3) ; Shader location: vertex attribute: normal
(define SHADER_LOC_VERTEX_TANGENT 4) ; Shader location: vertex attribute: tangent
(define SHADER_LOC_VERTEX_COLOR 5) ; Shader location: vertex attribute: color
(define SHADER_LOC_MATRIX_MVP 6) ; Shader location: matrix uniform: model-view-projection
(define SHADER_LOC_MATRIX_VIEW 7) ; Shader location: matrix uniform: view (camera transform)
(define SHADER_LOC_MATRIX_PROJECTION 8) ; Shader location: matrix uniform: projection
(define SHADER_LOC_MATRIX_MODEL 9) ; Shader location: matrix uniform: model (transform)
(define SHADER_LOC_MATRIX_NORMAL 10) ; Shader location: matrix uniform: normal
(define SHADER_LOC_VECTOR_VIEW 11) ; Shader location: vector uniform: view
(define SHADER_LOC_COLOR_DIFFUSE 12) ; Shader location: vector uniform: diffuse color
(define SHADER_LOC_COLOR_SPECULAR 13) ; Shader location: vector uniform: specular color
(define SHADER_LOC_COLOR_AMBIENT 14) ; Shader location: vector uniform: ambient color
(define SHADER_LOC_MAP_ALBEDO 15) ; Shader location: sampler2d texture: albedo (same as: SHADER_LOC_MAP_DIFFUSE)
(define SHADER_LOC_MAP_METALNESS 16) ; Shader location: sampler2d texture: metalness (same as: SHADER_LOC_MAP_SPECULAR)
(define SHADER_LOC_MAP_NORMAL 17) ; Shader location: sampler2d texture: normal
(define SHADER_LOC_MAP_ROUGHNESS 18) ; Shader location: sampler2d texture: roughness
(define SHADER_LOC_MAP_OCCLUSION 19) ; Shader location: sampler2d texture: occlusion
(define SHADER_LOC_MAP_EMISSION 20) ; Shader location: sampler2d texture: emission
(define SHADER_LOC_MAP_HEIGHT 21) ; Shader location: sampler2d texture: height
(define SHADER_LOC_MAP_CUBEMAP 22) ; Shader location: samplerCube texture: cubemap
(define SHADER_LOC_MAP_IRRADIANCE 23) ; Shader location: samplerCube texture: irradiance
(define SHADER_LOC_MAP_PREFILTER 24) ; Shader location: samplerCube texture: prefilter
(define SHADER_LOC_MAP_BRDF 25) ; Shader location: sampler2d texture: brdf

;; Shader uniform data type
(define _ShaderUniformDataType
  (_enum '(SHADER_UNIFORM_FLOAT = 0
           SHADER_UNIFORM_VEC2 = 1
           SHADER_UNIFORM_VEC3 = 2
           SHADER_UNIFORM_VEC4 = 3
           SHADER_UNIFORM_INT = 4
           SHADER_UNIFORM_IVEC2 = 5
           SHADER_UNIFORM_IVEC3 = 6
           SHADER_UNIFORM_IVEC4 = 7
           SHADER_UNIFORM_SAMPLER2D = 8
           )))
(define SHADER_UNIFORM_FLOAT 0) ; Shader uniform type: float
(define SHADER_UNIFORM_VEC2 1) ; Shader uniform type: vec2 (2 float)
(define SHADER_UNIFORM_VEC3 2) ; Shader uniform type: vec3 (3 float)
(define SHADER_UNIFORM_VEC4 3) ; Shader uniform type: vec4 (4 float)
(define SHADER_UNIFORM_INT 4) ; Shader uniform type: int
(define SHADER_UNIFORM_IVEC2 5) ; Shader uniform type: ivec2 (2 int)
(define SHADER_UNIFORM_IVEC3 6) ; Shader uniform type: ivec3 (3 int)
(define SHADER_UNIFORM_IVEC4 7) ; Shader uniform type: ivec4 (4 int)
(define SHADER_UNIFORM_SAMPLER2D 8) ; Shader uniform type: sampler2d

;; Shader attribute data types
(define _ShaderAttributeDataType
  (_enum '(SHADER_ATTRIB_FLOAT = 0
           SHADER_ATTRIB_VEC2 = 1
           SHADER_ATTRIB_VEC3 = 2
           SHADER_ATTRIB_VEC4 = 3
           )))
(define SHADER_ATTRIB_FLOAT 0) ; Shader attribute type: float
(define SHADER_ATTRIB_VEC2 1) ; Shader attribute type: vec2 (2 float)
(define SHADER_ATTRIB_VEC3 2) ; Shader attribute type: vec3 (3 float)
(define SHADER_ATTRIB_VEC4 3) ; Shader attribute type: vec4 (4 float)

;; Pixel formats
(define _PixelFormat
  (_enum '(PIXELFORMAT_UNCOMPRESSED_GRAYSCALE = 1
           PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA = 2
           PIXELFORMAT_UNCOMPRESSED_R5G6B5 = 3
           PIXELFORMAT_UNCOMPRESSED_R8G8B8 = 4
           PIXELFORMAT_UNCOMPRESSED_R5G5B5A1 = 5
           PIXELFORMAT_UNCOMPRESSED_R4G4B4A4 = 6
           PIXELFORMAT_UNCOMPRESSED_R8G8B8A8 = 7
           PIXELFORMAT_UNCOMPRESSED_R32 = 8
           PIXELFORMAT_UNCOMPRESSED_R32G32B32 = 9
           PIXELFORMAT_UNCOMPRESSED_R32G32B32A32 = 10
           PIXELFORMAT_COMPRESSED_DXT1_RGB = 11
           PIXELFORMAT_COMPRESSED_DXT1_RGBA = 12
           PIXELFORMAT_COMPRESSED_DXT3_RGBA = 13
           PIXELFORMAT_COMPRESSED_DXT5_RGBA = 14
           PIXELFORMAT_COMPRESSED_ETC1_RGB = 15
           PIXELFORMAT_COMPRESSED_ETC2_RGB = 16
           PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA = 17
           PIXELFORMAT_COMPRESSED_PVRT_RGB = 18
           PIXELFORMAT_COMPRESSED_PVRT_RGBA = 19
           PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA = 20
           PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA = 21
           )))
(define PIXELFORMAT_UNCOMPRESSED_GRAYSCALE 1) ; 8 bit per pixel (no alpha)
(define PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA 2) ; 8*2 bpp (2 channels)
(define PIXELFORMAT_UNCOMPRESSED_R5G6B5 3) ; 16 bpp
(define PIXELFORMAT_UNCOMPRESSED_R8G8B8 4) ; 24 bpp
(define PIXELFORMAT_UNCOMPRESSED_R5G5B5A1 5) ; 16 bpp (1 bit alpha)
(define PIXELFORMAT_UNCOMPRESSED_R4G4B4A4 6) ; 16 bpp (4 bit alpha)
(define PIXELFORMAT_UNCOMPRESSED_R8G8B8A8 7) ; 32 bpp
(define PIXELFORMAT_UNCOMPRESSED_R32 8) ; 32 bpp (1 channel - float)
(define PIXELFORMAT_UNCOMPRESSED_R32G32B32 9) ; 32*3 bpp (3 channels - float)
(define PIXELFORMAT_UNCOMPRESSED_R32G32B32A32 10) ; 32*4 bpp (4 channels - float)
(define PIXELFORMAT_COMPRESSED_DXT1_RGB 11) ; 4 bpp (no alpha)
(define PIXELFORMAT_COMPRESSED_DXT1_RGBA 12) ; 4 bpp (1 bit alpha)
(define PIXELFORMAT_COMPRESSED_DXT3_RGBA 13) ; 8 bpp
(define PIXELFORMAT_COMPRESSED_DXT5_RGBA 14) ; 8 bpp
(define PIXELFORMAT_COMPRESSED_ETC1_RGB 15) ; 4 bpp
(define PIXELFORMAT_COMPRESSED_ETC2_RGB 16) ; 4 bpp
(define PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA 17) ; 8 bpp
(define PIXELFORMAT_COMPRESSED_PVRT_RGB 18) ; 4 bpp
(define PIXELFORMAT_COMPRESSED_PVRT_RGBA 19) ; 4 bpp
(define PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA 20) ; 8 bpp
(define PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA 21) ; 2 bpp

;; Texture parameters: filter mode
(define _TextureFilter
  (_enum '(TEXTURE_FILTER_POINT = 0
           TEXTURE_FILTER_BILINEAR = 1
           TEXTURE_FILTER_TRILINEAR = 2
           TEXTURE_FILTER_ANISOTROPIC_4X = 3
           TEXTURE_FILTER_ANISOTROPIC_8X = 4
           TEXTURE_FILTER_ANISOTROPIC_16X = 5
           )))
(define TEXTURE_FILTER_POINT 0) ; No filter, just pixel approximation
(define TEXTURE_FILTER_BILINEAR 1) ; Linear filtering
(define TEXTURE_FILTER_TRILINEAR 2) ; Trilinear filtering (linear with mipmaps)
(define TEXTURE_FILTER_ANISOTROPIC_4X 3) ; Anisotropic filtering 4x
(define TEXTURE_FILTER_ANISOTROPIC_8X 4) ; Anisotropic filtering 8x
(define TEXTURE_FILTER_ANISOTROPIC_16X 5) ; Anisotropic filtering 16x

;; Texture parameters: wrap mode
(define _TextureWrap
  (_enum '(TEXTURE_WRAP_REPEAT = 0
           TEXTURE_WRAP_CLAMP = 1
           TEXTURE_WRAP_MIRROR_REPEAT = 2
           TEXTURE_WRAP_MIRROR_CLAMP = 3
           )))
(define TEXTURE_WRAP_REPEAT 0) ; Repeats texture in tiled mode
(define TEXTURE_WRAP_CLAMP 1) ; Clamps texture to edge pixel in tiled mode
(define TEXTURE_WRAP_MIRROR_REPEAT 2) ; Mirrors and repeats the texture in tiled mode
(define TEXTURE_WRAP_MIRROR_CLAMP 3) ; Mirrors and clamps to border the texture in tiled mode

;; Cubemap layouts
(define _CubemapLayout
  (_enum '(CUBEMAP_LAYOUT_AUTO_DETECT = 0
           CUBEMAP_LAYOUT_LINE_VERTICAL = 1
           CUBEMAP_LAYOUT_LINE_HORIZONTAL = 2
           CUBEMAP_LAYOUT_CROSS_THREE_BY_FOUR = 3
           CUBEMAP_LAYOUT_CROSS_FOUR_BY_THREE = 4
           CUBEMAP_LAYOUT_PANORAMA = 5
           )))
(define CUBEMAP_LAYOUT_AUTO_DETECT 0) ; Automatically detect layout type
(define CUBEMAP_LAYOUT_LINE_VERTICAL 1) ; Layout is defined by a vertical line with faces
(define CUBEMAP_LAYOUT_LINE_HORIZONTAL 2) ; Layout is defined by an horizontal line with faces
(define CUBEMAP_LAYOUT_CROSS_THREE_BY_FOUR 3) ; Layout is defined by a 3x4 cross with cubemap faces
(define CUBEMAP_LAYOUT_CROSS_FOUR_BY_THREE 4) ; Layout is defined by a 4x3 cross with cubemap faces
(define CUBEMAP_LAYOUT_PANORAMA 5) ; Layout is defined by a panorama image (equirectangular map)

;; Font type, defines generation method
(define _FontType
  (_enum '(FONT_DEFAULT = 0
           FONT_BITMAP = 1
           FONT_SDF = 2
           )))
(define FONT_DEFAULT 0) ; Default font generation, anti-aliased
(define FONT_BITMAP 1) ; Bitmap font generation, no anti-aliasing
(define FONT_SDF 2) ; SDF font generation, requires external shader

;; Color blending modes (pre-defined)
(define _BlendMode
  (_enum '(BLEND_ALPHA = 0
           BLEND_ADDITIVE = 1
           BLEND_MULTIPLIED = 2
           BLEND_ADD_COLORS = 3
           BLEND_SUBTRACT_COLORS = 4
           BLEND_CUSTOM = 5
           )))
(define BLEND_ALPHA 0) ; Blend textures considering alpha (default)
(define BLEND_ADDITIVE 1) ; Blend textures adding colors
(define BLEND_MULTIPLIED 2) ; Blend textures multiplying colors
(define BLEND_ADD_COLORS 3) ; Blend textures adding colors (alternative)
(define BLEND_SUBTRACT_COLORS 4) ; Blend textures subtracting colors (alternative)
(define BLEND_CUSTOM 5) ; Belnd textures using custom src/dst factors (use rlSetBlendMode())

;; Gesture
(define _Gesture
  (_enum '(GESTURE_NONE = 0
           GESTURE_TAP = 1
           GESTURE_DOUBLETAP = 2
           GESTURE_HOLD = 4
           GESTURE_DRAG = 8
           GESTURE_SWIPE_RIGHT = 16
           GESTURE_SWIPE_LEFT = 32
           GESTURE_SWIPE_UP = 64
           GESTURE_SWIPE_DOWN = 128
           GESTURE_PINCH_IN = 256
           GESTURE_PINCH_OUT = 512
           )))
(define GESTURE_NONE 0) ; No gesture
(define GESTURE_TAP 1) ; Tap gesture
(define GESTURE_DOUBLETAP 2) ; Double tap gesture
(define GESTURE_HOLD 4) ; Hold gesture
(define GESTURE_DRAG 8) ; Drag gesture
(define GESTURE_SWIPE_RIGHT 16) ; Swipe right gesture
(define GESTURE_SWIPE_LEFT 32) ; Swipe left gesture
(define GESTURE_SWIPE_UP 64) ; Swipe up gesture
(define GESTURE_SWIPE_DOWN 128) ; Swipe down gesture
(define GESTURE_PINCH_IN 256) ; Pinch in gesture
(define GESTURE_PINCH_OUT 512) ; Pinch out gesture

;; Camera system modes
(define _CameraMode
  (_enum '(CAMERA_CUSTOM = 0
           CAMERA_FREE = 1
           CAMERA_ORBITAL = 2
           CAMERA_FIRST_PERSON = 3
           CAMERA_THIRD_PERSON = 4
           )))
(define CAMERA_CUSTOM 0) ; Custom camera
(define CAMERA_FREE 1) ; Free camera
(define CAMERA_ORBITAL 2) ; Orbital camera
(define CAMERA_FIRST_PERSON 3) ; First person camera
(define CAMERA_THIRD_PERSON 4) ; Third person camera

;; Camera projection
(define _CameraProjection
  (_enum '(CAMERA_PERSPECTIVE = 0
           CAMERA_ORTHOGRAPHIC = 1
           )))
(define CAMERA_PERSPECTIVE 0) ; Perspective projection
(define CAMERA_ORTHOGRAPHIC 1) ; Orthographic projection

;; N-patch layout
(define _NPatchLayout
  (_enum '(NPATCH_NINE_PATCH = 0
           NPATCH_THREE_PATCH_VERTICAL = 1
           NPATCH_THREE_PATCH_HORIZONTAL = 2
           )))
(define NPATCH_NINE_PATCH 0) ; Npatch layout: 3x3 tiles
(define NPATCH_THREE_PATCH_VERTICAL 1) ; Npatch layout: 1x3 tiles
(define NPATCH_THREE_PATCH_HORIZONTAL 2) ; Npatch layout: 3x1 tiles
