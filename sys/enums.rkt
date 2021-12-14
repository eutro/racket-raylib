#lang racket/base

(require ffi/unsafe)

(provide (all-defined-out))

;; System/Window config flags
(define _ConfigFlags
  (_enum '(FLAG_VSYNC_HINT = 64 ; Set to try enabling V-Sync on GPU
           FLAG_FULLSCREEN_MODE = 2 ; Set to run program in fullscreen
           FLAG_WINDOW_RESIZABLE = 4 ; Set to allow resizable window
           FLAG_WINDOW_UNDECORATED = 8 ; Set to disable window decoration (frame and buttons)
           FLAG_WINDOW_HIDDEN = 128 ; Set to hide window
           FLAG_WINDOW_MINIMIZED = 512 ; Set to minimize window (iconify)
           FLAG_WINDOW_MAXIMIZED = 1024 ; Set to maximize window (expanded to monitor)
           FLAG_WINDOW_UNFOCUSED = 2048 ; Set to window non focused
           FLAG_WINDOW_TOPMOST = 4096 ; Set to window always on top
           FLAG_WINDOW_ALWAYS_RUN = 256 ; Set to allow windows running while minimized
           FLAG_WINDOW_TRANSPARENT = 16 ; Set to allow transparent framebuffer
           FLAG_WINDOW_HIGHDPI = 8192 ; Set to support HighDPI
           FLAG_MSAA_4X_HINT = 32 ; Set to try enabling MSAA 4X
           FLAG_INTERLACED_HINT = 65536 ; Set to try enabling interlaced video format (for V3D)
           )))

;; Trace log level
(define _TraceLogLevel
  (_enum '(LOG_ALL = 0 ; Display all logs
           LOG_TRACE = 1 ; Trace logging, intended for internal use only
           LOG_DEBUG = 2 ; Debug logging, used for internal debugging, it should be disabled on release builds
           LOG_INFO = 3 ; Info logging, used for program execution info
           LOG_WARNING = 4 ; Warning logging, used on recoverable failures
           LOG_ERROR = 5 ; Error logging, used on unrecoverable failures
           LOG_FATAL = 6 ; Fatal logging, used to abort program: exit(EXIT_FAILURE)
           LOG_NONE = 7 ; Disable logging
           )))

;; Keyboard keys (US keyboard layout)
(define _KeyboardKey
  (_enum '(KEY_NULL = 0 ; Key: NULL, used for no key pressed
           KEY_APOSTROPHE = 39 ; Key: '
           KEY_COMMA = 44 ; Key: ,
           KEY_MINUS = 45 ; Key: -
           KEY_PERIOD = 46 ; Key: .
           KEY_SLASH = 47 ; Key: /
           KEY_ZERO = 48 ; Key: 0
           KEY_ONE = 49 ; Key: 1
           KEY_TWO = 50 ; Key: 2
           KEY_THREE = 51 ; Key: 3
           KEY_FOUR = 52 ; Key: 4
           KEY_FIVE = 53 ; Key: 5
           KEY_SIX = 54 ; Key: 6
           KEY_SEVEN = 55 ; Key: 7
           KEY_EIGHT = 56 ; Key: 8
           KEY_NINE = 57 ; Key: 9
           KEY_SEMICOLON = 59 ; Key: ;
           KEY_EQUAL = 61 ; Key: =
           KEY_A = 65 ; Key: A | a
           KEY_B = 66 ; Key: B | b
           KEY_C = 67 ; Key: C | c
           KEY_D = 68 ; Key: D | d
           KEY_E = 69 ; Key: E | e
           KEY_F = 70 ; Key: F | f
           KEY_G = 71 ; Key: G | g
           KEY_H = 72 ; Key: H | h
           KEY_I = 73 ; Key: I | i
           KEY_J = 74 ; Key: J | j
           KEY_K = 75 ; Key: K | k
           KEY_L = 76 ; Key: L | l
           KEY_M = 77 ; Key: M | m
           KEY_N = 78 ; Key: N | n
           KEY_O = 79 ; Key: O | o
           KEY_P = 80 ; Key: P | p
           KEY_Q = 81 ; Key: Q | q
           KEY_R = 82 ; Key: R | r
           KEY_S = 83 ; Key: S | s
           KEY_T = 84 ; Key: T | t
           KEY_U = 85 ; Key: U | u
           KEY_V = 86 ; Key: V | v
           KEY_W = 87 ; Key: W | w
           KEY_X = 88 ; Key: X | x
           KEY_Y = 89 ; Key: Y | y
           KEY_Z = 90 ; Key: Z | z
           KEY_LEFT_BRACKET = 91 ; Key: [
           KEY_BACKSLASH = 92 ; Key: '\'
           KEY_RIGHT_BRACKET = 93 ; Key: ]
           KEY_GRAVE = 96 ; Key: `
           KEY_SPACE = 32 ; Key: Space
           KEY_ESCAPE = 256 ; Key: Esc
           KEY_ENTER = 257 ; Key: Enter
           KEY_TAB = 258 ; Key: Tab
           KEY_BACKSPACE = 259 ; Key: Backspace
           KEY_INSERT = 260 ; Key: Ins
           KEY_DELETE = 261 ; Key: Del
           KEY_RIGHT = 262 ; Key: Cursor right
           KEY_LEFT = 263 ; Key: Cursor left
           KEY_DOWN = 264 ; Key: Cursor down
           KEY_UP = 265 ; Key: Cursor up
           KEY_PAGE_UP = 266 ; Key: Page up
           KEY_PAGE_DOWN = 267 ; Key: Page down
           KEY_HOME = 268 ; Key: Home
           KEY_END = 269 ; Key: End
           KEY_CAPS_LOCK = 280 ; Key: Caps lock
           KEY_SCROLL_LOCK = 281 ; Key: Scroll down
           KEY_NUM_LOCK = 282 ; Key: Num lock
           KEY_PRINT_SCREEN = 283 ; Key: Print screen
           KEY_PAUSE = 284 ; Key: Pause
           KEY_F1 = 290 ; Key: F1
           KEY_F2 = 291 ; Key: F2
           KEY_F3 = 292 ; Key: F3
           KEY_F4 = 293 ; Key: F4
           KEY_F5 = 294 ; Key: F5
           KEY_F6 = 295 ; Key: F6
           KEY_F7 = 296 ; Key: F7
           KEY_F8 = 297 ; Key: F8
           KEY_F9 = 298 ; Key: F9
           KEY_F10 = 299 ; Key: F10
           KEY_F11 = 300 ; Key: F11
           KEY_F12 = 301 ; Key: F12
           KEY_LEFT_SHIFT = 340 ; Key: Shift left
           KEY_LEFT_CONTROL = 341 ; Key: Control left
           KEY_LEFT_ALT = 342 ; Key: Alt left
           KEY_LEFT_SUPER = 343 ; Key: Super left
           KEY_RIGHT_SHIFT = 344 ; Key: Shift right
           KEY_RIGHT_CONTROL = 345 ; Key: Control right
           KEY_RIGHT_ALT = 346 ; Key: Alt right
           KEY_RIGHT_SUPER = 347 ; Key: Super right
           KEY_KB_MENU = 348 ; Key: KB menu
           KEY_KP_0 = 320 ; Key: Keypad 0
           KEY_KP_1 = 321 ; Key: Keypad 1
           KEY_KP_2 = 322 ; Key: Keypad 2
           KEY_KP_3 = 323 ; Key: Keypad 3
           KEY_KP_4 = 324 ; Key: Keypad 4
           KEY_KP_5 = 325 ; Key: Keypad 5
           KEY_KP_6 = 326 ; Key: Keypad 6
           KEY_KP_7 = 327 ; Key: Keypad 7
           KEY_KP_8 = 328 ; Key: Keypad 8
           KEY_KP_9 = 329 ; Key: Keypad 9
           KEY_KP_DECIMAL = 330 ; Key: Keypad .
           KEY_KP_DIVIDE = 331 ; Key: Keypad /
           KEY_KP_MULTIPLY = 332 ; Key: Keypad *
           KEY_KP_SUBTRACT = 333 ; Key: Keypad -
           KEY_KP_ADD = 334 ; Key: Keypad +
           KEY_KP_ENTER = 335 ; Key: Keypad Enter
           KEY_KP_EQUAL = 336 ; Key: Keypad =
           KEY_BACK = 4 ; Key: Android back button
           KEY_MENU = 82 ; Key: Android menu button
           KEY_VOLUME_UP = 24 ; Key: Android volume up button
           KEY_VOLUME_DOWN = 25 ; Key: Android volume down button
           )))

;; Mouse buttons
(define _MouseButton
  (_enum '(MOUSE_BUTTON_LEFT = 0 ; Mouse button left
           MOUSE_BUTTON_RIGHT = 1 ; Mouse button right
           MOUSE_BUTTON_MIDDLE = 2 ; Mouse button middle (pressed wheel)
           MOUSE_BUTTON_SIDE = 3 ; Mouse button side (advanced mouse device)
           MOUSE_BUTTON_EXTRA = 4 ; Mouse button extra (advanced mouse device)
           MOUSE_BUTTON_FORWARD = 5 ; Mouse button fordward (advanced mouse device)
           MOUSE_BUTTON_BACK = 6 ; Mouse button back (advanced mouse device)
           )))

;; Mouse cursor
(define _MouseCursor
  (_enum '(MOUSE_CURSOR_DEFAULT = 0 ; Default pointer shape
           MOUSE_CURSOR_ARROW = 1 ; Arrow shape
           MOUSE_CURSOR_IBEAM = 2 ; Text writing cursor shape
           MOUSE_CURSOR_CROSSHAIR = 3 ; Cross shape
           MOUSE_CURSOR_POINTING_HAND = 4 ; Pointing hand cursor
           MOUSE_CURSOR_RESIZE_EW = 5 ; Horizontal resize/move arrow shape
           MOUSE_CURSOR_RESIZE_NS = 6 ; Vertical resize/move arrow shape
           MOUSE_CURSOR_RESIZE_NWSE = 7 ; Top-left to bottom-right diagonal resize/move arrow shape
           MOUSE_CURSOR_RESIZE_NESW = 8 ; The top-right to bottom-left diagonal resize/move arrow shape
           MOUSE_CURSOR_RESIZE_ALL = 9 ; The omni-directional resize/move cursor shape
           MOUSE_CURSOR_NOT_ALLOWED = 10 ; The operation-not-allowed shape
           )))

;; Gamepad buttons
(define _GamepadButton
  (_enum '(GAMEPAD_BUTTON_UNKNOWN = 0 ; Unknown button, just for error checking
           GAMEPAD_BUTTON_LEFT_FACE_UP = 1 ; Gamepad left DPAD up button
           GAMEPAD_BUTTON_LEFT_FACE_RIGHT = 2 ; Gamepad left DPAD right button
           GAMEPAD_BUTTON_LEFT_FACE_DOWN = 3 ; Gamepad left DPAD down button
           GAMEPAD_BUTTON_LEFT_FACE_LEFT = 4 ; Gamepad left DPAD left button
           GAMEPAD_BUTTON_RIGHT_FACE_UP = 5 ; Gamepad right button up (i.e. PS3: Triangle, Xbox: Y)
           GAMEPAD_BUTTON_RIGHT_FACE_RIGHT = 6 ; Gamepad right button right (i.e. PS3: Square, Xbox: X)
           GAMEPAD_BUTTON_RIGHT_FACE_DOWN = 7 ; Gamepad right button down (i.e. PS3: Cross, Xbox: A)
           GAMEPAD_BUTTON_RIGHT_FACE_LEFT = 8 ; Gamepad right button left (i.e. PS3: Circle, Xbox: B)
           GAMEPAD_BUTTON_LEFT_TRIGGER_1 = 9 ; Gamepad top/back trigger left (first), it could be a trailing button
           GAMEPAD_BUTTON_LEFT_TRIGGER_2 = 10 ; Gamepad top/back trigger left (second), it could be a trailing button
           GAMEPAD_BUTTON_RIGHT_TRIGGER_1 = 11 ; Gamepad top/back trigger right (one), it could be a trailing button
           GAMEPAD_BUTTON_RIGHT_TRIGGER_2 = 12 ; Gamepad top/back trigger right (second), it could be a trailing button
           GAMEPAD_BUTTON_MIDDLE_LEFT = 13 ; Gamepad center buttons, left one (i.e. PS3: Select)
           GAMEPAD_BUTTON_MIDDLE = 14 ; Gamepad center buttons, middle one (i.e. PS3: PS, Xbox: XBOX)
           GAMEPAD_BUTTON_MIDDLE_RIGHT = 15 ; Gamepad center buttons, right one (i.e. PS3: Start)
           GAMEPAD_BUTTON_LEFT_THUMB = 16 ; Gamepad joystick pressed button left
           GAMEPAD_BUTTON_RIGHT_THUMB = 17 ; Gamepad joystick pressed button right
           )))

;; Gamepad axis
(define _GamepadAxis
  (_enum '(GAMEPAD_AXIS_LEFT_X = 0 ; Gamepad left stick X axis
           GAMEPAD_AXIS_LEFT_Y = 1 ; Gamepad left stick Y axis
           GAMEPAD_AXIS_RIGHT_X = 2 ; Gamepad right stick X axis
           GAMEPAD_AXIS_RIGHT_Y = 3 ; Gamepad right stick Y axis
           GAMEPAD_AXIS_LEFT_TRIGGER = 4 ; Gamepad back trigger left, pressure level: [1..-1]
           GAMEPAD_AXIS_RIGHT_TRIGGER = 5 ; Gamepad back trigger right, pressure level: [1..-1]
           )))

;; Material map index
(define _MaterialMapIndex
  (_enum '(MATERIAL_MAP_ALBEDO = 0 ; Albedo material (same as: MATERIAL_MAP_DIFFUSE)
           MATERIAL_MAP_METALNESS = 1 ; Metalness material (same as: MATERIAL_MAP_SPECULAR)
           MATERIAL_MAP_NORMAL = 2 ; Normal material
           MATERIAL_MAP_ROUGHNESS = 3 ; Roughness material
           MATERIAL_MAP_OCCLUSION = 4 ; Ambient occlusion material
           MATERIAL_MAP_EMISSION = 5 ; Emission material
           MATERIAL_MAP_HEIGHT = 6 ; Heightmap material
           MATERIAL_MAP_CUBEMAP = 7 ; Cubemap material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
           MATERIAL_MAP_IRRADIANCE = 8 ; Irradiance material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
           MATERIAL_MAP_PREFILTER = 9 ; Prefilter material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
           MATERIAL_MAP_BRDF = 10 ; Brdf material
           )))

;; Shader location index
(define _ShaderLocationIndex
  (_enum '(SHADER_LOC_VERTEX_POSITION = 0 ; Shader location: vertex attribute: position
           SHADER_LOC_VERTEX_TEXCOORD01 = 1 ; Shader location: vertex attribute: texcoord01
           SHADER_LOC_VERTEX_TEXCOORD02 = 2 ; Shader location: vertex attribute: texcoord02
           SHADER_LOC_VERTEX_NORMAL = 3 ; Shader location: vertex attribute: normal
           SHADER_LOC_VERTEX_TANGENT = 4 ; Shader location: vertex attribute: tangent
           SHADER_LOC_VERTEX_COLOR = 5 ; Shader location: vertex attribute: color
           SHADER_LOC_MATRIX_MVP = 6 ; Shader location: matrix uniform: model-view-projection
           SHADER_LOC_MATRIX_VIEW = 7 ; Shader location: matrix uniform: view (camera transform)
           SHADER_LOC_MATRIX_PROJECTION = 8 ; Shader location: matrix uniform: projection
           SHADER_LOC_MATRIX_MODEL = 9 ; Shader location: matrix uniform: model (transform)
           SHADER_LOC_MATRIX_NORMAL = 10 ; Shader location: matrix uniform: normal
           SHADER_LOC_VECTOR_VIEW = 11 ; Shader location: vector uniform: view
           SHADER_LOC_COLOR_DIFFUSE = 12 ; Shader location: vector uniform: diffuse color
           SHADER_LOC_COLOR_SPECULAR = 13 ; Shader location: vector uniform: specular color
           SHADER_LOC_COLOR_AMBIENT = 14 ; Shader location: vector uniform: ambient color
           SHADER_LOC_MAP_ALBEDO = 15 ; Shader location: sampler2d texture: albedo (same as: SHADER_LOC_MAP_DIFFUSE)
           SHADER_LOC_MAP_METALNESS = 16 ; Shader location: sampler2d texture: metalness (same as: SHADER_LOC_MAP_SPECULAR)
           SHADER_LOC_MAP_NORMAL = 17 ; Shader location: sampler2d texture: normal
           SHADER_LOC_MAP_ROUGHNESS = 18 ; Shader location: sampler2d texture: roughness
           SHADER_LOC_MAP_OCCLUSION = 19 ; Shader location: sampler2d texture: occlusion
           SHADER_LOC_MAP_EMISSION = 20 ; Shader location: sampler2d texture: emission
           SHADER_LOC_MAP_HEIGHT = 21 ; Shader location: sampler2d texture: height
           SHADER_LOC_MAP_CUBEMAP = 22 ; Shader location: samplerCube texture: cubemap
           SHADER_LOC_MAP_IRRADIANCE = 23 ; Shader location: samplerCube texture: irradiance
           SHADER_LOC_MAP_PREFILTER = 24 ; Shader location: samplerCube texture: prefilter
           SHADER_LOC_MAP_BRDF = 25 ; Shader location: sampler2d texture: brdf
           )))

;; Shader uniform data type
(define _ShaderUniformDataType
  (_enum '(SHADER_UNIFORM_FLOAT = 0 ; Shader uniform type: float
           SHADER_UNIFORM_VEC2 = 1 ; Shader uniform type: vec2 (2 float)
           SHADER_UNIFORM_VEC3 = 2 ; Shader uniform type: vec3 (3 float)
           SHADER_UNIFORM_VEC4 = 3 ; Shader uniform type: vec4 (4 float)
           SHADER_UNIFORM_INT = 4 ; Shader uniform type: int
           SHADER_UNIFORM_IVEC2 = 5 ; Shader uniform type: ivec2 (2 int)
           SHADER_UNIFORM_IVEC3 = 6 ; Shader uniform type: ivec3 (3 int)
           SHADER_UNIFORM_IVEC4 = 7 ; Shader uniform type: ivec4 (4 int)
           SHADER_UNIFORM_SAMPLER2D = 8 ; Shader uniform type: sampler2d
           )))

;; Shader attribute data types
(define _ShaderAttributeDataType
  (_enum '(SHADER_ATTRIB_FLOAT = 0 ; Shader attribute type: float
           SHADER_ATTRIB_VEC2 = 1 ; Shader attribute type: vec2 (2 float)
           SHADER_ATTRIB_VEC3 = 2 ; Shader attribute type: vec3 (3 float)
           SHADER_ATTRIB_VEC4 = 3 ; Shader attribute type: vec4 (4 float)
           )))

;; Pixel formats
(define _PixelFormat
  (_enum '(PIXELFORMAT_UNCOMPRESSED_GRAYSCALE = 1 ; 8 bit per pixel (no alpha)
           PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA = 2 ; 8*2 bpp (2 channels)
           PIXELFORMAT_UNCOMPRESSED_R5G6B5 = 3 ; 16 bpp
           PIXELFORMAT_UNCOMPRESSED_R8G8B8 = 4 ; 24 bpp
           PIXELFORMAT_UNCOMPRESSED_R5G5B5A1 = 5 ; 16 bpp (1 bit alpha)
           PIXELFORMAT_UNCOMPRESSED_R4G4B4A4 = 6 ; 16 bpp (4 bit alpha)
           PIXELFORMAT_UNCOMPRESSED_R8G8B8A8 = 7 ; 32 bpp
           PIXELFORMAT_UNCOMPRESSED_R32 = 8 ; 32 bpp (1 channel - float)
           PIXELFORMAT_UNCOMPRESSED_R32G32B32 = 9 ; 32*3 bpp (3 channels - float)
           PIXELFORMAT_UNCOMPRESSED_R32G32B32A32 = 10 ; 32*4 bpp (4 channels - float)
           PIXELFORMAT_COMPRESSED_DXT1_RGB = 11 ; 4 bpp (no alpha)
           PIXELFORMAT_COMPRESSED_DXT1_RGBA = 12 ; 4 bpp (1 bit alpha)
           PIXELFORMAT_COMPRESSED_DXT3_RGBA = 13 ; 8 bpp
           PIXELFORMAT_COMPRESSED_DXT5_RGBA = 14 ; 8 bpp
           PIXELFORMAT_COMPRESSED_ETC1_RGB = 15 ; 4 bpp
           PIXELFORMAT_COMPRESSED_ETC2_RGB = 16 ; 4 bpp
           PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA = 17 ; 8 bpp
           PIXELFORMAT_COMPRESSED_PVRT_RGB = 18 ; 4 bpp
           PIXELFORMAT_COMPRESSED_PVRT_RGBA = 19 ; 4 bpp
           PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA = 20 ; 8 bpp
           PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA = 21 ; 2 bpp
           )))

;; Texture parameters: filter mode
(define _TextureFilter
  (_enum '(TEXTURE_FILTER_POINT = 0 ; No filter, just pixel approximation
           TEXTURE_FILTER_BILINEAR = 1 ; Linear filtering
           TEXTURE_FILTER_TRILINEAR = 2 ; Trilinear filtering (linear with mipmaps)
           TEXTURE_FILTER_ANISOTROPIC_4X = 3 ; Anisotropic filtering 4x
           TEXTURE_FILTER_ANISOTROPIC_8X = 4 ; Anisotropic filtering 8x
           TEXTURE_FILTER_ANISOTROPIC_16X = 5 ; Anisotropic filtering 16x
           )))

;; Texture parameters: wrap mode
(define _TextureWrap
  (_enum '(TEXTURE_WRAP_REPEAT = 0 ; Repeats texture in tiled mode
           TEXTURE_WRAP_CLAMP = 1 ; Clamps texture to edge pixel in tiled mode
           TEXTURE_WRAP_MIRROR_REPEAT = 2 ; Mirrors and repeats the texture in tiled mode
           TEXTURE_WRAP_MIRROR_CLAMP = 3 ; Mirrors and clamps to border the texture in tiled mode
           )))

;; Cubemap layouts
(define _CubemapLayout
  (_enum '(CUBEMAP_LAYOUT_AUTO_DETECT = 0 ; Automatically detect layout type
           CUBEMAP_LAYOUT_LINE_VERTICAL = 1 ; Layout is defined by a vertical line with faces
           CUBEMAP_LAYOUT_LINE_HORIZONTAL = 2 ; Layout is defined by an horizontal line with faces
           CUBEMAP_LAYOUT_CROSS_THREE_BY_FOUR = 3 ; Layout is defined by a 3x4 cross with cubemap faces
           CUBEMAP_LAYOUT_CROSS_FOUR_BY_THREE = 4 ; Layout is defined by a 4x3 cross with cubemap faces
           CUBEMAP_LAYOUT_PANORAMA = 5 ; Layout is defined by a panorama image (equirectangular map)
           )))

;; Font type, defines generation method
(define _FontType
  (_enum '(FONT_DEFAULT = 0 ; Default font generation, anti-aliased
           FONT_BITMAP = 1 ; Bitmap font generation, no anti-aliasing
           FONT_SDF = 2 ; SDF font generation, requires external shader
           )))

;; Color blending modes (pre-defined)
(define _BlendMode
  (_enum '(BLEND_ALPHA = 0 ; Blend textures considering alpha (default)
           BLEND_ADDITIVE = 1 ; Blend textures adding colors
           BLEND_MULTIPLIED = 2 ; Blend textures multiplying colors
           BLEND_ADD_COLORS = 3 ; Blend textures adding colors (alternative)
           BLEND_SUBTRACT_COLORS = 4 ; Blend textures subtracting colors (alternative)
           BLEND_CUSTOM = 5 ; Belnd textures using custom src/dst factors (use rlSetBlendMode())
           )))

;; Gesture
(define _Gesture
  (_enum '(GESTURE_NONE = 0 ; No gesture
           GESTURE_TAP = 1 ; Tap gesture
           GESTURE_DOUBLETAP = 2 ; Double tap gesture
           GESTURE_HOLD = 4 ; Hold gesture
           GESTURE_DRAG = 8 ; Drag gesture
           GESTURE_SWIPE_RIGHT = 16 ; Swipe right gesture
           GESTURE_SWIPE_LEFT = 32 ; Swipe left gesture
           GESTURE_SWIPE_UP = 64 ; Swipe up gesture
           GESTURE_SWIPE_DOWN = 128 ; Swipe down gesture
           GESTURE_PINCH_IN = 256 ; Pinch in gesture
           GESTURE_PINCH_OUT = 512 ; Pinch out gesture
           )))

;; Camera system modes
(define _CameraMode
  (_enum '(CAMERA_CUSTOM = 0 ; Custom camera
           CAMERA_FREE = 1 ; Free camera
           CAMERA_ORBITAL = 2 ; Orbital camera
           CAMERA_FIRST_PERSON = 3 ; First person camera
           CAMERA_THIRD_PERSON = 4 ; Third person camera
           )))

;; Camera projection
(define _CameraProjection
  (_enum '(CAMERA_PERSPECTIVE = 0 ; Perspective projection
           CAMERA_ORTHOGRAPHIC = 1 ; Orthographic projection
           )))

;; N-patch layout
(define _NPatchLayout
  (_enum '(NPATCH_NINE_PATCH = 0 ; Npatch layout: 3x3 tiles
           NPATCH_THREE_PATCH_VERTICAL = 1 ; Npatch layout: 1x3 tiles
           NPATCH_THREE_PATCH_HORIZONTAL = 2 ; Npatch layout: 3x1 tiles
           )))
