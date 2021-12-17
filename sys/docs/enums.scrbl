#lang scribble/manual

@(require (for-label raylib/sys/enums ffi/unsafe racket/base))

@title{Enums}
@defmodule[raylib/sys/enums]

@section{System/Window config flags}
@defthing[_ConfigFlags ctype?]{System/Window config flags}
@defthing[FLAG_VSYNC_HINT exact-integer? #:value 64 "Set to try enabling V-Sync on GPU"]
@defthing[FLAG_FULLSCREEN_MODE exact-integer? #:value 2 "Set to run program in fullscreen"]
@defthing[FLAG_WINDOW_RESIZABLE exact-integer? #:value 4 "Set to allow resizable window"]
@defthing[FLAG_WINDOW_UNDECORATED exact-integer? #:value 8 "Set to disable window decoration (frame and buttons)"]
@defthing[FLAG_WINDOW_HIDDEN exact-integer? #:value 128 "Set to hide window"]
@defthing[FLAG_WINDOW_MINIMIZED exact-integer? #:value 512 "Set to minimize window (iconify)"]
@defthing[FLAG_WINDOW_MAXIMIZED exact-integer? #:value 1024 "Set to maximize window (expanded to monitor)"]
@defthing[FLAG_WINDOW_UNFOCUSED exact-integer? #:value 2048 "Set to window non focused"]
@defthing[FLAG_WINDOW_TOPMOST exact-integer? #:value 4096 "Set to window always on top"]
@defthing[FLAG_WINDOW_ALWAYS_RUN exact-integer? #:value 256 "Set to allow windows running while minimized"]
@defthing[FLAG_WINDOW_TRANSPARENT exact-integer? #:value 16 "Set to allow transparent framebuffer"]
@defthing[FLAG_WINDOW_HIGHDPI exact-integer? #:value 8192 "Set to support HighDPI"]
@defthing[FLAG_MSAA_4X_HINT exact-integer? #:value 32 "Set to try enabling MSAA 4X"]
@defthing[FLAG_INTERLACED_HINT exact-integer? #:value 65536 "Set to try enabling interlaced video format (for V3D)"]

@section{Trace log level}
@defthing[_TraceLogLevel ctype?]{Trace log level}
@defthing[LOG_ALL exact-integer? #:value 0 "Display all logs"]
@defthing[LOG_TRACE exact-integer? #:value 1 "Trace logging, intended for internal use only"]
@defthing[LOG_DEBUG exact-integer? #:value 2 "Debug logging, used for internal debugging, it should be disabled on release builds"]
@defthing[LOG_INFO exact-integer? #:value 3 "Info logging, used for program execution info"]
@defthing[LOG_WARNING exact-integer? #:value 4 "Warning logging, used on recoverable failures"]
@defthing[LOG_ERROR exact-integer? #:value 5 "Error logging, used on unrecoverable failures"]
@defthing[LOG_FATAL exact-integer? #:value 6 "Fatal logging, used to abort program: exit(EXIT_FAILURE)"]
@defthing[LOG_NONE exact-integer? #:value 7 "Disable logging"]

@section{Keyboard keys (US keyboard layout)}
@defthing[_KeyboardKey ctype?]{Keyboard keys (US keyboard layout)}
@defthing[KEY_NULL exact-integer? #:value 0 "Key: NULL, used for no key pressed"]
@defthing[KEY_APOSTROPHE exact-integer? #:value 39 "Key: '"]
@defthing[KEY_COMMA exact-integer? #:value 44 "Key: ,"]
@defthing[KEY_MINUS exact-integer? #:value 45 "Key: -"]
@defthing[KEY_PERIOD exact-integer? #:value 46 "Key: ."]
@defthing[KEY_SLASH exact-integer? #:value 47 "Key: /"]
@defthing[KEY_ZERO exact-integer? #:value 48 "Key: 0"]
@defthing[KEY_ONE exact-integer? #:value 49 "Key: 1"]
@defthing[KEY_TWO exact-integer? #:value 50 "Key: 2"]
@defthing[KEY_THREE exact-integer? #:value 51 "Key: 3"]
@defthing[KEY_FOUR exact-integer? #:value 52 "Key: 4"]
@defthing[KEY_FIVE exact-integer? #:value 53 "Key: 5"]
@defthing[KEY_SIX exact-integer? #:value 54 "Key: 6"]
@defthing[KEY_SEVEN exact-integer? #:value 55 "Key: 7"]
@defthing[KEY_EIGHT exact-integer? #:value 56 "Key: 8"]
@defthing[KEY_NINE exact-integer? #:value 57 "Key: 9"]
@defthing[KEY_SEMICOLON exact-integer? #:value 59 "Key: ;"]
@defthing[KEY_EQUAL exact-integer? #:value 61 "Key: ="]
@defthing[KEY_A exact-integer? #:value 65 "Key: A | a"]
@defthing[KEY_B exact-integer? #:value 66 "Key: B | b"]
@defthing[KEY_C exact-integer? #:value 67 "Key: C | c"]
@defthing[KEY_D exact-integer? #:value 68 "Key: D | d"]
@defthing[KEY_E exact-integer? #:value 69 "Key: E | e"]
@defthing[KEY_F exact-integer? #:value 70 "Key: F | f"]
@defthing[KEY_G exact-integer? #:value 71 "Key: G | g"]
@defthing[KEY_H exact-integer? #:value 72 "Key: H | h"]
@defthing[KEY_I exact-integer? #:value 73 "Key: I | i"]
@defthing[KEY_J exact-integer? #:value 74 "Key: J | j"]
@defthing[KEY_K exact-integer? #:value 75 "Key: K | k"]
@defthing[KEY_L exact-integer? #:value 76 "Key: L | l"]
@defthing[KEY_M exact-integer? #:value 77 "Key: M | m"]
@defthing[KEY_N exact-integer? #:value 78 "Key: N | n"]
@defthing[KEY_O exact-integer? #:value 79 "Key: O | o"]
@defthing[KEY_P exact-integer? #:value 80 "Key: P | p"]
@defthing[KEY_Q exact-integer? #:value 81 "Key: Q | q"]
@defthing[KEY_R exact-integer? #:value 82 "Key: R | r"]
@defthing[KEY_S exact-integer? #:value 83 "Key: S | s"]
@defthing[KEY_T exact-integer? #:value 84 "Key: T | t"]
@defthing[KEY_U exact-integer? #:value 85 "Key: U | u"]
@defthing[KEY_V exact-integer? #:value 86 "Key: V | v"]
@defthing[KEY_W exact-integer? #:value 87 "Key: W | w"]
@defthing[KEY_X exact-integer? #:value 88 "Key: X | x"]
@defthing[KEY_Y exact-integer? #:value 89 "Key: Y | y"]
@defthing[KEY_Z exact-integer? #:value 90 "Key: Z | z"]
@defthing[KEY_LEFT_BRACKET exact-integer? #:value 91 "Key: ["]
@defthing[KEY_BACKSLASH exact-integer? #:value 92 "Key: '\\'"]
@defthing[KEY_RIGHT_BRACKET exact-integer? #:value 93 "Key: ]"]
@defthing[KEY_GRAVE exact-integer? #:value 96 "Key: `"]
@defthing[KEY_SPACE exact-integer? #:value 32 "Key: Space"]
@defthing[KEY_ESCAPE exact-integer? #:value 256 "Key: Esc"]
@defthing[KEY_ENTER exact-integer? #:value 257 "Key: Enter"]
@defthing[KEY_TAB exact-integer? #:value 258 "Key: Tab"]
@defthing[KEY_BACKSPACE exact-integer? #:value 259 "Key: Backspace"]
@defthing[KEY_INSERT exact-integer? #:value 260 "Key: Ins"]
@defthing[KEY_DELETE exact-integer? #:value 261 "Key: Del"]
@defthing[KEY_RIGHT exact-integer? #:value 262 "Key: Cursor right"]
@defthing[KEY_LEFT exact-integer? #:value 263 "Key: Cursor left"]
@defthing[KEY_DOWN exact-integer? #:value 264 "Key: Cursor down"]
@defthing[KEY_UP exact-integer? #:value 265 "Key: Cursor up"]
@defthing[KEY_PAGE_UP exact-integer? #:value 266 "Key: Page up"]
@defthing[KEY_PAGE_DOWN exact-integer? #:value 267 "Key: Page down"]
@defthing[KEY_HOME exact-integer? #:value 268 "Key: Home"]
@defthing[KEY_END exact-integer? #:value 269 "Key: End"]
@defthing[KEY_CAPS_LOCK exact-integer? #:value 280 "Key: Caps lock"]
@defthing[KEY_SCROLL_LOCK exact-integer? #:value 281 "Key: Scroll down"]
@defthing[KEY_NUM_LOCK exact-integer? #:value 282 "Key: Num lock"]
@defthing[KEY_PRINT_SCREEN exact-integer? #:value 283 "Key: Print screen"]
@defthing[KEY_PAUSE exact-integer? #:value 284 "Key: Pause"]
@defthing[KEY_F1 exact-integer? #:value 290 "Key: F1"]
@defthing[KEY_F2 exact-integer? #:value 291 "Key: F2"]
@defthing[KEY_F3 exact-integer? #:value 292 "Key: F3"]
@defthing[KEY_F4 exact-integer? #:value 293 "Key: F4"]
@defthing[KEY_F5 exact-integer? #:value 294 "Key: F5"]
@defthing[KEY_F6 exact-integer? #:value 295 "Key: F6"]
@defthing[KEY_F7 exact-integer? #:value 296 "Key: F7"]
@defthing[KEY_F8 exact-integer? #:value 297 "Key: F8"]
@defthing[KEY_F9 exact-integer? #:value 298 "Key: F9"]
@defthing[KEY_F10 exact-integer? #:value 299 "Key: F10"]
@defthing[KEY_F11 exact-integer? #:value 300 "Key: F11"]
@defthing[KEY_F12 exact-integer? #:value 301 "Key: F12"]
@defthing[KEY_LEFT_SHIFT exact-integer? #:value 340 "Key: Shift left"]
@defthing[KEY_LEFT_CONTROL exact-integer? #:value 341 "Key: Control left"]
@defthing[KEY_LEFT_ALT exact-integer? #:value 342 "Key: Alt left"]
@defthing[KEY_LEFT_SUPER exact-integer? #:value 343 "Key: Super left"]
@defthing[KEY_RIGHT_SHIFT exact-integer? #:value 344 "Key: Shift right"]
@defthing[KEY_RIGHT_CONTROL exact-integer? #:value 345 "Key: Control right"]
@defthing[KEY_RIGHT_ALT exact-integer? #:value 346 "Key: Alt right"]
@defthing[KEY_RIGHT_SUPER exact-integer? #:value 347 "Key: Super right"]
@defthing[KEY_KB_MENU exact-integer? #:value 348 "Key: KB menu"]
@defthing[KEY_KP_0 exact-integer? #:value 320 "Key: Keypad 0"]
@defthing[KEY_KP_1 exact-integer? #:value 321 "Key: Keypad 1"]
@defthing[KEY_KP_2 exact-integer? #:value 322 "Key: Keypad 2"]
@defthing[KEY_KP_3 exact-integer? #:value 323 "Key: Keypad 3"]
@defthing[KEY_KP_4 exact-integer? #:value 324 "Key: Keypad 4"]
@defthing[KEY_KP_5 exact-integer? #:value 325 "Key: Keypad 5"]
@defthing[KEY_KP_6 exact-integer? #:value 326 "Key: Keypad 6"]
@defthing[KEY_KP_7 exact-integer? #:value 327 "Key: Keypad 7"]
@defthing[KEY_KP_8 exact-integer? #:value 328 "Key: Keypad 8"]
@defthing[KEY_KP_9 exact-integer? #:value 329 "Key: Keypad 9"]
@defthing[KEY_KP_DECIMAL exact-integer? #:value 330 "Key: Keypad ."]
@defthing[KEY_KP_DIVIDE exact-integer? #:value 331 "Key: Keypad /"]
@defthing[KEY_KP_MULTIPLY exact-integer? #:value 332 "Key: Keypad *"]
@defthing[KEY_KP_SUBTRACT exact-integer? #:value 333 "Key: Keypad -"]
@defthing[KEY_KP_ADD exact-integer? #:value 334 "Key: Keypad +"]
@defthing[KEY_KP_ENTER exact-integer? #:value 335 "Key: Keypad Enter"]
@defthing[KEY_KP_EQUAL exact-integer? #:value 336 "Key: Keypad ="]
@defthing[KEY_BACK exact-integer? #:value 4 "Key: Android back button"]
@defthing[KEY_MENU exact-integer? #:value 82 "Key: Android menu button"]
@defthing[KEY_VOLUME_UP exact-integer? #:value 24 "Key: Android volume up button"]
@defthing[KEY_VOLUME_DOWN exact-integer? #:value 25 "Key: Android volume down button"]

@section{Mouse buttons}
@defthing[_MouseButton ctype?]{Mouse buttons}
@defthing[MOUSE_BUTTON_LEFT exact-integer? #:value 0 "Mouse button left"]
@defthing[MOUSE_BUTTON_RIGHT exact-integer? #:value 1 "Mouse button right"]
@defthing[MOUSE_BUTTON_MIDDLE exact-integer? #:value 2 "Mouse button middle (pressed wheel)"]
@defthing[MOUSE_BUTTON_SIDE exact-integer? #:value 3 "Mouse button side (advanced mouse device)"]
@defthing[MOUSE_BUTTON_EXTRA exact-integer? #:value 4 "Mouse button extra (advanced mouse device)"]
@defthing[MOUSE_BUTTON_FORWARD exact-integer? #:value 5 "Mouse button fordward (advanced mouse device)"]
@defthing[MOUSE_BUTTON_BACK exact-integer? #:value 6 "Mouse button back (advanced mouse device)"]

@section{Mouse cursor}
@defthing[_MouseCursor ctype?]{Mouse cursor}
@defthing[MOUSE_CURSOR_DEFAULT exact-integer? #:value 0 "Default pointer shape"]
@defthing[MOUSE_CURSOR_ARROW exact-integer? #:value 1 "Arrow shape"]
@defthing[MOUSE_CURSOR_IBEAM exact-integer? #:value 2 "Text writing cursor shape"]
@defthing[MOUSE_CURSOR_CROSSHAIR exact-integer? #:value 3 "Cross shape"]
@defthing[MOUSE_CURSOR_POINTING_HAND exact-integer? #:value 4 "Pointing hand cursor"]
@defthing[MOUSE_CURSOR_RESIZE_EW exact-integer? #:value 5 "Horizontal resize/move arrow shape"]
@defthing[MOUSE_CURSOR_RESIZE_NS exact-integer? #:value 6 "Vertical resize/move arrow shape"]
@defthing[MOUSE_CURSOR_RESIZE_NWSE exact-integer? #:value 7 "Top-left to bottom-right diagonal resize/move arrow shape"]
@defthing[MOUSE_CURSOR_RESIZE_NESW exact-integer? #:value 8 "The top-right to bottom-left diagonal resize/move arrow shape"]
@defthing[MOUSE_CURSOR_RESIZE_ALL exact-integer? #:value 9 "The omni-directional resize/move cursor shape"]
@defthing[MOUSE_CURSOR_NOT_ALLOWED exact-integer? #:value 10 "The operation-not-allowed shape"]

@section{Gamepad buttons}
@defthing[_GamepadButton ctype?]{Gamepad buttons}
@defthing[GAMEPAD_BUTTON_UNKNOWN exact-integer? #:value 0 "Unknown button, just for error checking"]
@defthing[GAMEPAD_BUTTON_LEFT_FACE_UP exact-integer? #:value 1 "Gamepad left DPAD up button"]
@defthing[GAMEPAD_BUTTON_LEFT_FACE_RIGHT exact-integer? #:value 2 "Gamepad left DPAD right button"]
@defthing[GAMEPAD_BUTTON_LEFT_FACE_DOWN exact-integer? #:value 3 "Gamepad left DPAD down button"]
@defthing[GAMEPAD_BUTTON_LEFT_FACE_LEFT exact-integer? #:value 4 "Gamepad left DPAD left button"]
@defthing[GAMEPAD_BUTTON_RIGHT_FACE_UP exact-integer? #:value 5 "Gamepad right button up (i.e. PS3: Triangle, Xbox: Y)"]
@defthing[GAMEPAD_BUTTON_RIGHT_FACE_RIGHT exact-integer? #:value 6 "Gamepad right button right (i.e. PS3: Square, Xbox: X)"]
@defthing[GAMEPAD_BUTTON_RIGHT_FACE_DOWN exact-integer? #:value 7 "Gamepad right button down (i.e. PS3: Cross, Xbox: A)"]
@defthing[GAMEPAD_BUTTON_RIGHT_FACE_LEFT exact-integer? #:value 8 "Gamepad right button left (i.e. PS3: Circle, Xbox: B)"]
@defthing[GAMEPAD_BUTTON_LEFT_TRIGGER_1 exact-integer? #:value 9 "Gamepad top/back trigger left (first), it could be a trailing button"]
@defthing[GAMEPAD_BUTTON_LEFT_TRIGGER_2 exact-integer? #:value 10 "Gamepad top/back trigger left (second), it could be a trailing button"]
@defthing[GAMEPAD_BUTTON_RIGHT_TRIGGER_1 exact-integer? #:value 11 "Gamepad top/back trigger right (one), it could be a trailing button"]
@defthing[GAMEPAD_BUTTON_RIGHT_TRIGGER_2 exact-integer? #:value 12 "Gamepad top/back trigger right (second), it could be a trailing button"]
@defthing[GAMEPAD_BUTTON_MIDDLE_LEFT exact-integer? #:value 13 "Gamepad center buttons, left one (i.e. PS3: Select)"]
@defthing[GAMEPAD_BUTTON_MIDDLE exact-integer? #:value 14 "Gamepad center buttons, middle one (i.e. PS3: PS, Xbox: XBOX)"]
@defthing[GAMEPAD_BUTTON_MIDDLE_RIGHT exact-integer? #:value 15 "Gamepad center buttons, right one (i.e. PS3: Start)"]
@defthing[GAMEPAD_BUTTON_LEFT_THUMB exact-integer? #:value 16 "Gamepad joystick pressed button left"]
@defthing[GAMEPAD_BUTTON_RIGHT_THUMB exact-integer? #:value 17 "Gamepad joystick pressed button right"]

@section{Gamepad axis}
@defthing[_GamepadAxis ctype?]{Gamepad axis}
@defthing[GAMEPAD_AXIS_LEFT_X exact-integer? #:value 0 "Gamepad left stick X axis"]
@defthing[GAMEPAD_AXIS_LEFT_Y exact-integer? #:value 1 "Gamepad left stick Y axis"]
@defthing[GAMEPAD_AXIS_RIGHT_X exact-integer? #:value 2 "Gamepad right stick X axis"]
@defthing[GAMEPAD_AXIS_RIGHT_Y exact-integer? #:value 3 "Gamepad right stick Y axis"]
@defthing[GAMEPAD_AXIS_LEFT_TRIGGER exact-integer? #:value 4 "Gamepad back trigger left, pressure level: [1..-1]"]
@defthing[GAMEPAD_AXIS_RIGHT_TRIGGER exact-integer? #:value 5 "Gamepad back trigger right, pressure level: [1..-1]"]

@section{Material map index}
@defthing[_MaterialMapIndex ctype?]{Material map index}
@defthing[MATERIAL_MAP_ALBEDO exact-integer? #:value 0 "Albedo material (same as: MATERIAL_MAP_DIFFUSE)"]
@defthing[MATERIAL_MAP_METALNESS exact-integer? #:value 1 "Metalness material (same as: MATERIAL_MAP_SPECULAR)"]
@defthing[MATERIAL_MAP_NORMAL exact-integer? #:value 2 "Normal material"]
@defthing[MATERIAL_MAP_ROUGHNESS exact-integer? #:value 3 "Roughness material"]
@defthing[MATERIAL_MAP_OCCLUSION exact-integer? #:value 4 "Ambient occlusion material"]
@defthing[MATERIAL_MAP_EMISSION exact-integer? #:value 5 "Emission material"]
@defthing[MATERIAL_MAP_HEIGHT exact-integer? #:value 6 "Heightmap material"]
@defthing[MATERIAL_MAP_CUBEMAP exact-integer? #:value 7 "Cubemap material (NOTE: Uses GL_TEXTURE_CUBE_MAP)"]
@defthing[MATERIAL_MAP_IRRADIANCE exact-integer? #:value 8 "Irradiance material (NOTE: Uses GL_TEXTURE_CUBE_MAP)"]
@defthing[MATERIAL_MAP_PREFILTER exact-integer? #:value 9 "Prefilter material (NOTE: Uses GL_TEXTURE_CUBE_MAP)"]
@defthing[MATERIAL_MAP_BRDF exact-integer? #:value 10 "Brdf material"]

@section{Shader location index}
@defthing[_ShaderLocationIndex ctype?]{Shader location index}
@defthing[SHADER_LOC_VERTEX_POSITION exact-integer? #:value 0 "Shader location: vertex attribute: position"]
@defthing[SHADER_LOC_VERTEX_TEXCOORD01 exact-integer? #:value 1 "Shader location: vertex attribute: texcoord01"]
@defthing[SHADER_LOC_VERTEX_TEXCOORD02 exact-integer? #:value 2 "Shader location: vertex attribute: texcoord02"]
@defthing[SHADER_LOC_VERTEX_NORMAL exact-integer? #:value 3 "Shader location: vertex attribute: normal"]
@defthing[SHADER_LOC_VERTEX_TANGENT exact-integer? #:value 4 "Shader location: vertex attribute: tangent"]
@defthing[SHADER_LOC_VERTEX_COLOR exact-integer? #:value 5 "Shader location: vertex attribute: color"]
@defthing[SHADER_LOC_MATRIX_MVP exact-integer? #:value 6 "Shader location: matrix uniform: model-view-projection"]
@defthing[SHADER_LOC_MATRIX_VIEW exact-integer? #:value 7 "Shader location: matrix uniform: view (camera transform)"]
@defthing[SHADER_LOC_MATRIX_PROJECTION exact-integer? #:value 8 "Shader location: matrix uniform: projection"]
@defthing[SHADER_LOC_MATRIX_MODEL exact-integer? #:value 9 "Shader location: matrix uniform: model (transform)"]
@defthing[SHADER_LOC_MATRIX_NORMAL exact-integer? #:value 10 "Shader location: matrix uniform: normal"]
@defthing[SHADER_LOC_VECTOR_VIEW exact-integer? #:value 11 "Shader location: vector uniform: view"]
@defthing[SHADER_LOC_COLOR_DIFFUSE exact-integer? #:value 12 "Shader location: vector uniform: diffuse color"]
@defthing[SHADER_LOC_COLOR_SPECULAR exact-integer? #:value 13 "Shader location: vector uniform: specular color"]
@defthing[SHADER_LOC_COLOR_AMBIENT exact-integer? #:value 14 "Shader location: vector uniform: ambient color"]
@defthing[SHADER_LOC_MAP_ALBEDO exact-integer? #:value 15 "Shader location: sampler2d texture: albedo (same as: SHADER_LOC_MAP_DIFFUSE)"]
@defthing[SHADER_LOC_MAP_METALNESS exact-integer? #:value 16 "Shader location: sampler2d texture: metalness (same as: SHADER_LOC_MAP_SPECULAR)"]
@defthing[SHADER_LOC_MAP_NORMAL exact-integer? #:value 17 "Shader location: sampler2d texture: normal"]
@defthing[SHADER_LOC_MAP_ROUGHNESS exact-integer? #:value 18 "Shader location: sampler2d texture: roughness"]
@defthing[SHADER_LOC_MAP_OCCLUSION exact-integer? #:value 19 "Shader location: sampler2d texture: occlusion"]
@defthing[SHADER_LOC_MAP_EMISSION exact-integer? #:value 20 "Shader location: sampler2d texture: emission"]
@defthing[SHADER_LOC_MAP_HEIGHT exact-integer? #:value 21 "Shader location: sampler2d texture: height"]
@defthing[SHADER_LOC_MAP_CUBEMAP exact-integer? #:value 22 "Shader location: samplerCube texture: cubemap"]
@defthing[SHADER_LOC_MAP_IRRADIANCE exact-integer? #:value 23 "Shader location: samplerCube texture: irradiance"]
@defthing[SHADER_LOC_MAP_PREFILTER exact-integer? #:value 24 "Shader location: samplerCube texture: prefilter"]
@defthing[SHADER_LOC_MAP_BRDF exact-integer? #:value 25 "Shader location: sampler2d texture: brdf"]

@section{Shader uniform data type}
@defthing[_ShaderUniformDataType ctype?]{Shader uniform data type}
@defthing[SHADER_UNIFORM_FLOAT exact-integer? #:value 0 "Shader uniform type: float"]
@defthing[SHADER_UNIFORM_VEC2 exact-integer? #:value 1 "Shader uniform type: vec2 (2 float)"]
@defthing[SHADER_UNIFORM_VEC3 exact-integer? #:value 2 "Shader uniform type: vec3 (3 float)"]
@defthing[SHADER_UNIFORM_VEC4 exact-integer? #:value 3 "Shader uniform type: vec4 (4 float)"]
@defthing[SHADER_UNIFORM_INT exact-integer? #:value 4 "Shader uniform type: int"]
@defthing[SHADER_UNIFORM_IVEC2 exact-integer? #:value 5 "Shader uniform type: ivec2 (2 int)"]
@defthing[SHADER_UNIFORM_IVEC3 exact-integer? #:value 6 "Shader uniform type: ivec3 (3 int)"]
@defthing[SHADER_UNIFORM_IVEC4 exact-integer? #:value 7 "Shader uniform type: ivec4 (4 int)"]
@defthing[SHADER_UNIFORM_SAMPLER2D exact-integer? #:value 8 "Shader uniform type: sampler2d"]

@section{Shader attribute data types}
@defthing[_ShaderAttributeDataType ctype?]{Shader attribute data types}
@defthing[SHADER_ATTRIB_FLOAT exact-integer? #:value 0 "Shader attribute type: float"]
@defthing[SHADER_ATTRIB_VEC2 exact-integer? #:value 1 "Shader attribute type: vec2 (2 float)"]
@defthing[SHADER_ATTRIB_VEC3 exact-integer? #:value 2 "Shader attribute type: vec3 (3 float)"]
@defthing[SHADER_ATTRIB_VEC4 exact-integer? #:value 3 "Shader attribute type: vec4 (4 float)"]

@section{Pixel formats}
@defthing[_PixelFormat ctype?]{Pixel formats}
@defthing[PIXELFORMAT_UNCOMPRESSED_GRAYSCALE exact-integer? #:value 1 "8 bit per pixel (no alpha)"]
@defthing[PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA exact-integer? #:value 2 "8*2 bpp (2 channels)"]
@defthing[PIXELFORMAT_UNCOMPRESSED_R5G6B5 exact-integer? #:value 3 "16 bpp"]
@defthing[PIXELFORMAT_UNCOMPRESSED_R8G8B8 exact-integer? #:value 4 "24 bpp"]
@defthing[PIXELFORMAT_UNCOMPRESSED_R5G5B5A1 exact-integer? #:value 5 "16 bpp (1 bit alpha)"]
@defthing[PIXELFORMAT_UNCOMPRESSED_R4G4B4A4 exact-integer? #:value 6 "16 bpp (4 bit alpha)"]
@defthing[PIXELFORMAT_UNCOMPRESSED_R8G8B8A8 exact-integer? #:value 7 "32 bpp"]
@defthing[PIXELFORMAT_UNCOMPRESSED_R32 exact-integer? #:value 8 "32 bpp (1 channel - float)"]
@defthing[PIXELFORMAT_UNCOMPRESSED_R32G32B32 exact-integer? #:value 9 "32*3 bpp (3 channels - float)"]
@defthing[PIXELFORMAT_UNCOMPRESSED_R32G32B32A32 exact-integer? #:value 10 "32*4 bpp (4 channels - float)"]
@defthing[PIXELFORMAT_COMPRESSED_DXT1_RGB exact-integer? #:value 11 "4 bpp (no alpha)"]
@defthing[PIXELFORMAT_COMPRESSED_DXT1_RGBA exact-integer? #:value 12 "4 bpp (1 bit alpha)"]
@defthing[PIXELFORMAT_COMPRESSED_DXT3_RGBA exact-integer? #:value 13 "8 bpp"]
@defthing[PIXELFORMAT_COMPRESSED_DXT5_RGBA exact-integer? #:value 14 "8 bpp"]
@defthing[PIXELFORMAT_COMPRESSED_ETC1_RGB exact-integer? #:value 15 "4 bpp"]
@defthing[PIXELFORMAT_COMPRESSED_ETC2_RGB exact-integer? #:value 16 "4 bpp"]
@defthing[PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA exact-integer? #:value 17 "8 bpp"]
@defthing[PIXELFORMAT_COMPRESSED_PVRT_RGB exact-integer? #:value 18 "4 bpp"]
@defthing[PIXELFORMAT_COMPRESSED_PVRT_RGBA exact-integer? #:value 19 "4 bpp"]
@defthing[PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA exact-integer? #:value 20 "8 bpp"]
@defthing[PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA exact-integer? #:value 21 "2 bpp"]

@section{Texture parameters: filter mode}
@defthing[_TextureFilter ctype?]{Texture parameters: filter mode}
@defthing[TEXTURE_FILTER_POINT exact-integer? #:value 0 "No filter, just pixel approximation"]
@defthing[TEXTURE_FILTER_BILINEAR exact-integer? #:value 1 "Linear filtering"]
@defthing[TEXTURE_FILTER_TRILINEAR exact-integer? #:value 2 "Trilinear filtering (linear with mipmaps)"]
@defthing[TEXTURE_FILTER_ANISOTROPIC_4X exact-integer? #:value 3 "Anisotropic filtering 4x"]
@defthing[TEXTURE_FILTER_ANISOTROPIC_8X exact-integer? #:value 4 "Anisotropic filtering 8x"]
@defthing[TEXTURE_FILTER_ANISOTROPIC_16X exact-integer? #:value 5 "Anisotropic filtering 16x"]

@section{Texture parameters: wrap mode}
@defthing[_TextureWrap ctype?]{Texture parameters: wrap mode}
@defthing[TEXTURE_WRAP_REPEAT exact-integer? #:value 0 "Repeats texture in tiled mode"]
@defthing[TEXTURE_WRAP_CLAMP exact-integer? #:value 1 "Clamps texture to edge pixel in tiled mode"]
@defthing[TEXTURE_WRAP_MIRROR_REPEAT exact-integer? #:value 2 "Mirrors and repeats the texture in tiled mode"]
@defthing[TEXTURE_WRAP_MIRROR_CLAMP exact-integer? #:value 3 "Mirrors and clamps to border the texture in tiled mode"]

@section{Cubemap layouts}
@defthing[_CubemapLayout ctype?]{Cubemap layouts}
@defthing[CUBEMAP_LAYOUT_AUTO_DETECT exact-integer? #:value 0 "Automatically detect layout type"]
@defthing[CUBEMAP_LAYOUT_LINE_VERTICAL exact-integer? #:value 1 "Layout is defined by a vertical line with faces"]
@defthing[CUBEMAP_LAYOUT_LINE_HORIZONTAL exact-integer? #:value 2 "Layout is defined by an horizontal line with faces"]
@defthing[CUBEMAP_LAYOUT_CROSS_THREE_BY_FOUR exact-integer? #:value 3 "Layout is defined by a 3x4 cross with cubemap faces"]
@defthing[CUBEMAP_LAYOUT_CROSS_FOUR_BY_THREE exact-integer? #:value 4 "Layout is defined by a 4x3 cross with cubemap faces"]
@defthing[CUBEMAP_LAYOUT_PANORAMA exact-integer? #:value 5 "Layout is defined by a panorama image (equirectangular map)"]

@section{Font type, defines generation method}
@defthing[_FontType ctype?]{Font type, defines generation method}
@defthing[FONT_DEFAULT exact-integer? #:value 0 "Default font generation, anti-aliased"]
@defthing[FONT_BITMAP exact-integer? #:value 1 "Bitmap font generation, no anti-aliasing"]
@defthing[FONT_SDF exact-integer? #:value 2 "SDF font generation, requires external shader"]

@section{Color blending modes (pre-defined)}
@defthing[_BlendMode ctype?]{Color blending modes (pre-defined)}
@defthing[BLEND_ALPHA exact-integer? #:value 0 "Blend textures considering alpha (default)"]
@defthing[BLEND_ADDITIVE exact-integer? #:value 1 "Blend textures adding colors"]
@defthing[BLEND_MULTIPLIED exact-integer? #:value 2 "Blend textures multiplying colors"]
@defthing[BLEND_ADD_COLORS exact-integer? #:value 3 "Blend textures adding colors (alternative)"]
@defthing[BLEND_SUBTRACT_COLORS exact-integer? #:value 4 "Blend textures subtracting colors (alternative)"]
@defthing[BLEND_CUSTOM exact-integer? #:value 5 "Belnd textures using custom src/dst factors (use rlSetBlendMode())"]

@section{Gesture}
@defthing[_Gesture ctype?]{Gesture}
@defthing[GESTURE_NONE exact-integer? #:value 0 "No gesture"]
@defthing[GESTURE_TAP exact-integer? #:value 1 "Tap gesture"]
@defthing[GESTURE_DOUBLETAP exact-integer? #:value 2 "Double tap gesture"]
@defthing[GESTURE_HOLD exact-integer? #:value 4 "Hold gesture"]
@defthing[GESTURE_DRAG exact-integer? #:value 8 "Drag gesture"]
@defthing[GESTURE_SWIPE_RIGHT exact-integer? #:value 16 "Swipe right gesture"]
@defthing[GESTURE_SWIPE_LEFT exact-integer? #:value 32 "Swipe left gesture"]
@defthing[GESTURE_SWIPE_UP exact-integer? #:value 64 "Swipe up gesture"]
@defthing[GESTURE_SWIPE_DOWN exact-integer? #:value 128 "Swipe down gesture"]
@defthing[GESTURE_PINCH_IN exact-integer? #:value 256 "Pinch in gesture"]
@defthing[GESTURE_PINCH_OUT exact-integer? #:value 512 "Pinch out gesture"]

@section{Camera system modes}
@defthing[_CameraMode ctype?]{Camera system modes}
@defthing[CAMERA_CUSTOM exact-integer? #:value 0 "Custom camera"]
@defthing[CAMERA_FREE exact-integer? #:value 1 "Free camera"]
@defthing[CAMERA_ORBITAL exact-integer? #:value 2 "Orbital camera"]
@defthing[CAMERA_FIRST_PERSON exact-integer? #:value 3 "First person camera"]
@defthing[CAMERA_THIRD_PERSON exact-integer? #:value 4 "Third person camera"]

@section{Camera projection}
@defthing[_CameraProjection ctype?]{Camera projection}
@defthing[CAMERA_PERSPECTIVE exact-integer? #:value 0 "Perspective projection"]
@defthing[CAMERA_ORTHOGRAPHIC exact-integer? #:value 1 "Orthographic projection"]

@section{N-patch layout}
@defthing[_NPatchLayout ctype?]{N-patch layout}
@defthing[NPATCH_NINE_PATCH exact-integer? #:value 0 "Npatch layout: 3x3 tiles"]
@defthing[NPATCH_THREE_PATCH_VERTICAL exact-integer? #:value 1 "Npatch layout: 1x3 tiles"]
@defthing[NPATCH_THREE_PATCH_HORIZONTAL exact-integer? #:value 2 "Npatch layout: 3x1 tiles"]
