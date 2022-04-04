#lang scribble/manual

@(require (for-label raylib/generated/unsafe/structs ffi/unsafe racket/base))

@table-of-contents[]

@title{Structs}
@defmodule[raylib/generated/unsafe/structs]
@section{Struct types}

@deftogether[(@defthing[_Color ctype?]
              @defstruct[Color
                         ([r _ubyte]
                          [g _ubyte]
                          [b _ubyte]
                          [a _ubyte])
                         #:constructor-name make-Color])]{
Color, 4 components, R8G8B8A8 (32bit)
}

@section{Type aliases}
@deftogether[(@defthing[_Quaternion ctype? #:value _Vector4]
              @defthing[_Texture2D ctype? #:value _Texture]
              @defthing[_TextureCubemap ctype? #:value _Texture]
              @defthing[_RenderTexture2D ctype? #:value _RenderTexture]
              @defthing[_Camera ctype? #:value _Camera3D])]{
Aliases for some struct types.
}

@section{Callback function types}
@deftogether[(@defthing[_TraceLogCallback ctype?
                        #:value
                        (_fun
                         [logLevel : _int]
                         [text : _string]
                         [args : _byte #;"va_list"]
                         -> _void)]
              @defthing[_LoadFileDataCallback ctype?
                        #:value
                        (_fun
                         [fileName : _string]
                         [bytesRead : _pointer #;"unsigned int *"]
                         -> _pointer #;"unsigned char *")]
              @defthing[_SaveFileDataCallback ctype?
                        #:value
                        (_fun
                         [fileName : _string]
                         [data : _pointer #;"void *"]
                         [bytesToWrite : _uint]
                         -> _bool)]
              @defthing[_LoadFileTextCallback ctype?
                        #:value
                        (_fun
                         [fileName : _string]
                         -> _pointer #;"char *")]
              @defthing[_SaveFileTextCallback ctype?
                        #:value
                        (_fun
                         [fileName : _string]
                         [text : _pointer #;"char *"]
                         -> _bool)]
              @defthing[_AudioCallback ctype?
                        #:value
                        (_fun
                         [bufferData : _pointer #;"void *"]
                         [frames : _uint]
                         -> _void)])]{
Types for certain callback functions.
}
