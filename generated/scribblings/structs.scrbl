#lang scribble/manual

@(require (for-label raylib/generated/structs raylib/support ffi/unsafe racket/base))

@title{Structs}

@table-of-contents[]

@defmodule[raylib/generated/structs]
@section{Struct types}

@deftogether[(@defthing[_Vector2 ctype?]
              @defstruct[Vector2
                         ([x _float]
                          [y _float])
                         #:constructor-name make-Vector2])]{
Vector2, 2 components
}

@deftogether[(@defthing[_Vector3 ctype?]
              @defstruct[Vector3
                         ([x _float]
                          [y _float]
                          [z _float])
                         #:constructor-name make-Vector3])]{
Vector3, 3 components
}

@deftogether[(@defthing[_Vector4 ctype?]
              @defstruct[Vector4
                         ([x _float]
                          [y _float]
                          [z _float]
                          [w _float])
                         #:constructor-name make-Vector4])]{
Vector4, 4 components
}

@deftogether[(@defthing[_Matrix ctype?]
              @defstruct[Matrix
                         ([m0 _float] [m4 _float] [m8 _float] [m12 _float]
                          [m1 _float] [m5 _float] [m9 _float] [m13 _float]
                          [m2 _float] [m6 _float] [m10 _float] [m14 _float]
                          [m3 _float] [m7 _float] [m11 _float] [m15 _float])
                         #:constructor-name make-Matrix])]{
Matrix, 4x4 components, column major, OpenGL style, right handed
}

@deftogether[(@defthing[_Color ctype?]
              @defstruct[Color
                         ([r _ubyte]
                          [g _ubyte]
                          [b _ubyte]
                          [a _ubyte])
                         #:constructor-name make-Color])]{
Color, 4 components, R8G8B8A8 (32bit)
}

@deftogether[(@defthing[_Rectangle ctype?]
              @defstruct[Rectangle
                         ([x _float]
                          [y _float]
                          [width _float]
                          [height _float])
                         #:constructor-name make-Rectangle])]{
Rectangle, 4 components
}

@deftogether[(@defthing[_Image ctype?]
              @defstruct[Image
                         ([data (_pointer-to _void)]
                          [width _int]
                          [height _int]
                          [mipmaps _int]
                          [format _int])
                         #:constructor-name make-Image])]{
Image, pixel data stored in CPU memory (RAM)
}

@deftogether[(@defthing[_Texture ctype?]
              @defstruct[Texture
                         ([id _uint]
                          [width _int]
                          [height _int]
                          [mipmaps _int]
                          [format _int])
                         #:constructor-name make-Texture])]{
Texture, tex data stored in GPU memory (VRAM)
}

@deftogether[(@defthing[_RenderTexture ctype?]
              @defstruct[RenderTexture
                         ([id _uint]
                          [texture _Texture]
                          [depth _Texture])
                         #:constructor-name make-RenderTexture])]{
RenderTexture, fbo for texture rendering
}

@deftogether[(@defthing[_NPatchInfo ctype?]
              @defstruct[NPatchInfo
                         ([source _Rectangle]
                          [left _int]
                          [top _int]
                          [right _int]
                          [bottom _int]
                          [layout _int])
                         #:constructor-name make-NPatchInfo])]{
NPatchInfo, n-patch layout info
}

@deftogether[(@defthing[_GlyphInfo ctype?]
              @defstruct[GlyphInfo
                         ([value _int]
                          [offsetX _int]
                          [offsetY _int]
                          [advanceX _int]
                          [image _Image])
                         #:constructor-name make-GlyphInfo])]{
GlyphInfo, font characters glyphs info
}

@deftogether[(@defthing[_Font ctype?]
              @defstruct[Font
                         ([baseSize _int]
                          [glyphCount _int]
                          [glyphPadding _int]
                          [texture _Texture2D]
                          [recs (_pointer-to _Rectangle)]
                          [glyphs (_pointer-to _GlyphInfo)])
                         #:constructor-name make-Font])]{
Font, font texture and GlyphInfo array data
}

@deftogether[(@defthing[_Camera3D ctype?]
              @defstruct[Camera3D
                         ([position _Vector3]
                          [target _Vector3]
                          [up _Vector3]
                          [fovy _float]
                          [projection _int])
                         #:constructor-name make-Camera3D])]{
Camera, defines position/orientation in 3d space
}

@deftogether[(@defthing[_Camera2D ctype?]
              @defstruct[Camera2D
                         ([offset _Vector2]
                          [target _Vector2]
                          [rotation _float]
                          [zoom _float])
                         #:constructor-name make-Camera2D])]{
Camera2D, defines position/orientation in 2d space
}

@deftogether[(@defthing[_Mesh ctype?]
              @defstruct[Mesh
                         ([vertexCount _int]
                          [triangleCount _int]
                          [vertices (_pointer-to _float)]
                          [texcoords (_pointer-to _float)]
                          [texcoords2 (_pointer-to _float)]
                          [normals (_pointer-to _float)]
                          [tangents (_pointer-to _float)]
                          [colors (_pointer-to _ubyte)]
                          [indices (_pointer-to _ushort)]
                          [animVertices (_pointer-to _float)]
                          [animNormals (_pointer-to _float)]
                          [boneIds (_pointer-to _ubyte)]
                          [boneWeights (_pointer-to _float)]
                          [vaoId _uint]
                          [vboId (_pointer-to _uint)])
                         #:constructor-name make-Mesh])]{
Mesh, vertex data and vao/vbo
}

@deftogether[(@defthing[_Shader ctype?]
              @defstruct[Shader
                         ([id _uint]
                          [locs (_pointer-to _int)])
                         #:constructor-name make-Shader])]{
Shader
}

@deftogether[(@defthing[_MaterialMap ctype?]
              @defstruct[MaterialMap
                         ([texture _Texture2D]
                          [color _Color]
                          [value _float])
                         #:constructor-name make-MaterialMap])]{
MaterialMap
}

@deftogether[(@defthing[_Material ctype?]
              @defstruct[Material
                         ([shader _Shader]
                          [maps (_pointer-to _MaterialMap)]
                          [params (_array _float 4)])
                         #:constructor-name make-Material])]{
Material, includes shader and maps
}

@deftogether[(@defthing[_Transform ctype?]
              @defstruct[Transform
                         ([translation _Vector3]
                          [rotation _Quaternion]
                          [scale _Vector3])
                         #:constructor-name make-Transform])]{
Transform, vectex transformation data
}

@deftogether[(@defthing[_BoneInfo ctype?]
              @defstruct[BoneInfo
                         ([name (_array _byte 32)]
                          [parent _int])
                         #:constructor-name make-BoneInfo])]{
Bone, skeletal animation bone
}

@deftogether[(@defthing[_Model ctype?]
              @defstruct[Model
                         ([transform _Matrix]
                          [meshCount _int]
                          [materialCount _int]
                          [meshes (_pointer-to _Mesh)]
                          [materials (_pointer-to _Material)]
                          [meshMaterial (_pointer-to _int)]
                          [boneCount _int]
                          [bones (_pointer-to _BoneInfo)]
                          [bindPose (_pointer-to _Transform)])
                         #:constructor-name make-Model])]{
Model, meshes, materials and animation data
}

@deftogether[(@defthing[_ModelAnimation ctype?]
              @defstruct[ModelAnimation
                         ([boneCount _int]
                          [frameCount _int]
                          [bones (_pointer-to _BoneInfo)]
                          [framePoses (_pointer-to (_pointer-to _Transform))])
                         #:constructor-name make-ModelAnimation])]{
ModelAnimation
}

@deftogether[(@defthing[_Ray ctype?]
              @defstruct[Ray
                         ([position _Vector3]
                          [direction _Vector3])
                         #:constructor-name make-Ray])]{
Ray, ray for raycasting
}

@deftogether[(@defthing[_RayCollision ctype?]
              @defstruct[RayCollision
                         ([hit _stdbool]
                          [distance _float]
                          [point _Vector3]
                          [normal _Vector3])
                         #:constructor-name make-RayCollision])]{
RayCollision, ray hit information
}

@deftogether[(@defthing[_BoundingBox ctype?]
              @defstruct[BoundingBox
                         ([min _Vector3]
                          [max _Vector3])
                         #:constructor-name make-BoundingBox])]{
BoundingBox
}

@deftogether[(@defthing[_Wave ctype?]
              @defstruct[Wave
                         ([frameCount _uint]
                          [sampleRate _uint]
                          [sampleSize _uint]
                          [channels _uint]
                          [data (_pointer-to _void)])
                         #:constructor-name make-Wave])]{
Wave, audio wave data
}

@deftogether[(@defthing[_AudioStream ctype?]
              @defstruct[AudioStream
                         ([buffer (_pointer-to _rAudioBuffer)]
                          [sampleRate _uint]
                          [sampleSize _uint]
                          [channels _uint])
                         #:constructor-name make-AudioStream])]{
AudioStream, custom audio stream
}

@deftogether[(@defthing[_Sound ctype?]
              @defstruct[Sound
                         ([stream _AudioStream]
                          [frameCount _uint])
                         #:constructor-name make-Sound])]{
Sound
}

@deftogether[(@defthing[_Music ctype?]
              @defstruct[Music
                         ([stream _AudioStream]
                          [frameCount _uint]
                          [looping _stdbool]
                          [ctxType _int]
                          [ctxData (_pointer-to _void)])
                         #:constructor-name make-Music])]{
Music, audio stream, anything longer than ~10 seconds should be streamed
}

@deftogether[(@defthing[_VrDeviceInfo ctype?]
              @defstruct[VrDeviceInfo
                         ([hResolution _int]
                          [vResolution _int]
                          [hScreenSize _float]
                          [vScreenSize _float]
                          [vScreenCenter _float]
                          [eyeToScreenDistance _float]
                          [lensSeparationDistance _float]
                          [interpupillaryDistance _float]
                          [lensDistortionValues (_array _float 4)]
                          [chromaAbCorrection (_array _float 4)])
                         #:constructor-name make-VrDeviceInfo])]{
VrDeviceInfo, Head-Mounted-Display device parameters
}

@deftogether[(@defthing[_VrStereoConfig ctype?]
              @defstruct[VrStereoConfig
                         ([projection (_array _Matrix 2)]
                          [viewOffset (_array _Matrix 2)]
                          [leftLensCenter (_array _float 2)]
                          [rightLensCenter (_array _float 2)]
                          [leftScreenCenter (_array _float 2)]
                          [rightScreenCenter (_array _float 2)]
                          [scale (_array _float 2)]
                          [scaleIn (_array _float 2)])
                         #:constructor-name make-VrStereoConfig])]{
VrStereoConfig, VR stereo rendering configuration for simulator
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
                         [bytesRead : (_pointer-to _uint)]
                         -> (_pointer-to _ubyte))]
              @defthing[_SaveFileDataCallback ctype?
                        #:value
                        (_fun
                         [fileName : _string]
                         [data : (_pointer-to _void)]
                         [bytesToWrite : _uint]
                         -> _stdbool)]
              @defthing[_LoadFileTextCallback ctype?
                        #:value
                        (_fun
                         [fileName : _string]
                         -> (_pointer-to _byte))]
              @defthing[_SaveFileTextCallback ctype?
                        #:value
                        (_fun
                         [fileName : _string]
                         [text : (_pointer-to _byte)]
                         -> _stdbool)])]{
Types for certain callback functions.
}
