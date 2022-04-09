#lang scribble/manual

@(require (for-label raylib/generated/unsafe/functions
                        racket/gui/base
                        racket/base))

@title{2D Functions}

@defmodule[raylib/2d/unsafe/functions]

@(define functions-ref
      @racketmodname[raylib/generated/unsafe/functions])

This module re-exports @racketmodname[raylib/derived/unsafe] and
most of @|functions-ref|. Specifically, @racketmodname[raylib/2d/unsafe]
re-exports everything that is useful for 2D rendering with Raylib.

@section{Excluded Functions}

This is a list of all the functions excluded from @|functions-ref|.

These are documented here in case you are looking at one of the
@hyperlink["https://www.raylib.com/examples.html"]{Raylib examples}
and come across a function that doesn't exist in these bindings.

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
        (list @list{@racket[LoadFileData], @racket[LoadFileText], @racket[UnloadFileData], @racket[UnloadFileText], @racket[SaveFileData], @racket[SaveFileText], @racket[FileExists], @racket[DirectoryExists], @racket[IsFileExtension], @racket[GetFileExtension], @racket[GetFileName], @racket[GetFileNameWithoutExt], @racket[GetDirectoryPath], @racket[GetPrevDirectoryPath], @racket[GetWorkingDirectory], @racket[GetDirectoryFiles], @racket[ClearDirectoryFiles], @racket[ChangeDirectory], @racket[GetFileModTime]}
              @list{Racket's own IO functions should be used instead})
        (list @list{@racket[GetCodepointCount], @racket[GetCodepoint], @racket[CodepointToUTF8], @racket[TextCodepointsToUTF8], @racket[TextCopy], @racket[TextIsEqual], @racket[TextLength], @racket[TextFormat], @racket[TextSubtext], @racket[TextReplace], @racket[TextInsert], @racket[TextJoin], @racket[TextSplit], @racket[TextAppend], @racket[TextFindIndex], @racket[TextToUpper], @racket[TextToLower], @racket[TextToPascal], @racket[TextToInteger]}
              @list{Racket's own string functions should be used instead})
        (list @list{@racket[SetTraceLogCallback]}
              @list{this takes a varargs function pointer, which is
                    impossible to produce with pure Racket bindings})
        (list @list{@racket[BeginVrStereoMode], @racket[BeginMode3D], @racket[EndVrStereoMode], @racket[EndMode3D], @racket[LoadVrStereoConfig], @racket[UnloadVrStereoConfig], @racket[DrawLine3D], @racket[DrawPoint3D], @racket[DrawCircle3D], @racket[DrawTriangle3D], @racket[DrawTriangleStrip3D], @racket[DrawCube], @racket[DrawCubeV], @racket[DrawCubeWires], @racket[DrawCubeWiresV], @racket[DrawCubeTexture], @racket[DrawCubeTextureRec], @racket[DrawSphere], @racket[DrawSphereEx], @racket[DrawSphereWires], @racket[DrawCylinder], @racket[DrawCylinderEx], @racket[DrawCylinderWires], @racket[DrawCylinderWiresEx], @racket[DrawPlane], @racket[DrawRay], @racket[DrawGrid], @racket[LoadModel], @racket[LoadModelFromMesh], @racket[UnloadModel], @racket[UnloadModelKeepMeshes], @racket[GetModelBoundingBox], @racket[DrawModel], @racket[DrawModelEx], @racket[DrawModelWires], @racket[DrawModelWiresEx], @racket[DrawBoundingBox], @racket[DrawBillboard], @racket[DrawBillboardRec], @racket[DrawBillboardPro], @racket[UploadMesh], @racket[UpdateMeshBuffer], @racket[UnloadMesh], @racket[DrawMesh], @racket[DrawMeshInstanced], @racket[ExportMesh], @racket[GetMeshBoundingBox], @racket[GenMeshTangents], @racket[GenMeshBinormals], @racket[GenMeshPoly], @racket[GenMeshPlane], @racket[GenMeshCube], @racket[GenMeshSphere], @racket[GenMeshHemiSphere], @racket[GenMeshCylinder], @racket[GenMeshCone], @racket[GenMeshTorus], @racket[GenMeshKnot], @racket[GenMeshHeightmap], @racket[GenMeshCubicmap], @racket[LoadMaterials], @racket[LoadMaterialDefault], @racket[UnloadMaterial], @racket[SetMaterialTexture], @racket[SetModelMeshMaterial], @racket[LoadModelAnimations], @racket[UpdateModelAnimation], @racket[UnloadModelAnimation], @racket[UnloadModelAnimations], @racket[IsModelAnimationValid], @racket[CheckCollisionSpheres], @racket[CheckCollisionBoxes], @racket[CheckCollisionBoxSphere], @racket[GetRayCollisionSphere], @racket[GetRayCollisionBox], @racket[GetRayCollisionMesh], @racket[GetRayCollisionTriangle], @racket[GetRayCollisionQuad]}
              @list{these are not applicable to 2D rendering}))
]
