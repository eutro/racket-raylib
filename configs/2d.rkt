#lang at-exp raylib/codegen/config

(local-require racket/set
               racket/match
               racket/format
               racket/port
               (rename-in scribble/text [output scrbl-output]))

(define raylib-version
  (api-constant-value
   (car
    (include
     (name-matches "RAYLIB_VERSION")
     #:from (parsed 'constants)))))

(output
 #:to "scribblings/raylib-2d.scrbl"
 #:from
 @list{
 #lang scribble/manual

 @"@"title{Raylib 2D Bindings}

 @"@"defmodule[raylib/2d/unsafe]

 Unsafe bindings for Raylib's 2D components.

 They are called and marked unsafe, since they can cause undefined behaviour
 if misused, as they are a thin wrapper over a C API.

 This module re-exports all of
 @"@"racketmodname[raylib/2d/unsafe/functions],
 @"@"racketmodname[raylib/2d/structs],
 @"@"racketmodname[raylib/2d/enums] and
 @"@"racketmodname[raylib/2d/constants].

 @"@"table-of-contents[]

 @"@"include-section["unsafe/functions.scrbl"]

 @(splice
   (add-newlines
    (for/list ([t+m (in-list
                     '(("Structs" . "raylib/~a/structs")
                       ("Enums" . "raylib/~a/enums")
                       ("Constants" . "raylib/~a/constants")))])
      (define title (car t+m))
      (define mod (cdr t+m))
      @list{
      @"@"section{2D @|title|}
      @"@"defmodule[@(format mod "2d")]
      Re-exports @"@"racketmodname[@(format mod "generated")].
      @(void)})))})

(define (exclusion
         #:reason reason
         . funcs)
  (cons
   reason
   funcs))

(define exclusions
  (list
   (exclusion
    #:reason @list{@"@"racket[clipboard<%>] should be used instead}
    "SetClipboardText"
    "GetClipboardText")

   #;; There's open-url in DrRacket but I'm against suggesting it
   (exclusion
    #:reason @list{@"@"racket[open-url] should be used instead}
    "OpenURL")

   (exclusion
    #:reason @list{@"@"racket[sleep] should be used instead}
    "WaitTime")

   (exclusion
    #:reason @list{@"@"racket[random] should be used instead}
    "GetRandomValue"
    "SetRandomSeed")

   (exclusion
    #:reason @list{
    @"@"racket[malloc], @"@"racket[free] and other Racket pointer
    conversion functions should be used instead
    }
    "MemAlloc"
    "MemRealloc"
    "MemFree")

   (exclusion
    #:reason @list{
    @"@"seclink["zip" #:doc '(lib "file/scribblings/file.scrbl")]{@"@"racket[file/zip]}
    and
    @"@"seclink["base64" #:doc '(lib "net/scribblings/net.scrbl")]{@"@"racket[net/base64]}
    (or alternatives) should be used instead
    }
    "CompressData"
    "DecompressData"
    "EncodeDataBase64"
    "DecodeDataBase64")

   (exclusion
    #:reason @list{Racket's own IO functions should be used instead}
    "LoadFileData"
    "LoadFileText"
    "UnloadFileData"
    "UnloadFileText"
    "SaveFileData"
    "SaveFileText"

    "FileExists"
    "DirectoryExists"
    "IsFileExtension"
    "GetFileLength"
    "GetFileExtension"
    "GetFileName"
    "GetFileNameWithoutExt"
    "GetDirectoryPath"
    "GetPrevDirectoryPath"
    "GetWorkingDirectory"
    "GetApplicationDirectory"
    "GetDirectoryFiles"
    "ClearDirectoryFiles"
    "ChangeDirectory"
    "GetFileModTime")

   (exclusion
    #:reason @list{Racket's own string functions should be used instead}
    "GetCodepointCount"
    "GetCodepoint"
    "CodepointToUTF8"
    "TextCodepointsToUTF8"
    "TextCopy"
    "TextIsEqual"
    "TextLength"
    "TextFormat"
    "TextSubtext"
    "TextReplace"
    "TextInsert"
    "TextJoin"
    "TextSplit"
    "TextAppend"
    "TextFindIndex"
    "TextToUpper"
    "TextToLower"
    "TextToPascal"
    "TextToInteger")

   (exclusion
    #:reason @list{
    this takes a varargs function pointer, which is
    impossible to produce with pure Racket bindings
    }
    "SetTraceLogCallback")

   (exclusion
    #:reason @list{these are not applicable to 2D rendering}
    "BeginVrStereoMode"
    "BeginMode3D"
    "EndVrStereoMode"
    "EndMode3D"
    "LoadVrStereoConfig"
    "UnloadVrStereoConfig"

    "DrawLine3D"
    "DrawPoint3D"
    "DrawCircle3D"
    "DrawTriangle3D"
    "DrawTriangleStrip3D"
    "DrawCube"
    "DrawCubeV"
    "DrawCubeWires"
    "DrawCubeWiresV"
    "DrawCubeTexture"
    "DrawCubeTextureRec"
    "DrawSphere"
    "DrawSphereEx"
    "DrawSphereWires"
    "DrawCylinder"
    "DrawCylinderEx"
    "DrawCylinderWires"
    "DrawCylinderWiresEx"
    "DrawPlane"
    "DrawRay"
    "DrawGrid"
    "LoadModel"
    "LoadModelFromMesh"
    "UnloadModel"
    "UnloadModelKeepMeshes"
    "GetModelBoundingBox"
    "DrawModel"
    "DrawModelEx"
    "DrawModelWires"
    "DrawModelWiresEx"
    "DrawBoundingBox"
    "DrawBillboard"
    "DrawBillboardRec"
    "DrawBillboardPro"
    "UploadMesh"
    "UpdateMeshBuffer"
    "UnloadMesh"
    "DrawMesh"
    "DrawMeshInstanced"
    "ExportMesh"
    "GetMeshBoundingBox"
    "GenMeshTangents"
    "GenMeshBinormals"
    "GenMeshPoly"
    "GenMeshPlane"
    "GenMeshCube"
    "GenMeshSphere"
    "GenMeshHemiSphere"
    "GenMeshCylinder"
    "GenMeshCone"
    "GenMeshTorus"
    "GenMeshKnot"
    "GenMeshHeightmap"
    "GenMeshCubicmap"
    "LoadMaterials"
    "LoadMaterialDefault"
    "UnloadMaterial"
    "SetMaterialTexture"
    "SetModelMeshMaterial"
    "LoadModelAnimations"
    "UpdateModelAnimation"
    "UnloadModelAnimation"
    "UnloadModelAnimations"
    "IsModelAnimationValid"
    "CheckCollisionSpheres"
    "CheckCollisionBoxes"
    "CheckCollisionBoxSphere"
    "GetRayCollisionSphere"
    "GetRayCollisionBox"
    "GetRayCollisionMesh"
    "GetRayCollisionTriangle"
    "GetRayCollisionQuad")))

(define existing-functions
  (for/set ([obj (in-list (parsed 'functions))])
    (api-object-name obj)))

(define exclusion-list
  (filter (λ (x) (set-member? existing-functions x))
          (append-map cdr exclusions)))

(define generate-reexport
  (template "./templates/reexport.rkt" 'generate-reexport))

(output
 #:to "unsafe.rkt"
 #:from
 (generate-reexport
  (list @list{raylib/2d/unsafe/functions})
  (list @list{raylib/2d/structs})
  (list @list{raylib/2d/constants})
  (list @list{raylib/2d/enums})))

(output
 #:to "unsafe/functions.rkt"
 #:from
 (generate-reexport
  (cons @list{raylib/generated/unsafe/functions} exclusion-list)
  (list @list{raylib/derived/unsafe})))

(for ([base (in-list '("structs" "enums" "constants"))])
  (output
   #:to (format "~a.rkt" base)
   #:from
   (generate-reexport
    (list (format "raylib/generated/~a" base)))))

(output
 #:to "scribblings/unsafe/functions.scrbl"
 #:from
 @list{
 #lang scribble/manual

 @"@"(require (for-label raylib/generated/unsafe/functions
                         racket/gui/base
                         racket/base
                         ffi/unsafe))

 @"@"title{2D Functions}

 @"@"defmodule[raylib/2d/unsafe/functions]

 @"@"(define functions-ref
       @"@"racketmodname[raylib/generated/unsafe/functions])

 This module re-exports @"@"racketmodname[raylib/derived/unsafe] and
 most of @"@"|functions-ref|. Specifically, @"@"racketmodname[raylib/2d/unsafe]
 re-exports everything that is useful for 2D rendering with Raylib.

 @"@"section{Excluded Functions}

 This is a list of all the functions excluded from @"@"|functions-ref|.

 These are documented here in case you are looking at one of the
 @"@"hyperlink["https://www.raylib.com/examples.html"]{Raylib examples}
 and come across a function that doesn't exist in these bindings.

 @"@"tabular[
   #:style 'boxed
   #:row-properties '(bottom-border)
   (list (list @"@"bold{Function(s) Excluded}
               @"@"bold{Reason})
         @(block
           (add-newlines
            (for/list ([excl (in-list exclusions)])
              (match-define (cons reason functions) excl)
              @list{(list @"@"list{@(add-newlines
                                     #:sep ", "
                                     (for/list ([func (in-list functions)]
                                                #:when (set-member? existing-functions func))
                                       @list{@"@"racket[@|func|]}))}
                          @"@"list{@|reason|})}))))
 ]
 @(void)}
 )
