#lang at-exp raylib/codegen/config

(local-require racket/set
               racket/match
               racket/format
               racket/port
               (rename-in scribble/text [output scrbl-output]))

(define (all-modules fmt)
  (for/list ([path
              (in-list
               '("unsafe/functions"
                 "structs"
                 "enums"
                 "constants"))])
    (format fmt path)))

(define raylib-version
  (api-constant-value
   (car
    (include
     (name-matches "RAYLIB_VERSION")
     #:from (parsed 'constants)))))

(output
 #:to "unsafe.rkt"
 #:from
 (apply (template "./templates/root.rkt" 'generate-root)
        (all-modules "~a.rkt")))

(output
 #:to "scribblings/raylib-2d.scrbl"
 #:from
 (apply
  (template "./templates/root.scrbl" 'generate-root)
  #:title @list{Raylib 2D Bindings}
  #:top-desc @list{
  Unsafe bindings for @"@"hyperlink["https://www.raylib.com/"]{Raylib}'s
  2D components.

  These bindings are currently for Raylib @|raylib-version|.

  Most of these bindings are perfectly safe, as long as they are not horribly misused.
  They are called and marked "unsafe", since they are a thin wrapper over a C API,
  and thus undefined behaviour is possible if the bindings are used incorrectly.
  }
  #:module @list{raylib/2d/unsafe}
  #:module-desc @list{
  Reexports all of @"@"racket[raylib/2d/unsafe/functions] and @"@"racket[raylib/2d/*].
  }
  (all-modules "~a.scrbl")))

(define structs-module
  @list{raylib/2d/structs})

(for ([out-fmt (in-list '("~a.rkt" "scribblings/~a.scrbl"))]
      [in-fmt (in-list '("~a.rkt" "~a.scrbl"))])

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

  (define exclusion-set
    (list->set (append-map cdr exclusions)))

  (define included-functions
    (for/list ([func (in-list (parsed 'functions))]
               #:unless (set-member? exclusion-set (api-object-name func)))
      func))

  (define templated-functions
    ((template (format in-fmt "./templates/functions") 'generate-functions)
     #:module @list{raylib/2d/unsafe/functions}
     #:structs-module structs-module
     included-functions))

  (output
   #:to (format out-fmt "unsafe/functions")
   #:from
   (if (string-suffix? out-fmt ".rkt")
       templated-functions
       @list{
   @|templated-functions|
   @"@"section{Excluded Functions}
   This is a list of all the functions excluded from these bindings.

   These are documented here in case you are looking at one of the
   @"@"hyperlink["https://www.raylib.com/examples.html"]{Raylib examples}
   and come across a function that doesn't exist in these bindings.

   @"@"(require (for-label racket/gui/base))

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
                                       (for/list ([func (in-list functions)])
                                         @list{@"@"racket[@|func|]}))}
                            @"@"list{@|reason|})}))))
   ]
   @(void)}))

  ;; these are all perfectly safe
  (output
   #:to (format out-fmt "structs")
   #:from
   ((template (format in-fmt "./templates/structs") 'generate-structs)
    #:module structs-module
    (parsed 'structs) ;; TODO exclude a number of structs
    (parsed 'typedefs)
    (exclude
     #:from (parsed 'function-typedefs)
     (name-matches "TraceLogCallback"))))
  (output
   #:to (format out-fmt "constants")
   #:from
   ((template (format in-fmt "./templates/constants") 'generate-constants)
    #:module @list{raylib/2d/constants}
    #:structs-module structs-module
    (parsed 'constants)))
  (output
   #:to (format out-fmt "enums")
   #:from
   ((template (format in-fmt "./templates/enums") 'generate-enums)
    #:module @list{raylib/2d/enums}
    (parsed 'enums))))

(apply-patch
 #:from "./patches/2d.patch"
 "-p1")
