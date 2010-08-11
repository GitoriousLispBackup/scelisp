(in-package :scelisp)

(define-foreign-library libsceinterface
  (:unix (:or "libsceinterface.so.0" "libsceinterface.so"))
  (t (:default "libsceinterface")))
(use-foreign-library libsceinterface)

;;; Interface
(defcfun "SCE_Init_Interface" :int
  (outlog :int)
  (flags scebitfield))

(defcfun "SCE_Quit_Interface" :void)

;;; Shaders
(defobject shader)

;;; Textures
(defobject texture)

;;; Cameras
(defobject camera)

;;; Lights
(defobject light)

(defsetter light "Activate"
  (activated scebool))
(def-sce-method light "IsActivated" scebool)

;; TODO GetIterator
(def-sce-method light "GetNode" scenode)

(defsetter light "SetColor"
  (r :float)
  (g :float)
  (b :float))
;; TODO SetColorv
;; TODO GetColor
;; TODO GetColorv
;; TODO GetPositionv
;; TODO GetDirectionv

(defsetter light "Infinite"
  (inf scebool))
(def-sce-method light "IsInfinite" scebool)

(defsetter light "SetAngle"
  (angle :float))
(def-sce-method light "GetAngle" :float)

(defsetter light "SetIntensity"
  (intensity :float))
(def-sce-method light "GetIntensity" :float)

(defsetter light "SetRadius"
  (radius :float))
(defgetter light "GetRadius" :float)

(defcfun "SCE_Light_ActivateLighting" :void
  (activated scebool))

(defsetter light "Use")

;;; Meshes
(defobject mesh)

(defcfun "SCE_Mesh_Load" scemesh
  (fname :string)
  (force :int))

(defsetter mesh "AutoBuild")

;;; Models
(defobject model)

(def-sce-method model "AddEntity" :int
  (level :int)
  (mesh scemesh)
  (shader sceshader)
  (rest :pointer))                      ; TODO: handle "..." args

(def-sce-method model "AddNewInstance" :int
  (n :unsigned-int)
  (root scebool)
  (mat :pointer))

(def-sce-method model "MergeInstances" :int)
(def-sce-method model "GetRootNode" scenode)

;;; Scenes
(defobject scene)

(defsetter scene "AddCamera"
  (camera scecamera))
(defsetter scene "AddLight"
  (light scelight))
(defsetter scene "AddModel"
  (model scemodel))

(defsetter scene "Update"
  (camera scecamera)
  (rendertarget scetexture)
  (cubeface :unsigned-int))
(defsetter scene "Render"
  (camera scecamera)
  (rendertarget scetexture)
  (cubeface :unsigned-int))
