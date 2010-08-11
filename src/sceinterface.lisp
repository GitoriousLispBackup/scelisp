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

(defsetter light "SetColor"
  (r :float)
  (g :float)
  (b :float))

(defsetter light "Infinite"
  (inf scebool))

(def-sce-method light "GetNode" scenode)

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
