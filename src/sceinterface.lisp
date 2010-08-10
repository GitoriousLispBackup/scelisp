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
(defctype sceshader :pointer)

;;; Textures
(defctype scetexture :pointer)

;;; Cameras
(defctype scecamera :pointer)

(defcfun "SCE_Camera_Create" scecamera)
(defcfun "SCE_Camera_Delete" :void
  (camera scecamera))

;;; Lights
(defctype scelight :pointer)

(defcfun "SCE_Light_Create" scelight)
(defcfun "SCE_Light_Delete" :void)

(defcfun "SCE_Light_SetColor" :void
  (light scelight)
  (r :float)
  (g :float)
  (b :float))

(defcfun "SCE_Light_Infinite" :void
  (light scelight)
  (inf scebool))

(defcfun "SCE_Light_GetNode" scenode
  (light scelight))

;;; Meshes
(defctype scemesh :pointer)

(defcfun "SCE_Mesh_Load" scemesh
  (fname :string)
  (force :int))
(defcfun "SCE_Mesh_AutoBuild" :void
  (mesh scemesh))

;;; Models
(defctype scemodel :pointer)

(defcfun "SCE_Model_Create" scemodel)
(defcfun "SCE_Model_Delete" :void
  (model scemodel))

(defcfun "SCE_Model_AddEntity" :int
  (model scemodel)
  (level :int)
  (mesh scemesh)
  (shader sceshader)
  (rest :pointer))                      ; TODO: handle "..." args

(defcfun "SCE_Model_AddNewInstance" :int
  (model scemodel)
  (n :unsigned-int)
  (root scebool)
  (mat :pointer))

(defcfun "SCE_Model_MergeInstances" :int
  (model scemodel))

;;; Scenes
(defctype scescene :pointer)

(defcfun "SCE_Scene_Create" scescene)
(defcfun "SCE_Scene_Delete" :void
  (scene scescene))

(defcfun "SCE_Scene_AddCamera" :void
  (scene scescene)
  (camera scecamera))
(defcfun "SCE_Scene_AddLight" :void
  (scene scescene)
  (light scelight))
(defcfun "SCE_Scene_AddModel" :void
  (scene scescene)
  (model scemodel))

(defcfun "SCE_Scene_Update" :void
  (scene scescene)
  (camera scecamera)
  (rendertarget scetexture)
  (cubeface :unsigned-int))
(defcfun "SCE_Scene_Render" :void
  (scene scescene)
  (camera scecamera)
  (rendertarget scetexture)
  (cubeface :unsigned-int))
