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

(defmacro with-interface (&body body)
  `(unwind-protect
        (progn 
          (sce-init-interface 0 0)      ; TODO: use stdout
          ,@body)
     (sce-quit-interface)))

;;; Shader
(defobject shader)

;;; Texture
(defobject texture)

;;; Material
(defobject material)

(def-sce-method material "Init" :void)

(defsetter material "SetColor"
  (type sceenum)
  (r :float)
  (g :float)
  (b :float)
  (a :float))
(defsetter material "SetColorv"
  (type sceenum)
  (color scevector))

(def-sce-method material "GetColor" scevector
  (type sceenum))
;; GetColorv is useless here

(defsetter material "ActivatePointSprite"
  (activated scebool))
(defsetter material "EnablePointSprite")
(defsetter material "DisablePointSprite")
(defsetter material "ActivateBlending"
  (activated scebool))
(defsetter material "EnableBlending")
(defsetter material "DisableBlending")
(defsetter material "SetBlending"
  (src sceenum)
  (dst sceenum))

(defsetter material "Use")

;;; Light
(defobject light)

(defstatus light "Activate")

(def-sce-method light "GetNode" scenode)

(defsetter light "SetColor"
  (r :float)
  (g :float)
  (b :float))
(eval-when (:compile-toplevel :load-toplevel)
  (addprop 'light "Color"))

(defprop light "Colorv" scevector)
(def-sce-method light "GetColor" scevector)
(def-sce-method light "GetPositionv" scevector)
(def-sce-method light "GetDirectionv" scevector)

(defstatus light "Infinite")
(defprop light "Angle" :float)
(defprop light "Intensity" :float)
(defprop light "Radius" :float)

(defcfun "SCE_Light_ActivateLighting" :void
  (activated scebool))
(defsetter light "Use")

(defconstructor light)
;;; Meshes
(defobject mesh)

(defcfun "SCE_Mesh_Load" scemesh
  (fname :string)
  (force :int))

(defsetter mesh "AutoBuild")

(defmacro with-mesh ((name path) &body body)
  `(let ((,name (sce-mesh-load ,path 2)))
     ;; Disabled because it throws an error even when the mesh is loaded
     #|(when (sce-error-haveerror)
       (sce-error-out)
       (error "Can't load mesh"))|#
     (sce-mesh-autobuild ,name)         ; TODO: has to be here?
     ,@body))

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

(defconstructor model)

;;; SceneEntity
(defobject (sceneentity "SceneEntity"))

(def-sce-method sceneentity "Init" :void)
(def-sce-method sceneentity "RemoveEntity" :void)
(def-sce-method sceneentity "AddTexture" :int
  (tex scetexture))
(def-sce-method sceneentity "RemoveTexture" :void
  (tex scetexture))

(defprop sceneentity "Mesh" scemesh)
(defprop sceneentity "Shader" sceshader)
(defprop sceneentity "Material" scematerial)

(defsetter sceneentity "SetupBoundingVolume"
  (volume :int))

(def-sce-method sceneentity "HasResourceOfType" scebool
  (type :int))
(def-sce-method sceneentity "HasInstance" scebool)

(defsetter sceneentity "ApplyProperties")
(defsetter sceneentity "UseRessources")
(defsetter sceneentity "Render")

(defconstructor sceneentity)

;;; Scene
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

(defsetter scene "ClearBuffers")

(defconstructor scene)