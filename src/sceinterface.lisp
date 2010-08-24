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
;; TODO: SCE_Texture_Create isn't "standard" (two params)
(defobject texture)
;(deflist-type scetexture)

(defsetter texture "Init")
(defsetter texture "SetupParameters")
(defsetter texture "MakeRender"
  (type :int))
(defsetter texture "MakeRenderCube")

(defsetter texture "SetFilter"
  (filter :int))
(defsetter texture "Pixelize"
  (p scebool))
(defsetter texture "SetParam"
  (pname sceenum)
  (param :int))
(defsetter texture "SetParamf"
  (pname sceenum)
  (param :float))
(defsetter texture "SetUnit"
  (unit :unsigned-int))

(def-sce-method texture "GetUnit" :unsigned-int)
(def-sce-method texture "GetMatrix" scematrix4)
(def-sce-method texture "GetType" :int)
(def-sce-method texture "GetWidth" :int
  (target :int)
  (level :int))
(def-sce-method texture "GetHeight" :int
  (target :int)
  (level :int))

;; TODO: handle SCE_ERROR and SCE_OK (and throw an error)
(def-sce-method texture "Build" :int
  (use-mipmap scebool))
(def-sce-method texture "Update" :int)

(defcfun "SCE_Texture_Loadv" scetexture
  (type :int)
  (w :int)
  (h :int)
  (d :int)
  (force :int)
  (names stringlist))

(defcfun "SCE_Texture_Load" scetexture
  (type :int)
  (w :int)
  (h :int)
  (d :int)
  (force :int)
  &rest)

(def-sce-method texture "AddRenderTexture" :int
  (id :int)
  (new scetexture))

(defcfun "SCE_Texture_Blit" :void
  (rdst sceintrect)
  (dst scetexture)
  (rsrc sceintrect)
  (src scetexture))
(defcfun "SCE_Texture_Blitf" :void
  (rdst scefloatrect)
  (dst scetexture)
  (rsrc scefloatrect)
  (src scetexture))

(defcfun "SCE_Texture_RenderQuad" :void
  (r scefloatrect))

(defsetter texture "Set")
(defsetter texture "Use")
(defsetter texture "RenderTo"
  (cubeface :unsigned-int))

(defcfun "SCE_Texture_Flush" :void)
(defcfun "SCE_Texture_BeginLot" :void)
(defcfun "SCE_Texture_EndLot" :void)

;;; Material
(defobject material)

(def-sce-method material "Init" :void)

(eval-when (:compile-toplevel :load-toplevel)
  (addprop 'material "Color"))

(defcenum scecolortype
  :diffuse :emissive :specular :ambiant :shineness)

(defsetter material "SetColor"
  (type scecolortype)
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

(defconstructor material)
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

;;; Mesh
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

;;; Model
(defobject model)

(def-sce-method model "AddEntityv" :int
  (level :int)
  (mesh scemesh)
  (shader sceshader)
  (textures :pointer))
(def-sce-method model "AddEntity" :int
  (level :int)
  (mesh scemesh)
  (shader sceshader)
  &rest)

(def-sce-method model "AddNewInstance" :int
  (n :unsigned-int)
  (root scebool)
  (mat :pointer))

(def-sce-method model "MergeInstances" :int)
(def-sce-method model "GetRootNode" scenode)

(defconstructor model)

;;; SceneEntity & co
(defobject (sceneentity "SceneEntity"))
(defctype scesceneentityinstance :pointer)
(defctype scesceneentityproperties :pointer)
(defctype scesceneentitygroup :pointer)

;; TODO: ugly, but SCE's interface for those types is weird
(eval-when (:compile-toplevel :load-toplevel)
  (setf (gethash 'sceneentityinstance *types*) "SceneEntity"
        (gethash 'sceneentityproperties *types*) "SceneEntity"
        (gethash 'sceneentitygroup *types*) "SceneEntity"))

;;; SceneEntityInstance
(defsetter sceneentityinstance "InitInstance")
(defcfun "SCE_SceneEntity_CreateInstance" scesceneentityinstance)
(defsetter sceneentityinstance "DeleteInstance")

(def-sce-method sceneentityinstance "DupInstance" scesceneentityinstance)
(defsetter sceneentityinstance "ReplaceInstanceToEntity")
(defsetter sceneentityinstance "RemoveInstanceFromEntity")

(def-sce-method sceneentityinstance "GetInstanceNode" scenode)
(def-sce-method sceneentityinstance "GetInstanceNodeType" scenodetype)
;; TODO: GetInstanceInstance, GetInstanceElement, GetInstanceLOD
(def-sce-method sceneentityinstance "GetInstanceGroup" scesceneentitygroup)

(defprop sceneentityinstance "InstanceData" :pointer)

(def-sce-method sceneentityinstance "IsBBInFrustum" scebool
  (cam scecamera))
(def-sce-method sceneentityinstance "IsBSInFrustum" scebool
  (cam scecamera))
(def-sce-method sceneentityinstance "IsInstanceInFrustum" scebool
  (cam scecamera))

(defsetter sceneentityinstance "DetermineInstanceLOD"
  (cam scecamera))

;;; SceneEntityProperties
(defsetter sceneentityproperties "InitProperties")

;;; SceneEntityGroup
(defsetter sceneentitygroup "InitGroup")
(defcfun "SCE_SceneEntity_CreateGroup" scesceneentitygroup)
(defsetter sceneentitygroup "DeleteGroup")

(defsetter sceneentitygroup "AddEntity"
  (entity scesceneentity))

;;; SceneEntity
(def-sce-method sceneentity "Init" :void)
(def-sce-method sceneentity "RemoveEntity" :void)
(def-sce-method sceneentity "AddTexture" :int
  (tex scetexture))
(def-sce-method sceneentity "RemoveTexture" :void
  (tex scetexture))

(def-sce-method sceneentity "GetProperties" scesceneentityproperties)

(defprop sceneentity "Mesh" scemesh)
(defprop sceneentity "Shader" sceshader)
(defprop sceneentity "Material" scematerial)

(defsetter sceneentity "SetupBoundingVolume"
  (volume :int))

(def-sce-method sceneentity "HasResourceOfType" scebool
  (type :int))
(def-sce-method sceneentity "HasInstance" scebool)
;; TODO GetInstancesGroup

(defsetter sceneentity "Flush")
(defsetter sceneentity "ApplyProperties")
(defsetter sceneentity "UseRessources")
(defsetter sceneentity "Render")

(defsetter sceneentity "AddInstanceToEntity"
  (einst scesceneentityinstance))

(defconstructor sceneentity)

;;; Scene
(defobject scene)

(defsetter scene "AddCamera"
  (camera scecamera))
(defsetter scene "AddLight"
  (light scelight))
(defsetter scene "AddModel"
  (model scemodel))

(defsetter scene "RemoveModel"
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