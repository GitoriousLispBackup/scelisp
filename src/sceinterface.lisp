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
          (sce-init-interface 0 0)
          ,@body)
     (sce-quit-interface)))

;;; Shaders
(defobject shader)

;;; Textures
(defobject texture)

;;; Lights
(defobject light)

(defstatus light "Activate")

;; TODO GetIterator
(def-sce-method light "GetNode" scenode)

(defsetter light "SetColor"
  (r :float)
  (g :float)
  (b :float))
(eval-when (:compile-toplevel :load-toplevel)
  (addprop 'light "Color"))
;; TODO SetColorv
;; TODO GetColor
;; TODO GetColorv
;; TODO GetPositionv
;; TODO GetDirectionv

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

(defconstructor scene)