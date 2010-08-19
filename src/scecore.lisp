(in-package :scelisp)

(define-foreign-library libscecore
  (:unix (:or "libscecore.so.0" "libscecore.so"))
  (t (:default "libscecore")))
(use-foreign-library libscecore)

;;; Frustum
(defctype scefrustum :pointer)

;;; Node
(defobject node)

(defcenum scenodetype
  :single-matrix-node
  :tree-node)

(defcenum scenodematrixarray
  (:node-read-matrix 0)
  :node-write-matix)

(def-sce-method node "GetMatrix" scematrix4
  (id scenodematrixarray))

(defsetter node "HasMoved")

;;; Camera
(defobject camera)

(defsetter camera "SetViewport"
  (x :int)
  (y :int)
  (w :int)
  (h :int))

(def-sce-method camera "GetView" scematrix4)
(def-sce-method camera "GetViewInverse" scematrix4)
(def-sce-method camera "GetProj" scematrix4)
(def-sce-method camera "GetProjInverse" scematrix4)
(def-sce-method camera "GetFinalViewProj" scematrix4)
(def-sce-method camera "GetFinalViewProjInverse" scematrix4)
(def-sce-method camera "GetFinalView" scematrix4)
(def-sce-method camera "GetFinalViewInverse" scematrix4)

(def-sce-method camera "GetNode" scenode)
(def-sce-method camera "GetFrustum" scefrustum)

(def-sce-method camera "UpdateView" :void)
(def-sce-method camera "UpdateFrustum" :void)
(def-sce-method camera "UpdateViewProj" :void)
(def-sce-method camera "Update" :void)

(defconstructor camera)

;;; Geometry
(defcfun "SCE_Init_Geometry" :int)