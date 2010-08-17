(in-package :scelisp)

(define-foreign-library libscecore
  (:unix (:or "libscecore.so.0" "libscecore.so"))
  (t (:default "libscecore")))
(use-foreign-library libscecore)

;;; Camera
(defobject camera)

(defsetter camera "SetViewport"
  (x :int)
  (y :int)
  (w :int)
  (h :int))

(def-sce-method camera "GetViewport" :pointer)

(defconstructor camera)

;;; Node
(defobject node)

(defcenum nodematrixarray
  (:node-read-matrix 0)
  (:node-write-matix 1))

(def-sce-method node "GetMatrix" scematrix4
  (id nodematrixarray))

;;; Geometry
(defcfun "SCE_Init_Geometry" :int)