(in-package :scelisp)

(define-foreign-library libscecore
  (:unix (:or "libscecore.so.0" "libscecore.so"))
  (t (:default "libscecore")))
(use-foreign-library libscecore)

;;; Node
(defobject node)

(defcenum nodematrixarray
  (:node-read-matrix 0)
  (:node-write-matix 1))

(def-sce-method node "GetMatrix" scematrix
  (id nodematrixarray))

;;; Geometry
(defcfun "SCE_Init_Geometry" :int)