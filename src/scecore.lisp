(in-package :scelisp)

(define-foreign-library libscecore
  (:unix (:or "libscecore.so.0" "libscecore.so"))
  (t (:default "libscecore")))
(use-foreign-library libscecore)

;;; Node
(defctype scenode :pointer)

(defcenum nodematrixarray
  (:node-read-matrix 0)
  (:node-write-matix 1))

(defcfun "SCE_Node_Create" scenode)
(defcfun "SCE_Node_Delete" :void
  (node scenode))

(defcfun "SCE_Node_GetMatrix" scematrix
  (node scenode)
  (id nodematrixarray))