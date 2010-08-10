(in-package :scelisp)

(define-foreign-library libsceinterface
  (:unix (:or "libsceinterface.so.0" "libsceinterface.so"))
  (t (:default "libsceinterface")))
(use-foreign-library libsceinterface)

(defcfun "SCE_Init_Interface" :int
  (outlog :pointer)
  (flags scebitfield))