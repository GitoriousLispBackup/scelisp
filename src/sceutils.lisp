(in-package :scelisp)

(define-foreign-library libsceutils
  (:unix (:or "libsceutils.so.0" "libsceutils.so"))
  (t (:default "libsceutils")))
(use-foreign-library libsceutils)

;;; Bitfields
(defctype scebitfield :unsigned-int)

;;; Bools
(define-foreign-type scebool ()
  ()
  (:actual-type :int)
  (:simple-parser scebool))

(defmethod expand-to-foreign (value (type scebool))
  `(if ,value 1 0))

(defmethod expand-from-foreign (value (type scebool))
  `(not (zerop ,value)))

;;; Matrices
(defctype scematrix (:pointer :float))

(defcfun "SCE_Matrix4_Translate" :void
  (matrix scematrix)
  (x :float)
  (y :float)
  (z :float))

(defcfun "SCE_Matrix4_Scale" :void
  (matrix scematrix)
  (x :float)
  (y :float)
  (z :float))

(defcfun "SCE_Matrix4_Rotate" :void
  (matrix scematrix)
  (a :float)
  (x :float)
  (y :float)
  (z :float))

(defcfun "SCE_Matrix4_MulScale" :void
  (matrix scematrix)
  (x :float)
  (y :float)
  (z :float))

(defcfun "SCE_Matrix4_MulRotX" :void
  (matrix scematrix)
  (angle :float))
(defcfun "SCE_Matrix4_MulRotY" :void
  (matrix scematrix)
  (angle :float))
(defcfun "SCE_Matrix4_MulRotZ" :void
  (matrix scematrix)
  (angle :float))

;;; Errors
(defcfun "SCE_Error_HaveError" scebool)
(defcfun "SCE_Error_Out" :void)
(defcfun "SCE_Error_Clean" :void)