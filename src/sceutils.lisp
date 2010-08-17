(in-package :scelisp)

(define-foreign-library libsceutils
  (:unix (:or "libsceutils.so.0" "libsceutils.so"))
  (t (:default "libsceutils")))
(use-foreign-library libsceutils)

;;; Bitfields
(defctype scebitfield :unsigned-int)

;;; Bools
(define-foreign-type scebool-type ()
  ()
  (:actual-type :int)
  (:simple-parser scebool))

(defmethod expand-to-foreign (value (type scebool-type))
  `(if ,value 1 0))

(defmethod expand-from-foreign (value (type scebool-type))
  `(not (zerop ,value)))

;;; Matrices
(defctype scematrix4 (:pointer :float))
#|(define-foreign-type scematrix4-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser scematrix4))|#

;; TODO
#|(defmethod expand-to-foreign (value (type scematrix4-type))
  `(foreign-alloc :float :initial-contents
                  (list ,@(loop for x from 0 to 3 append
                             (loop for y from 0 to 3 collect
                                  `(aref ,value ,x ,y))))))|#

#|(defmethod expand-from-foreign (value (type scematrix4-type))
  `(make-array '(4 4)
               :initial-contents
               (list
                ,@(loop for x from 0 to 3 collect
                      `(list
                        ,@(loop for y from 0 to 3 collect
                               `(mem-aref ,value :float ,(+ (* x 4) y))))))))|#

(defcfun "SCE_Matrix4_Translate" :void
  (matrix scematrix4)
  (x :float)
  (y :float)
  (z :float))

(defcfun "SCE_Matrix4_Scale" :void
  (matrix scematrix4)
  (x :float)
  (y :float)
  (z :float))

(defcfun "SCE_Matrix4_Rotate" :void
  (matrix scematrix4)
  (a :float)
  (x :float)
  (y :float)
  (z :float))

(defcfun "SCE_Matrix4_MulScale" :void
  (matrix scematrix4)
  (x :float)
  (y :float)
  (z :float))

(defcfun "SCE_Matrix4_MulRotX" :void
  (matrix scematrix4)
  (angle :float))
(defcfun "SCE_Matrix4_MulRotY" :void
  (matrix scematrix4)
  (angle :float))
(defcfun "SCE_Matrix4_MulRotZ" :void
  (matrix scematrix4)
  (angle :float))

;;; Errors
(defcfun "SCE_Error_HaveError" scebool)
(defcfun "SCE_Error_Out" :void)
(defcfun "SCE_Error_Clear" :void)