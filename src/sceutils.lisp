(in-package :scelisp)

(define-foreign-library libsceutils
  (:unix (:or "libsceutils.so.0" "libsceutils.so"))
  (t (:default "libsceutils")))
(use-foreign-library libsceutils)

(defctype scebitfield :unsigned-int)

(define-foreign-type scebool ()
  ()
  (:actual-type :int)
  (:simple-parser scebool))

(defmethod expand-to-foreign (value (type scebool))
  `(if ,value 1 0))

(defmethod expand-from-foreign (value (type scebool))
  `(not (zerop ,value)))