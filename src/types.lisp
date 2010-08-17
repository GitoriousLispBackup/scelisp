(in-package :scelisp)

;;; Vectors, used by Get/Set*v functions
(define-foreign-type scevector-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser scevector))

(defmethod expand-to-foreign (value (type scevector-type))
  `(foreign-alloc :float :initial-contents
                  (list ,@(loop for x from 0 to 2 collect
                             `(aref ,value ,x)))))

(defmethod expand-from-foreign (value (type scevector-type))
  `(vector ,@(loop for x from 0 to 2 collect
                  `(mem-aref ,value :float ,x))))