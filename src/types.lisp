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

;;; List types, used by some *v functions
(defmacro deflist-type (name &optional (type name))
  (let ((typename (symbolicate name 'list-type)))
    `(progn
       (define-foreign-type ,typename ()
         ()
         (:actual-type :pointer)
         (:simple-parser ,(symbolicate name 'list)))
       (defmethod expand-to-foreign (value (type ,typename))
         `(foreign-alloc ,',type
                         :initial-contents ,value))
       ;; Not implemented yet, and won't never be implemented I guess
       ;; (we don't seem to need functions returning lists)
       (defmethod expand-from-foreign (value (type ,typename))
         `(error "Can't expand from foreign to list")))))

(deflist-type string :string)
(deflist-type scetexture)