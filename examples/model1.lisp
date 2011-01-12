;;;; SCEngine's model1 example in Lisp
(require :scelisp)
(in-package :scelisp)

(defclass model-app (simple-app)
  ((model :accessor model)))

(defmethod init ((app model-app))
  (let* ((model (make-instance 'model
                               :mesh (make-instance 'mesh :file "cube.obj"))))
    (setf (model app) model)
    (add (scene app) model)
    (set-position model 0.0 0.0 -2.0)))

(defun main ()
  (launch (make-instance 'model-app)))