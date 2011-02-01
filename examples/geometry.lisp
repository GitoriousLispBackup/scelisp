(require :scelisp)
(in-package :scelisp)

(defclass geom-app (simple-app)
  ((model :accessor model)))

(defmethod init ((app geom-app))
  (let* ((geom (make-instance 'geometry
                              :primitive :triangles
                              :positions '(0 0 0 1 0 0 1 1 0
                                           0 0 0 0 1 0 1 1 0)))
         (model (make-instance 'model
                               :mesh (make-instance 'mesh :geometry geom))))
    (setf (model app) model)
    (add (scene app) model)
    (set-position model 0.0 0.0 -2.0)))

(defun main ()
  (launch (make-instance 'geom-app)))