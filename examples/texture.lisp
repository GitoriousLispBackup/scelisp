(require :scelisp)
(in-package :scelisp)

(load "camera.lisp")

(defclass tex-app (app)
  ((tex :accessor app)))

(defmethod init ((app tex-app))
  (let* ((model (make-instance 'model
                               :mesh (make-instance 'mesh :file "cube.obj")
                               :texture (make-instance 'texture :file "lisp.png")))
         (rx (make-instance 'inert :coeff 0.1 :accum t))
         (ry (make-instance 'inert :coeff 0.1 :accum t)))
    (setf (model app) model
          (rx app) rx
          (ry app) ry)
    (add (scene app) model)
    (translate (camera (scene app)) 0.0 0.0 -1.0)))

(defun main ()
  (launch (make-instance 'tex-app)))
