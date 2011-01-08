(require :scelisp)
(in-package :scelisp)

(load "camera.lisp")

(defclass tex-app (app)
  ((tex :accessor app)))

(defmethod init ((app app))
  (let* ((camera (make-instance 'camera :width *width* :height *height*))
         (red-light (make-instance 'light :color '(0.8 0.2 0.2)
                                   :x 1.0 :y 2.4 :z 1.0))
         (green-light (make-instance 'light :color '(0.2 0.8 0.2)
                                     :x 1.0 :y -2.4 :z 1.0))
         (model (make-instance 'model
                               :mesh (make-instance 'mesh :file "cube.obj")
                               :texture (make-instance 'texture :file "lisp.png")))
         (scene (make-instance 'scene :camera camera))
         (rx (make-instance 'inert :coeff 0.1 :accum 1))
         (ry (make-instance 'inert :coeff 0.1 :accum 1)))
    (setf (scene app) scene
          (model app) model
          (rx app) rx
          (ry app) ry)
    (add scene red-light)
    (add scene green-light)
    (add scene model)
    (sce-matrix4-mulscale (get-matrix model) 0.3 0.3 0.3)))

(defun main ()
  (launch (make-instance 'tex-app)))
