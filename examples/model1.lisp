;;;; SCEngine's model1 example in Lisp
(require :scelisp)
(in-package :scelisp)

(defparameter *width* 800)
(defparameter *height* 600)

(defclass app (sce)
  ((scene :accessor scene)
   (model :accessor model)))

(defmethod init ((app app))
  (let* ((camera (make-instance 'camera :width *width* :height *height*))
         (light (make-instance 'light :x 1.0 :y 2.4 :z 1.0))
         (model (make-instance 'model
                               :mesh (make-instance 'mesh :file "cube.obj")))
         (scene (make-instance 'scene :camera camera)))
    (setf (scene app) scene
          (model app) model)
    (add scene model)
    (add scene light)
    (sce-matrix4-mulscale (get-matrix model) 0.3 0.3 0.3)))

(defmethod update ((app app))
  (update (scene app)))

(defmethod draw ((app app))
  (draw (scene app)))

(defun main ()
  (launch (make-instance 'app :width *width* :height *height*)))