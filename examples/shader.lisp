(require :scelisp)
(in-package :scelisp)

(load "camera.lisp")

(defclass shader-app (app)
  ((vshader :accessor vertex-shader)
   (pshader :accessor pixel-shader)))

(defmethod init ((app shader-app))
  (let* ((vshader (make-instance 'shader
                                 :vertex "vertex.glsl"))
         (pshader (make-instance 'shader
                                 :pixel "color.frag"))
         (model (make-instance 'model
                               :mesh (make-instance 'mesh :file "cube.obj")
                               :shader pshader))
         (rx (make-instance 'inert :coeff 0.1 :accum t))
         (ry (make-instance 'inert :coeff 0.1 :accum t)))
    (setf (model app) model
          (vertex-shader app) vshader
          (pixel-shader app) pshader
          (rx app) rx
          (ry app) ry)
    (add (scene app) model)
    (translate (camera (scene app)) 0.0 0.0 -1.0)))

(defun main ()
  (launch (make-instance 'shader-app)))