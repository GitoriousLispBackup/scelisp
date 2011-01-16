(require :scelisp)
(in-package :scelisp)

(load "camera.lisp")

(defclass shader-app (app) ())

;;; TODO: the skybox is just white, whatever the texture is
(defmethod init ((app shader-app))
  (let* ((sky-tex (make-instance 'texture
                                 :type :tex-cube
                                 :w 1024 :h 1024
                                 :files '("lisp.png" "lisp.png" "lisp.png"
                                          "lisp.png" "lisp.png" "lisp.png")))
         (skybox (make-instance 'skybox
                                :texture sky-tex))
         (model (make-instance 'model
                               :mesh (make-instance 'mesh :file "cube.obj")))
         (rx (make-instance 'inert :coeff 0.1 :accum t))
         (ry (make-instance 'inert :coeff 0.1 :accum t)))
    (setf (model app) model
          (rx app) rx
          (ry app) ry)
    (add (scene app) model)
    (add (scene app) skybox)
    (translate (camera (scene app)) 0.0 0.0 -1.0)))

(defun main ()
  (launch (make-instance 'shader-app)))