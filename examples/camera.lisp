(require :scelisp)
(in-package :scelisp)

(defparameter *width* 800)
(defparameter *height* 600)

(defclass app (sce)
  ((scene :accessor scene)
   (model :accessor model)
   (rx :accessor rx)
   (ry :accessor ry)))

(defmethod init ((app app))
  (let* ((camera (make-instance 'camera :width *width* :height *height*))
         (red-light (make-instance 'light :color '(0.8 0.2 0.2)
                                   :x 1.0 :y 2.4 :z 1.0))
         (green-light (make-instance 'light :color '(0.2 0.8 0.2)
                                     :x 1.0 :y -2.4 :z 1.0))
         (model (make-instance 'model
                               :mesh (make-instance 'mesh :file "cube.obj")))
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

(defmethod update ((app app))
  (update (scene app))
  (compute (rx app))
  (compute (ry app))
  (let ((matrix (get-matrix (model app))))
    (translate (model app) 0.0 0.0 0.0)
    (sce-matrix4-mulscale matrix 0.3 0.3 0.3)
    (sce-matrix4-mulrotx matrix (* (value (rx app)) 0.01))
    (sce-matrix4-mulrotz matrix (* (value (ry app)) 0.01)))
  (has-moved (get-node (model app))))

(defmethod draw ((app app))
  (draw (scene app)))

(defmethod handle-event ((app app) (event (eql :quit-event))
                         &key)
  (free (rx app))
  (free (ry app))
  t)
(defmethod handle-event ((app app) (event (eql :mouse-motion-event))
                         &key state x-rel y-rel)
  (when (= state 1)
    (operate (ry app) #'- x-rel)
    (operate (rx app) #'+ y-rel)))

(defun main ()
  (launch (make-instance 'app :width *width* :height *height*)))
