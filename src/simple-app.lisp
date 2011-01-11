(in-package :scelisp)

(defclass simple-app (sce)
  ((scene :accessor scene)))

(defmethod init :before ((app simple-app))
  (let* ((camera (make-instance 'camera
                                :width (width app)
                                :height (height app)))
         (light (make-instance 'light :color '(1.0 1.0 1.0)
                               :x 1.0 :y 2.4 :z 1.0))
         (scene (make-instance 'scene :camera camera)))
    (setf (scene app) scene)
    (add scene light)))

(defmethod draw :after ((app simple-app))
  (draw (scene app)))

(defmethod update :after ((app simple-app))
  (update (scene app)))