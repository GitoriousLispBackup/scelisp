(require :scelisp)
(in-package :scelisp)

(defclass game (sce)
  ((scene :accessor game-scene)
   (cube :accessor game-cube)
   (pos-x :accessor pos-x :initform 0.0)))

(defmethod init ((game game))
  (let* ((camera (make-instance 'camera
                               :width 800 :height 600))
         (light (make-instance 'light
                               :infinite t :color '(1.0 1.0 1.0)
                               :x 1.0 :y 2.4 :z 1.0))
         (cube (make-instance 'model
                              :mesh (make-instance 'mesh :file "/home/quentin/scelisp/examples/cube.obj")))
         (scene (make-instance 'scene
                               :camera camera)))
    (setf (game-scene game) scene
          (game-cube game) cube)
    (add scene cube)
    (add scene light)
    (sce-matrix4-mulscale (get-matrix cube) 0.3 0.3 0.3)))

(defmethod update ((game game))
  (update (game-scene game)))

(defmethod draw ((game game))
;  (translate (game-cube game) (pos-x game) 0.0 0.0)
  (draw (game-scene game)))

(defmethod handle-event ((game game) (event (eql :mouse-button-down-event)))
  (incf (pos-x game)))

(defun main ()
  (launch (make-instance 'game :width 800 :height 600)))