(require :scelisp)
(in-package :scelisp)

(defclass app (simple-app)
  ((model :accessor model)
   (rx :accessor rx)
   (ry :accessor ry)))

(defmethod init ((app app))
  (let* ((model (make-instance 'model
                               :mesh (make-instance 'mesh :file "cube.obj")))
         (rx (make-instance 'inert :coeff 0.1 :accum t))
         (ry (make-instance 'inert :coeff 0.1 :accum t)))
    (setf (model app) model
          (rx app) rx
          (ry app) ry)
    (add (scene app) model)
    (translate (camera (scene app)) 0.0 0.0 -1.0)))

(defmethod update ((app app))
  (with-accessors ((scene scene) (model model)
                   (rx rx) (ry ry))
      app
    (compute rx)
    (compute ry)
    (set-position model 0.0 0.0 0.0)
    (scale model 0.3 0.3 0.3)
    (rot-x model (* (value (rx app)) 0.01))
    (rot-z model (* (value (ry app)) 0.01))
    (has-moved (get-node (model app)))))

(defmethod handle-event ((app app) (event (eql :mouse-motion-event))
                         &key state x-rel y-rel)
  (when (= state 1)
    (operate (ry app) #'- x-rel)
    (operate (rx app) #'+ y-rel)))

(defun main ()
  (launch (make-instance 'app)))
