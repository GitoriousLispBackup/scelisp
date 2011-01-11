(require :scelisp)
(in-package :scelisp)

(defun radians (angle)
  (* angle (/ pi 180)))

;;; The player
(defclass player ()
  ((x :accessor pos-x :initarg :x)
   (y :accessor pos-y :initarg :y)
   (z :accessor pos-z :initarg :z)
   (step :accessor player-step :initform 0.1)
   (bobbing :accessor bobbing :initform 0)
   (bobbing-mod :accessor bobbing-mod :initarg :mod)
   (bobbing-func :accessor bobbing-func :initarg :func)
   (bobbing-inc :accessor bobbing-inc :initarg :inc))
  (:default-initargs :x 0 :y 0 :z 0 :mod (* 2 pi) :inc 0.1
                     :func (lambda (x) (/ (sin x) 3))))

(defmethod forward ((p player) &key (step (player-step p)))
  (setf (bobbing p) (mod (+ (bobbing p) (* (signum step) (bobbing-inc p)))
                         (bobbing-mod p)))
  (setf (pos-y p) (funcall (bobbing-func p) (bobbing p)))
  (incf (pos-z p) step))
(defmethod backward ((p player) &key (step (player-step p)))
  (forward p :step (- step)))
(defmethod step-right ((p player) &key (step (player-step p)))
  (decf (pos-x p) step))
(defmethod step-left ((p player) &key (step (player-step p)))
  (step-right p :step (- step)))

;;; The app
(defclass fps-camera (simple-app)
  ((model :accessor model)
   (player :accessor player)))

(defmethod init ((app fps-camera))
  (let ((model (make-instance 'model
                              :mesh (make-instance 'mesh :file "cube.obj"))))
    (sdl:enable-key-repeat 10 10)
    ;; Setup the scene
    (add (scene app) model)
    (setf (model app) model)
    (translate model 0.0 0.0 -5.0)
    (sce-matrix4-mulscale (get-matrix model) 0.3 0.3 0.3)
    (sce-matrix4-mulrotate (get-matrix model) 1.5 1.0 0.1 0.6)
    ;; Initialize the player
    (setf (player app) (make-instance 'player))))

(defmethod update ((app fps-camera))
  (translate (camera (scene app))
             (coerce (pos-x (player app)) 'single-float)
             (coerce (pos-y (player app)) 'single-float)
             (coerce (pos-z (player app)) 'single-float)))

(defmethod handle-event ((app fps-camera) (type (eql :key-down-event))
                         &key key)
  (let ((player (player app)))
    (case key
      (:sdl-key-up (forward player))
      (:sdl-key-down (backward player))
      (:sdl-key-left (step-left player))
      (:sdl-key-right (step-right player)))))

(defun main ()
  (launch (make-instance 'fps-camera)))