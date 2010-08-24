;;; Example inspired from buclet's hello-world.lisp. This example
;;; won't work as-is, you should do some modifications to bullet's C
;;; API, see aerique's patch at
;;; http://code.google.com/p/bullet/issues/detail?id=43
(require :scelisp)
(require :lispbuilder-sdl)
(require :buclet)

(in-package :scelisp)

(defparameter *dynamics-world* nil)
(defparameter *fall-rigid-bodies* nil)
(defparameter *fall-models* nil)
(defparameter *fall-shape* nil)
(defparameter *ground-rigid-body* nil)
(defparameter *ground-shape* nil)
(defparameter *physics-sdk* nil)

(defun run ()
  (init-vars)
  (sdl:with-init ()
    (sdl:window 800 600 :opengl t)
    (setf (sdl:frame-rate) 70)
    (with-interface
      (with-objects ((scene scene)
                     (camera camera)
                     (light light :color '(1.0 1.0 1.0)))
        (sce-camera-setviewport camera 0 0 800 600)
        (sce-matrix4-projection (sce-camera-getproj camera) (radians 70.0)
                                (/ 800.0 600.0) 0.1 10000.0)
        (let ((matrix (sce-node-getmatrix
                       (sce-camera-getnode camera) :node-read-matrix)))
          (sce-matrix4-translate matrix 10.0 10.0 10.0)
          (sce-matrix4-mulroty matrix (radians -20.0))
          (sce-matrix4-mulrotx matrix (radians -20.0)))
        (sce-light-infinite light t)
        (sce-matrix4-translate (sce-node-getmatrix
                                (sce-light-getnode light) :node-read-matrix)
                               1.0 2.4 1.0)
        (sce-scene-addcamera scene camera)
        (sce-scene-addlight scene light)
        (with-mesh (mesh "/home/quentin/scelisp/examples/cube.obj")
          (let ((last-time (get-internal-real-time)))
            (sdl:with-events ()
              (:quit-event ()
                (delete-bodies scene)
                t)
              (:mouse-button-down-event ()
                (add-body scene mesh))
                (:idle ()
                       (let* ((time (get-internal-real-time))
                              (delta (/ (- time last-time) 10.0)))
                         (buclet:step-simulation *dynamics-world* delta)
                         (setf last-time time))
                       (draw-bodies)
                       (sce-scene-update scene camera (null-pointer) 0)
                       (sce-scene-render scene camera (null-pointer) 0)
                       (sdl:update-display)))))))))

(defun draw-bodies ()
  (loop for body in *fall-rigid-bodies*
     for model in *fall-models*
     do
       (let ((matrix (sce-node-getmatrix
                      (sce-model-getrootnode model)
                      :node-read-matrix)))
         (destructuring-bind (x y z) (buclet:get-position body)
           (sce-matrix4-translate matrix x y z))
         (destructuring-bind (x y z a) (buclet:get-orientation body)
           (sce-matrix4-mulrotate matrix a x y z)))))

(defun add-body (scene mesh)
  (let ((model (make-model))
        (body (buclet:create-rigid-body 10.0 *fall-shape*)))
    (sce-model-addentityv model 0 mesh (null-pointer) (null-pointer))
    (sce-model-addnewinstance model 0 1 (null-pointer))
    (sce-model-mergeinstances model)
    (sce-scene-addmodel scene model)

    (buclet:set-position body '(0.0 10.0 0.0))
    (buclet:set-orientation body '(0.0 100.0 10.0 1.0))
    (buclet:add-rigid-body *dynamics-world* body)

    (push body *fall-rigid-bodies*)
    (push model *fall-models*)))

(defun delete-bodies (scene)
  (loop for body in *fall-rigid-bodies*
     for model in *fall-models* do
       (sce-scene-removemodel scene model)
       (sce-model-delete model)
       (buclet:delete-rigid-body body)))

(defun init-vars ()
  (setf *physics-sdk* (buclet:new-bullet-sdk))
  (setf *dynamics-world* (buclet:create-dynamics-world *physics-sdk*))

  (setf *ground-shape* (buclet:new-static-plane-shape '(0.0 1.0 0.0) 1.0))
  (setf *ground-rigid-body* (buclet:create-rigid-body 0.0 *ground-shape*))
  (buclet:set-position *ground-rigid-body* '(0.0 -1.0 0.0))
  (buclet:set-orientation *ground-rigid-body* '(0.0 0.0 0.0 1.0))
  (buclet:add-rigid-body *dynamics-world* *ground-rigid-body*)

  
  (setf *fall-shape* (buclet:new-box-shape 1.0 1.0 1.0))
  
  (setf *fall-models* nil
        *fall-rigid-bodies* nil))


(defun degrees (x)
  (coerce (* (/ 180 pi) x) 'single-float))

(defun radians (x)
  (coerce (* (/ pi 180) x) 'single-float))