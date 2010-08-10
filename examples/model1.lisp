;;;; SCEngine's model1 example in Lisp
(require :lispbuilder-sdl)
(require :scelisp)
(in-package :scelisp)

(defvar *scene*)
(defvar *camera*)

(defun init ()
  (sce-init-interface 0 0)
  (setf *scene* (sce-scene-create))
  (setf *camera* (sce-camera-create))
  (sce-scene-addcamera *scene* *camera*)
  (let ((light (sce-light-create))
        ;; TODO: use relative path
        (mesh (sce-mesh-load "/home/quentin/sce/samples/model1/spaceship.obj" 2))
        (model (sce-model-create)))
    ;; Light
    (sce-light-setcolor light 1.0 1.0 1.0)
    (sce-light-infinite light t)
    (sce-scene-addlight *scene* light)
    ;; Mesh
    (sce-mesh-autobuild mesh)
    ;; Model
    (sce-model-addentity model 0 mesh (null-pointer) (null-pointer))
    (sce-model-addnewinstance model 0 1 (null-pointer))
    (sce-model-mergeinstances model)
    (sce-scene-addmodel *scene* model)))

(defun quit ()
  (sce-camera-delete *camera*)
  (sce-scene-delete *scene*)
  (sce-quit-interface))

(defun draw ()
  (sce-scene-update *scene* *camera* (null-pointer) 0)
  (sce-scene-render *scene* *camera* (null-pointer) 0))

(defun run ()
  (sdl:with-init ()
    (sdl:window 200 200 :opengl t)
    (setf (sdl:frame-rate) 15)
    (init)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
        (draw)
        (sdl:update-display)))
    (quit)))