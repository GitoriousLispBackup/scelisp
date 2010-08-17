;;;; SCEngine's model1 example in Lisp
(require :lispbuilder-sdl)
(require :scelisp)
(in-package :scelisp)

(defun run ()
  (sdl:with-init ()
    (sdl:window 800 600 :opengl t)
    (setf (sdl:frame-rate) 15)
    (with-interface
      (with-objects ((scene scene)
                     (camera camera)
                     (light light :color '(1.0 0.5 0.5))
                     (model model))
        (sce-light-infinite light t)
        (sce-matrix4-translate (sce-node-getmatrix
                                (sce-light-getnode light) :node-read-matrix)
                               1.0 2.4 1.0)
        (sce-scene-addcamera scene camera)
        (sce-scene-addlight scene light)
        ;; TODO: use relative path
        (with-mesh (mesh "/home/quentin/sce/samples/model1/spaceship.obj")
          (sce-model-addentity model 0 mesh (null-pointer) (null-pointer))
          (sce-model-addnewinstance model 0 1 (null-pointer))
          (sce-model-mergeinstances model)
          (sce-scene-addmodel scene model)
          (let ((matrix (sce-node-getmatrix
                         (sce-model-getrootnode model)
                         :node-read-matrix)))
            (sce-matrix4-scale matrix 0.3 0.3 0.3)
            (sce-matrix4-mulrotx matrix (coerce pi 'single-float))
            (sdl:with-events ()
              (:quit-event () t)
              (:idle ()
                     (when (sce-error-haveerror)
                       (sce-error-out))
                     (sce-matrix4-mulrotz matrix 0.02)
                     (sce-scene-update scene camera (null-pointer) 0)
                     (sce-scene-render scene camera (null-pointer) 0)
                     (sdl:update-display)))))))))