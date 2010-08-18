(require :lispbuilder-sdl)
(require :scelisp)
(in-package :scelisp)

(defun run ()
  (let ((distance 0.0))
    (sdl:with-init ()
      (sdl:window 800 600 :opengl t)
      (setf (sdl:frame-rate) 15)
      (with-interface
        (with-objects ((scene scene)
                       (camera camera)
                       (light light :color '(1.0 0.5 0.5))
                       (model model))
          (sce-camera-setviewport camera 0 0 800 600)
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
              (sce-matrix4-mulscale matrix 0.3 0.3 0.3)
              (sce-matrix4-mulrotx matrix (coerce pi 'single-float)))
            (with-foreign-objects ((rx 'sceinertvar)
                                   (ry 'sceinertvar))
              (sce-inert-init rx)
              (sce-inert-init ry)
              (sce-inert-setcoefficient rx 0.1)
              (sce-inert-setcoefficient ry 0.1)
              (sce-inert-accum rx 1)
              (sce-inert-accum ry 1)
              
              (sdl:with-events ()
                (:quit-event () t)
                (:mouse-motion-event (:state state :x-rel dx :y-rel dy)
                                     (when (= state 1)
                                       (sce-inert-operator ry #'+ dx)
                                       (sce-inert-operator rx #'+ dy)))
                (:mouse-button-down-event (:button button)
                                          (case button
                                            (4 (decf distance 0.1))
                                            (5 (incf distance 0.1))))
                (:idle ()
                       (sce-inert-compute rx)
                       (sce-inert-compute ry)
                       (let ((matrix (sce-camera-getview camera)))
                         (sce-matrix4-translate matrix 0.0 distance 0.0)
                         (sce-matrix4-mulrotx matrix (* (sce-inert-get rx) -0.2 0.017))
                         (sce-matrix4-mulrotz matrix (* (sce-inert-get ry) -0.2 0.017)))
                       (sce-node-hasmoved (sce-model-getrootnode model))
                       (sce-scene-update scene camera (null-pointer) 0)
                       (sce-scene-render scene camera (null-pointer) 0)
                       (sdl:update-display))))))))))