;;;; SCEngine's model1 example in Lisp
(require :scelisp)
(in-package :scelisp)

(defun run ()
  (with-simple-scene (:width 800 :height 600
                      :lightcolor (1.0 1.0 1.0)
                      :lightpos (1.0 2.4 1.0))
    (with-simple-model ("cube.obj"
                        :matrix matrix)
      (sce-matrix4-mulscale matrix 0.3 0.3 0.3)
      (sce-matrix4-mulrotx matrix (coerce pi 'single-float))
      (sdl:with-events ()
        (:quit-event () t)
        (:idle ()
          (sce-matrix4-mulrotz matrix 0.02)
          (update-and-render-simple-scene))))))
