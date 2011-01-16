(in-package :scelisp)

;; Maps SCE Texture types to GL Texture (see GL/gl.h)
(defcenum scetextype
  (:tex-1d #x0DE0)
  (:tex-2d #x0DE1)
  (:tex-3d #x806F)
  (:tex-cube #x8513))