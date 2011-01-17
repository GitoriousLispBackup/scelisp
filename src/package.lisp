(defpackage :scelisp
  (:use :common-lisp :cffi :alexandria)
  (:nicknames :sce)
  (:export
   ;;; api.lisp
   :init :create :draw :objects :update :free :add
   ;; Movables objects
   :get-matrix :get-node
   :set-position :translate :set-rotation :rot :rot-x :rot-y :rot-z 
   :set-scale :scale
   ;; Lights
   :infinite :active :color :angle :radius :intensity
   ;; Cameras
   :set-viewport :get-view
   ;; Inerts
   :operate :compute :value
   ;; Shaders
   :use :shader-param :with-shader
   ;; SCE class
   :launch :handle-event
   ;; Class names
   :sce :simple-app
   :light :node :camera :mesh :model :inert :texture :shader :skybox :scene
   ))
