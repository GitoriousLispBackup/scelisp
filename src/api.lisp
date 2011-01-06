(in-package :scelisp)
;;;; TODO: create the pointer for each class
(defgeneric init (object)
  (:documentation "Initialize an object"))

(defgeneric create (object)
  (:documentation "Set the pointer slot for an object")
  (:method (object)
    nil))

(defgeneric draw (object)
  (:documentation "Draw an object on the screen"))

(defgeneric objects (object)
  (:documentation "The objects contained by an object"))

(defgeneric update (object)
  (:documentation "Update an object")
  (:method (object)
    (mapcar #'update (objects object))))

(defgeneric add (object something)
  (:documentation "Add something to an object"))

(defgeneric objects (object)
  (:documentation "Return the object contained by object")
  (:method (object)
    nil))

(defgeneric get-node (object)
  (:documentation "Return the node of an object")
  (:method (object)
    nil))

(defgeneric get-matrix (object &optional type)
  (:documentation "Return the matrix of an object")
  (:method (object &optional type)
    (let ((node (get-node object)))
      (when node
        (if type
            (get-matrix node type)
            (get-matrix node))))))

;;; SCE class
(defclass sce ()
  ((width :type integer :accessor width :initarg :width)
   (height :type integer :accessor height :initarg :height))
  (:documentation "The main class that should be inherited"))

(defgeneric launch (object)
  (:documentation "Launch SCEngine"))

(defmethod launch ((sce sce))
  (sdl:with-init ()
    (sdl:window (width sce) (height sce) :opengl t)
    (with-interface
      (init sce)
      (sdl:with-events ()
        (:quit-event () t)
        (:idle ()
          (update sce)
          (draw sce)
          (sdl:update-display))))))

;;; sceobject
(defclass sceobject ()
  ((pointer :accessor pointer :initform (null-pointer) :initarg :pointer)
   (objects :accessor objects :initform nil :initarg :objects))
  (:documentation "A generic SCE object"))

(defmethod add :after (object something)
  (push something (objects object)))

(defmethod initialize-instance :before ((obj sceobject) &key pointer
                                        &allow-other-keys)
  (unless pointer
    (create obj)))

(defmethod create :before ((obj sceobject))
  (when (and (slot-boundp obj 'pointer) (pointer obj))
    (warn "Recreating an object that was already created")))

;;; scemovable
(defclass scemovable (sceobject)
  ()
  (:documentation "A SCE movable object (with a node)"))

(defgeneric translate (object x y z)
  (:documentation "Translate a movable object")
  (:method (object x y z)
    (sce-matrix4-translate (get-matrix object :node-read-matrix)
                           x y z)))

(defmethod initialize-instance :after ((obj scemovable) &key
                                       (x .0) (y .0) (z .0)
                                       &allow-other-keys)
  (translate obj x y z))

;;; SCELight
(defclass light (sceobject)
  ()
  (:documentation "A light"))

(defmethod create ((l light))
  (setf (pointer l) (sce-light-create)))

(defgeneric infinite (object)
  (:documentation "Is the light infinite?"))
(defgeneric active (object)
  (:documentation "Is the light active?"))
(defgeneric color (object)
  (:documentation "The color of the object"))
(defgeneric angle (object)
  (:documentation "The angle of the light"))
(defgeneric radius (object)
  (:documentation "The radius of the light"))
(defgeneric intensity (object)
  (:documentation "The intensity of the light)"))

(defmethod (setf active) (status (l light))
  (sce-light-activate (pointer l) status))
(defmethod active ((l light))
  (sce-light-isactivated (pointer l)))

(defmethod (setf infinite) (status (l light))
  (sce-light-infinite (pointer l) status))
(defmethod infinite ((l light))
  (sce-light-isinfinite (pointer l)))

(defmethod get-node ((l light))
  (sce-light-getnode (pointer l)))

(defmethod (setf color) (color (l light))
  (sce-light-setcolor (pointer l) (first color) (second color) (third color)))
;; TODO: Does it return a list ?
(defmethod color ((l light))
  (sce-light-getcolor (pointer l)))

;; TODO: GetPositionv GetDirectionv

(defmethod (setf angle) (angle (l light))
  (sce-light-setangle (pointer l) angle))
(defmethod angle ((l light))
  (sce-light-getangle (pointer l)))

(defmethod (setf intensity) (intensity (l light))
  (sce-light-setintensity (pointer l) intensity))
(defmethod intensity ((l light))
  (sce-light-getintensity (pointer l)))

(defmethod (setf radius) (radius (l light))
  (sce-light-setradius (pointer l) radius))
(defmethod radius ((l light))
  (sce-light-getradius (pointer l)))

(defmethod initialize-instance :after ((l light) &key
                                       (activate t) (infinite t)
                                       color angle intensity radius)
  (setf (active l) activate
        (infinite l) infinite)
  (when color (setf (color l) color))
  (when angle (setf (angle l) angle))
  (when intensity (setf (intensity l) intensity))
  (when radius (setf (radius l) radius)))

;;; SCENode
(defclass node (sceobject)
  ()
  (:documentation "A node"))

(defmethod get-node :around (object)
  (make-instance 'node
                 :pointer (call-next-method)))

(defmethod create ((n node))
  (setf (pointer n) (sce-node-create)))

(defgeneric has-moved (object)
  (:documentation "Call this when the node moved"))

(defmethod get-matrix ((n node) &optional (type :node-read-matrix))
  (sce-node-getmatrix (pointer n) type))

(defmethod has-moved ((n node))
  (sce-node-hasmoved (pointer n)))

;;; SCECamera
(defclass camera (scemovable)
  ()
  (:documentation "A camera"))

(defmethod create ((c camera))
  (setf (pointer c) (sce-camera-create)))

(defgeneric set-viewport (object x y w h)
  (:documentation "Set the viewport of a camera"))

(defmethod set-viewport ((c camera) x y w h)
  (sce-camera-setviewport (pointer c) x y w h))

;; TODO: GetView etc.

(defmethod get-node ((c camera))
  (sce-camera-getnode (pointer c)))

(defmethod initialize-instance :after ((c camera) &key
                                       (width 800) (height 600)
                                       &allow-other-keys)
  (set-viewport c 0 0 width height))

;;; SCEMesh
(defclass mesh (sceobject)
  ())

(defmethod initialize-instance :after ((m mesh) &key file (force 2)
                                       &allow-other-keys)
  (unless file (error "Can't create a mesh without an obj file"))
  (setf (pointer m) (sce-mesh-load file force))
  (sce-mesh-autobuild (pointer m)))

;;; SCEModel
(defclass model (scemovable)
  ())

(defmethod create ((m model))
  (setf (pointer m) (sce-model-create)))

(defmethod get-node ((m model))
  (sce-model-getrootnode (pointer m)))

(defmethod add ((m model) (mesh mesh))
  ;; TODO: should more parameters be availables ?
  (sce-model-addnewentityv (pointer m) 0 1 (pointer mesh)
                           (null-pointer) (null-pointer))
  (sce-model-addnewinstance (pointer m) 0 1 (null-pointer))
  (sce-model-mergeinstances (pointer m)))

;;; Scene
(defclass scene (sceobject)
  ((camera :type camera :accessor camera :initarg :camera))
  (:documentation "A scene"))

(defmethod create ((s scene))
  (setf (pointer s) (sce-scene-create)))

(defmethod add ((s scene) (c camera))
  (sce-scene-addcamera (pointer s) (pointer c)))
(defmethod add ((s scene) (l light))
  (sce-scene-addlight (pointer s) (pointer l)))
(defmethod add ((s scene) (m model))
  (sce-scene-addmodel (pointer s) (pointer m)))

(defmethod update ((s scene))
  (unless (camera s)
    (error "Can't update a scene without a camera"))
  (sce-scene-update (pointer s) (pointer (camera s)) (null-pointer) 0))
(defmethod draw ((s scene))
  (unless (camera s)
    (error "Can't draw a scene without a camera"))
  (sce-scene-render (pointer s) (pointer (camera s)) (null-pointer) 0))

(defmethod initialize-instance :after ((s scene) &key objects
                                       &allow-other-keys)
  (mapcar (curry #'add s) objects))
