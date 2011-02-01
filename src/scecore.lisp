(in-package :scelisp)

(define-foreign-library libscecore
  (:unix (:or "libscecore.so.0" "libscecore.so"))
  (t (:default "libscecore")))
(use-foreign-library libscecore)

;;; Frustum
(defctype scefrustum :pointer)

;;; Node
(defobject node)

(defcenum scenodetype
  :single-matrix-node
  :tree-node)

(defcenum scenodematrixarray
  (:node-read-matrix 0)
  :node-write-matix)

(def-sce-method node "GetMatrix" scematrix4
  (id scenodematrixarray))

(defsetter node "HasMoved")

;;; Camera
(defobject camera)

(defsetter camera "SetViewport"
  (x :int)
  (y :int)
  (w :int)
  (h :int))

(def-sce-method camera "GetView" scematrix4)
(def-sce-method camera "GetViewInverse" scematrix4)
(def-sce-method camera "GetProj" scematrix4)
(def-sce-method camera "GetProjInverse" scematrix4)
(def-sce-method camera "GetFinalViewProj" scematrix4)
(def-sce-method camera "GetFinalViewProjInverse" scematrix4)
(def-sce-method camera "GetFinalView" scematrix4)
(def-sce-method camera "GetFinalViewInverse" scematrix4)

(def-sce-method camera "GetNode" scenode)
(def-sce-method camera "GetFrustum" scefrustum)

(def-sce-method camera "UpdateView" :void)
(def-sce-method camera "UpdateFrustum" :void)
(def-sce-method camera "UpdateViewProj" :void)
(def-sce-method camera "Update" :void)

(defconstructor camera)

;;; Geometry
(defctype scevertices :pointer) ; list of floats
(defctype sceindices :pointer) ; list of shorts

(defcenum sceeprimitivetype
  :points
  :lines
  :triangles
  :triangle-strip
  :triangle-fan)
(defcenum sceevertexattribute
  (:position 1)
  :rolor
  :normal

  :texcoord0 :texcoord1 :texcoord2 :texcoord3
  :texcoord4 :texcoord5 :texcoord6 :texcoord7

  (:attrib0 128)
  :attrib1 :attrib2 :attrib3 :attrib4 :attrib5
  :attrib6 :attrib7 :attrib8 :attrib9 :attrib10
  :attrib11 :attrib12 :attrib13 :attrib14 :attrib15)

(defcfun "SCE_Init_Geometry" :int)
(defcfun "SCE_Quit_Geometry" :int)
(defcfun "SCE_Geometry_GetResourceType" :int)

(eval-when (:compile-toplevel :load-toplevel)
  (setf (gethash 'geometryarraydata *types*) "Geometry")
  (setf (gethash 'geometryarray *types*) "Geometry"))
(defctype scegeometryarraydata :pointer)
(defctype scegeometryarray :pointer)

(defsetter geometryarraydata "InitArrayData")
(defcfun "SCE_Geometry_CreateArrayData" scegeometryarraydata)
(defsetter geometryarraydata "DeletArrayData")

;; TODO: User things
(defsetter geometryarray "InitArray")
(defcfun "SCE_Geometry_CreateArray" scegeometryarray)
;; TODO: CreateArrayFrom
(defsetter geometryarray "DeleteArray")
(defsetter geometryarray "CopyArray"
  (array scegeometryarray))

(defobject geometry)
(defsetter geometry "Init")
(defsetter geometry "DeleteIndexArray")

(defsetter geometryarray "AttachArray"
  (array scegeometryarray))
(def-sce-method geometryarray "GetRoot" scegeometryarray)
(def-sce-method geometryarray "GetChild" scegeometryarray)
;; TODO: Modified & Unmodified
(defsetter geometryarray "UpdateArray")
(defsetter geometry "Update")

(defsetter geometryarray "SetArrayData"
  (attrib sceevertexattribute)
  (type sceetype)
  (stride :unsigned-int)
  (size :int)
  (data :pointer)
  (canfree scebool))

(defsetter geometryarray "SetArrayPosition"
  (stride :unsigned-int)
  (size :int)
  (data scevertices)
  (canfree scebool))

(defsetter geometryarray "SetArrayTexCoord"
  (unit :unsigned-int)
  (stride :unsigned-int)
  (size :int)
  (data scevertices)
  (canfree scebool))

(defsetter geometryarray "SetArrayNormal"
  (stride :unsigned-int)
  (data scevertices)
  (canfrfee scebool))

(defsetter geometryarray "SetArrayTangent"
  (stride :unsigned-int)
  (data scevertices)
  (canfree scebool))

(defsetter geometryarray "SetArrayBinormal"
  (stride :unsigned-int)
  (data scevertices)
  (canfree scebool))

(defsetter geometryarray "SetArrayIndices"
  (type sceetype)
  (data :pointer)
  (canfree scebool))

(def-sce-method geometryarray "GetData" :pointer)
(def-sce-method geometryarray "GetArrayVertexAttribute" sceevertexattribute)
(def-sce-method geometryarray "GetArrayData" scegeometryarraydata)

(defsetter geometry "AddArray"
  (array scegeometryarray))
(defsetter geometry "AddArrayRec"
  (array scegeometryarray))

(defsetter geometry "AddNewArray"
  (attrib sceevertexattribute)
  (type sceetype)
  (stride :unsigned-int)
  (size :int)
  (data :pointer)
  (canfree scebool))

(defsetter geometry "AddArrayDup"
  (array scegeometryarray)
  (canfree scebool))
(defsetter geometry "AddArrayRecDup"
  (array scegeometryarray)
  (canfree scebool))
(defsetter geometry "AddArrayDupDup"
  (array scegeometryarray)
  (keep scebool))
(defsetter geometry "AddArrayRecDupDup"
  (array scegeometryarray)
  (keep scebool))

(defsetter geometryarray "RemoveArray")

(defsetter geometry "SetIndexArray"
  (array scegeometryarray)
  (canfree scebool))
(defsetter geometry "SetIndexArrayDup"
  (array scegeometryarray)
  (canfree scebool))
(defsetter geometry "SetIndexArrayDupDup"
  (array scegeometryarray)
  (keep scebool))

(def-sce-method geometry "GetIndexArray" scegeometryarray)

(defsetter geometry "SetData"
  (pos scevertices)
  (nor scevertices)
  (tex scevertices)
  (index sceindices)
  (n-vertices :unsigned-int)
  (n-indices :unsigned-int))
(defsetter geometry "SetDataDup"
  (pos scevertices)
  (nor scevertices)
  (tex scevertices)
  (index sceindices)
  (n-vertices :unsigned-int)
  (n-indices :unsigned-int))

(def-sce-method geometry "GetPositionsArray" scegeometryarray)
(def-sce-method geometry "GetNormalsArray" scegeometryarray)
(def-sce-method geometry "GetTexCoordsArray" scegeometryarray)
(def-sce-method geometry "GetPositions" scevertices)
(def-sce-method geometry "GetNormals" scevertices)
(def-sce-method geometry "GetTexCoords" scevertices)
(def-sce-method geometry "GetIndices" sceindices)

(defsetter geometry "SetPrimitiveType"
  (prim sceeprimitivetype))
(def-sce-method geometry "GetPrimitiveType" sceeprimitivetype)

(defsetter geometry "SetNumVertices"
  (n-vertices :unsigned-int))
(defsetter geometry "SetNumIndices"
  (n-indices :unsigned-int))

(def-sce-method geometry "GetNumVertices" :unsigned-int)
(def-sce-method geometry "GetNumIndices" :unsigned-int)
(def-sce-method geometry "GetNumVerticesPerPrimitive" :unsigned-int)
(def-sce-method geometry "GetNumPrimitives" :unsigned-int)

;; TODO; GetArrays, GetModifiedArrays

(def-sce-method geometry "IsModified" :int)

(defcfun "SCE_Geometry_Load" scegeometry
  (fname :string)
  (force :int))

;; TODO: bounding box/spheres etc.

;;; BoxGeometry
(defcenum sceeboxgeomtexcoordmode
  :box-none-texcoord
  :box-interior-texcoord
  :box-exterior-texcoord
  :box-cubemap-texcoord)