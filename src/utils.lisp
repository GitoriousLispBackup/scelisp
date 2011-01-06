(in-package :scelisp)

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *types* (make-hash-table :test 'eq))
  (defvar *types-properties* (make-hash-table :test 'eq)))

(defun scetype-string (name)
  (let ((str (gethash name *types*)))
    (if str
        str
        (format nil "~@(~a~)" name))))

(defun scetype (name)
  (symbolicate 'sce name))

(defmacro defobject (name &optional (c-name nil))
  (when c-name
    (setf (gethash name *types*) c-name))
  (let ((typename (scetype name))
        (string-name (scetype-string name)))
    `(progn
       (defctype ,typename :pointer)
       (defcfun ,(format nil "SCE_~a_Create" string-name) ,typename)
       (defcfun ,(format nil "SCE_~a_Delete" string-name) :void
         (,name ,typename)))))

(defmacro def-sce-method (object name return-type &rest args)
  `(defcfun ,(format nil "SCE_~a_~a" (scetype-string object) name)
       ,return-type
     (,object ,(scetype object))
     ,@args))

(defmacro defsetter (object name &rest args)
  `(def-sce-method ,object ,name :void ,@args))

(defun addprop (object name)
  (setf #1=(gethash object *types-properties*)
        (union (list (symbolicate (string-upcase name))) #1#)))

(defmacro defprop (object name type)
  (addprop object name)
  `(progn
     (defsetter ,object ,(format nil "Set~a" name)
       (value ,type))
     (def-sce-method ,object ,(format nil "Get~a" name)
       ,type)))

;; Not sure about the usefulness right now, but let's keep it
(defmacro defstatus (object name)
  (addprop object name)
  `(progn
     (defsetter ,object ,name
       (status scebool))
     (def-sce-method ,object ,(format nil "Is~ad" name)
       scebool)))

;; TODO: seperate status from properties
(defmacro defconstructor (object)
  (let ((properties (gethash object *types-properties*)))
    `(defun ,(symbolicate 'make- object)
         (&key ,@(mapcar (lambda (prop)
                           (list prop nil (symbolicate prop '-supplied)))
                         properties))
       (let ((object (funcall ',(symbolicate 'sce- object '-create))))
         ,@(mapcar (lambda (prop)
                     `(when ,(symbolicate prop '-supplied)
                        (apply ',(symbolicate 'sce- object '-set prop)
                               (cons object
                                     (if (listp ,prop)
                                         ,prop
                                         (list ,prop))))))
                   properties)
         object))))

(defun get-constructor (object)
  (symbolicate 'make- object))

(defun get-destructor (object)
  (symbolicate 'sce- object '-delete))

(defmacro defwith (name specs &body body)
  (let* ((with-name (symbolicate 'with- name))
         (with-names (symbolicate with-name 's)))
    `(progn
       (defmacro ,with-name (,specs &body body)
         ,@body)
       (defmacro ,with-names (bindings &body body)
         (if bindings
             `(,',with-name ,(first bindings)
                (,',with-names ,(rest bindings)
                  ,@body))
             `(progn ,@body))))))

(defwith object (name object &rest args)
  `(let ((,name (funcall ',(get-constructor object)
                         ,@args)))
     (unwind-protect
          (progn ,@body)
       (,(get-destructor object) ,name))))

#|(defmacro with-object ((name object &rest args) &body body)
  `(let ((,name (funcall ',(get-constructor object)
                       ,@args)))
     (unwind-protect
          (progn ,@body)
       (,(get-destructor object) ,name))))|#

#|(defmacro with-objects (bindings &body body)
  (if bindings
      `(with-object ,(first bindings)
         (with-objects ,(rest bindings)
           ,@body))
       `(progn ,@body)))|#
