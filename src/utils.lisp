(in-package :scelisp)

(defun scetype (name)
  (symbolicate 'sce name))

(defmacro defobject (name)
  (let ((typename (scetype name)))
    `(progn
       (defctype ,typename :pointer)
       (defcfun ,(format nil "SCE_~@(~a~)_Create" name) ,typename)
       (defcfun ,(format nil "SCE_~@(~a~)_Delete" name) :void
         (,name ,typename)))))

(defmacro def-sce-method (object name return-type &rest args)
  `(defcfun ,(format nil "SCE_~@(~a~)_~a" object name)
       ,return-type
     (,object ,(scetype object))
     ,@args))

(defmacro defsetter (object name &rest args)
  `(def-sce-method ,object ,name :void ,@args))


(defmacro defprop (object name type)
  `(progn
     (defsetter ,object ,(format nil "Set~a" name)
       (value ,type))
     (def-sce-method ,object ,(format nil "Get~a" name)
       ,type)))

;; Not sure about the usefulness right now, but let's keep it
(defmacro defstatus (object name)
  `(progn
     (defsetter ,object ,name
       (status scebool))
     (def-sce-method ,object ,(format nil "Is~ad" name)
       scebool)))

(defun get-constructor (object)
  (symbolicate 'sce- object '-create))

(defun get-destructor (object)
  (symbolicate 'sce- object '-delete))

(defmacro with-object ((name object) &body body)
  `(let ((,name (,(get-constructor object))))
     (unwind-protect
          (progn ,@body)
       (,(get-destructor object) ,name))))

(defmacro with-objects (bindings &body body)
  (if bindings
      `(with-object ,(first bindings)
         (with-objects ,(rest bindings)
           ,@body))
       `(progn ,@body)))
