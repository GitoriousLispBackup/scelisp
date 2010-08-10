(asdf:defsystem scelisp
  :depends-on (:cffi)
  :license "MIT"
  :description "SCEngine Common Lisp bindings"
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "sceutils")
             (:file "sceinterface")))))