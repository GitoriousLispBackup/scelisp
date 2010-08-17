(asdf:defsystem scelisp
  :depends-on (:cffi :alexandria)
  :license "MIT"
  :description "SCEngine Common Lisp bindings"
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "utils")
             (:file "types")
             (:file "sceutils")
             (:file "scecore")
             (:file "sceinterface")))))