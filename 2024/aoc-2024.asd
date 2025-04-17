(defsystem #:aoc-2024
  :serial t
  :depends-on (:cl-ppcre :str :uiop :cl-interpol :alexandria)
  :components ((:file "aoc-2024")
               (:module "dsa"
                :components ((:file "tree")))
               (:module "09"
                :components ((:file "2")))))
