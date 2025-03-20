(defsystem #:aoc-2024
  :serial t
  :depends-on (:cl-ppcre :str :uiop :cl-interpol)
  :components ((:file "aoc-2024")
               (:module "09"
                :components ((:file "2")))))
