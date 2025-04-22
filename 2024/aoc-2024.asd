(defsystem #:aoc-2024
  :serial t
  :depends-on (:cl-ppcre :str :uiop :cl-interpol :alexandria)
  :author "Chinaza Uzoukwu <uzoukwuc@tcd.ie>"
  :description "Common lisp advent of code solutions for 2024"
  :components ((:file "aoc-2024")
               (:module "dsa"
                :components ((:file "tree")))
               (:module "09"
                :components ((:file "2")))))
