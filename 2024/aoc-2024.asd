(defsystem #:aoc-2024
  :serial t
  :author "Chinaza Uzoukwu <uzoukwuc@tcd.ie>"
  :description "Common lisp advent of code solutions for 2024"
  :depends-on (:cl-ppcre :str :uiop :cl-interpol :alexandria :named-readtables)
  :components ((:file "aoc-2024")
               (:file "font")
               (:module "dsa"
                :components ((:file "tree")))
               (:module "graphics"
                :components ((:module "tree"
                              :components ((:file "layout")))))
               (:module "09"
                :components ((:file "2")))))
