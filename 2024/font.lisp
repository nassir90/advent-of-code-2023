(defpackage :aoc-2024/font
  (:use #:cl)
  (:export :load-font :current-font-size))

(in-package :aoc-2024/font)

(defun load-font ()
  (sdl2-ttf:open-font
   (lem-sdl2/resource:get-resource-pathname
    "resources/fonts/NotoSansMono-Regular.ttf")
   (current-font-size)))

(defun current-font-size ()
  (lem-sdl2/font:font-config-size
   (lem-sdl2/display:display-font-config
    (lem-sdl2/display:current-display))))
