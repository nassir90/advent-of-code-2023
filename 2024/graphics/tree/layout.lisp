(defpackage :aoc-2024/graphics/tree/layout
  (:use #:cl #:aoc-2024/dsa/tree)
  (:export #:layout #:layout-left #:layout-left-root #:layout-width #:layout-right #:layout-right-root #:layout-span #:layout-render-root #:width-for-layout
           ;; things to make private later
           #:node-height #:layout-middle #:margin-y)
  (:import-from #:aoc-2024/dsa/tree))

(in-package :aoc-2024/graphics/tree/layout)

(defclass layout ()
  ((left :initarg :left :accessor layout-left)
   (left-root :initarg :left-root :reader layout-left-root)
   (width :initarg :width :reader layout-width)
   (right :initarg :right :reader layout-right)
   (right-root :initarg :right-root :reader layout-right-root)
   (span :initarg :span :reader layout-span)
   (render-root :initarg :render-root :reader layout-render-root)))

(defgeneric width-for-layout (object)
  (:documentation "Get the width for an object"))

(defparameter margin-x 10)
(defparameter margin-y 60)
(defparameter node-height 30)

(defun layout-middle (layout)
  (declare (type layout layout))
  (+ (layout-render-root layout) (round (layout-width layout) 2)))

(defun layout (tree)
  "We are interested in taking children from their untransformed coordinate spaces, 
centering or shifting them, and from those transformations we want to compute the
vector from the left of the span to the center and other similar vectors"
  (declare (type b-tree tree))
  (let* ((node-width (width-for-layout (b-value tree)))
         (node-middle (round node-width 2)))
    (cond ((b-no-kids tree)
           (make-instance 'layout :width node-width :span node-width :render-root 0))
          ((and (b-left tree) (b-right tree))
           (let* ((left (layout (b-left tree)))
                  (right (layout (b-right tree)))
                  (left-root 0)
                  (left-middle (layout-middle left))
                  (right-root (+ (layout-span left) margin-x))
                  (right-middle (+ right-root (layout-middle right)))
                  (overall-middle (round (+ left-middle right-middle) 2))
                  (rootward-vector (- node-middle overall-middle))
                  (span-start (min 0 (+ left-root rootward-vector)))
                  (span-end (max node-width (+ right-root (layout-span right) rootward-vector))))
             (make-instance 'layout
                            :width node-width
                            :span (- span-end span-start)
                            :render-root (- span-start)
                            :left left :left-root left-root
                            :right right :right-root right-root)))
          (t
           (let* ((child (layout (or (b-left tree) (b-right tree))))
                  (rootward-vector (- node-middle (layout-middle child)))
                  (offset-rootward-vector (funcall (if (b-left tree) #'- #'+) rootward-vector margin-x))
                  (child-root 0)
                  (span-start (min 0 (+ child-root offset-rootward-vector)))
                  (span-end (max node-width (+ (layout-span child) offset-rootward-vector))))
             (make-instance 'layout
                            :width node-width
                            :span (- span-end span-start)
                            :render-root (- span-start)
                            :left (when (b-left tree) child) :left-root (max 0 (+ child-root offset-rootward-vector))
                            :right (when (b-right tree) child) :right-root (max 0 (+ child-root offset-rootward-vector))))))))
