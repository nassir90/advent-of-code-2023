(defpackage :aoc-2024/dsa/tree
  (:use #:cl #:aoc-2024 #:lem #:alexandria)
  (:export :b-tree :b 
           :b-left :b-right :b-value :b-height
           :b-< :b-> :b-=
           :b-insert :b-no-kids))

(in-package :aoc-2024/dsa/tree)

(defclass b-tree ()
  ((left :initarg :left :initform nil :accessor b-left)
   (right :initarg :right :initform nil :accessor b-right)
   (value :initarg :value :accessor b-value)
   (color :initarg :color :accessor b-color :initform '(255 255 255))
   (height :initform 1 :accessor b-height)))

(defgeneric b-< (a b))
(defgeneric b-> (a b))
(defgeneric b-= (a b))

(defmethod b-< ((a number) (b number)) (< a b))
(defmethod b-> ((a number) (b number)) (> a b))
(defmethod b-= ((a number) (b number)) (= a b))

(defun b-no-kids (b-tree)
  (not (or (b-left b-tree) (b-right b-tree))))

(defun b (value &optional left right)
  (make-instance 'b-tree :value value :left left :right right))

(defun b-with-fixed-heights (b-tree)
  "Assuming that the children of the b-tree have heights configured
correctly, configure the heights of this b-tree."
  (declare (type b-tree b-tree))
  (setf
   (b-height b-tree)
   (1+ (max (if (b-left b-tree) (b-height (b-left b-tree)) 0)
            (if (b-right b-tree) (b-height (b-right b-tree)) 0))))
  (the b-tree b-tree))

(defun b-insert (b-tree newlet)
  ;; In the first phase we modify
  (b-with-fixed-heights
   (if b-tree
       (with-slots (left right value) b-tree
         (cond ((b-< newlet value)
                (b value (b-insert left newlet) right))
               ((b-> newlet value)
                (b value left (b-insert right newlet)))
               ((b-= newlet value)
                (b newlet left right))))
       (b newlet))))