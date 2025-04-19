(defpackage :aoc-2024/dsa/tree
  (:use #:cl #:aoc-2024 #:lem #:alexandria)
  (:export :b-tree :b 
           :b-left :b-right :b-value :b-height
           :b-< :b-> :b-=
           :b-insert :b-no-kids))

(in-package :aoc-2024/dsa/tree)

#| Initiial section defining accessor methods and tree data schema |#

(defclass b-tree ()
  ((left :initarg :left :initform nil :accessor b-left)
   (right :initarg :right :initform nil :accessor b-right)
   (value :initarg :value :accessor b-value)
   (color :initarg :color :accessor b-color :initform '(255 255 255))
   (height :initform 1 :accessor b-height)))
(defun b-no-kids (b-tree)
  (not (or (b-left b-tree) (b-right b-tree))))

(defun b (value &optional left right)
  (b-with-fixed-heights (make-instance 'b-tree :value value :left left :right right)))

(defun b-height-left (b-tree)
  (declare (type b-tree b-tree))
  (if (b-left b-tree) (b-height (b-left b-tree)) 0))

(defun b-height-right (b-tree)
  (declare (type b-tree b-tree))
  (if (b-right b-tree) (b-height (b-right b-tree)) 0))

(defun b-with-recursively-fixed-heights (b-tree)
  (declare (type b-tree tree))
  (when (b-left b-tree)
    (setf (b-left b-tree) (b-with-recursively-fixed-heights (b-left b-tree))))
  (when (b-right b-tree)
    (setf (b-right b-tree) (b-with-recursively-fixed-heights (b-right b-tree))))
  (b-with-fixed-heights b-tree))

(defun b-with-fixed-heights (b-tree)
  "Assuming that the children of the b-tree have heights configured
correctly, configure the heights of this b-tree."
  (declare (type (or null b-tree) b-tree))
  (when b-tree
    (setf (b-height b-tree)
          (1+ (max (b-height-left b-tree)
                   (b-height-right b-tree))))
    (the b-tree b-tree)))

#| Necessary logic for b tree insertion |#

(defgeneric b-< (a b))
(defgeneric b-> (a b))
(defgeneric b-= (a b))

(defmethod b-< ((a number) (b number)) (< a b))
(defmethod b-> ((a number) (b number)) (> a b))
(defmethod b-= ((a number) (b number)) (= a b))

(defun b-insert (tree newlet)
  (declare (type (or null b-tree) tree))
  (b-balance
   (b-with-fixed-heights
    (if tree
        (with-slots (left right value) tree
          (cond ((b-< newlet value)
                 (b value (b-insert left newlet) right))
                ((b-> newlet value)
                 (b value left (b-insert right newlet)))
                ((b-= newlet value)
                 (b newlet left right))))
        (b newlet)))))

#| Code for doing rotations and preserving heights after rotations have been performed |#

(defun b-left-rotate (tree)
  (b (b-value (b-right tree))
     (b (b-value tree)
        (b-left tree)
        (b-left (b-right tree)))
     (b-right (b-right tree))))

(defun b-right-rotate (tree)
  (b (b-value (b-left tree))
     (b-left (b-left tree))
     (b (b-value tree)
        (b-right (b-left tree))
        (b-right tree))))

#| Utility functions for querying and understanding tree balance factors and performing rebalancing |#

(defun b-balance-factor (b-tree)
  "The balance factor is the height of the left subtree minus the height of the
right subtree. Thus, it is positive when there is weight on the left side and negative
when there is weight on the right side"
  (if b-tree (- (b-height-left b-tree) (b-height-right b-tree)) 0))

(defun b-left-heavy (b-tree)
  (< 0 (b-balance-factor b-tree)))

(defun b-left-overweight (b-tree)
  (< 1 (b-balance-factor b-tree)))

(defun b-right-heavy (b-tree)
  (< (b-balance-factor b-tree) 0))

(defun b-right-overweight (b-tree)
  (< (b-balance-factor b-tree) (- 1)))

(defun b-left-right-rotate (b-tree)
  "Move left inner weight to the extremities and then right rotate"
  (declare (type b-tree b-tree))
  (b-right-rotate
   (b (b-value b-tree)
      (b-left-rotate (b-left b-tree))
      (b-right b-tree))))

(defun b-right-left-rotate (b-tree)
  "Move right inner weight to the extremities and then left rotate"
  (declare (type b-tree b-tree))
  (b-left-rotate
   (b (b-value b-tree)
      (b-left b-tree)
      (b-right-rotate (b-right b-tree)))))

(defun b-balance (tree)
  "Balance the tree after a single mutation. This function is not guaranteed to
work if you have done more than one operation on the tree. It might be too fucked up
in that case."
  (declare (type (or null b-tree) tree))
  (when tree
    (cond ((b-left-overweight tree)
         (if (b-right-heavy (b-left tree))
             (b-left-right-rotate tree)
             (b-right-rotate tree)))
        ((b-right-overweight tree)
         (if (b-left-heavy (b-right tree))
             (b-right-left-rotate tree)
             (b-left-rotate tree)))
        (t tree))))

#| Code for deleting element from trees |#

(defun b-remove-rightmost (tree)
  "Basically, return two values, the first being the result oft he tree after the removal
and the second value being the value of the node that was deleted"
  (declare (type b-tree tree))
  (if (b-right tree)
      (multiple-value-bind (right-after-removal removed) (b-remove-rightmost (b-right tree))
        (values
         (b-balance (b (b-value tree)
                       (b-left tree)
                       right-after-removal))
         removed))
      (values (b-left tree) (b-value tree))))

(defun b-remove-leftmost (tree)
  "Basically, return two values, the first being the result oft he tree after the removal
and the second value being the value of the node that was deleted"
  (declare (type b-tree tree))
  (if (b-left tree)
      (multiple-value-bind (left-after-removal removed) (b-remove-leftmost (b-left tree))
        (values
         (b-balance (b (b-value tree)
                       left-after-removal
                       (b-right tree)))
         removed))
      (values (b-right tree) (b-value tree))))

(defun b-remove (tree target)
  "Either defer the removal to a subnode or remove from the current node. Preserves balance"
  (declare (type (or null b-tree) b-tree))
  (when tree
    (with-slots (value left right)
        (b-balance
         (cond
           ((b-< target value)
            (b value (b-remove left target) right))
           ((b-< value target)
            (b value left (b-remove right target)))
           ((b-= target value)
            (cond
              ((b-no-kids tree) nil)
              ((and left right)
               (multiple-value-bind (right-after-removal successor) (b-remove-leftmost right)
                 (b successor
                    left
                    right-after-removal)))
              (t (or left right)))))))))