(defpackage :aoc-2024/day-09
  (:use #:cl #:aoc-2024 #:lem))

(in-package :aoc-2024/day-09)

(ql:quickload "str")
(ql:quickload "uiop")

(defclass b-tree ()
  ((left :initarg :left :initform nil :accessor b-tree-left)
   (right :initarg :right :initform nil :accessor b-tree-right)
   (value :initarg :value :accessor b-tree-value)))

(defgeneric b-< (a b))
(defgeneric b-> (a b))
(defgeneric b-= (a b))

(defmethod b-< ((a number) (b number)) (< a b))
(defmethod b-> ((a number) (b number)) (> a b))
(defmethod b-= ((a number) (b number)) (= a b))

(defun b-left (b-tree)
  (slot-value b-tree 'left))

(defun b-right (b-tree)
  (slot-value b-tree 'right))

(defun b-no-kids (b-tree)
  (with-slots (left right) b-tree
    (not (or left right))))

(defun b-right-only (b-tree)
  (with-slots (left right) b-tree
    (and (not left) right)))

(defun b-left-only (b-tree)
  (with-slots (left right) b-tree
    (and left (not right))))

(defun b-insert (b-tree newlet)
    (if b-tree
        (with-slots (left right value) b-tree
        (cond ((b-< newlet value) (make-instance 'b-tree
                                               :left (b-insert left newlet)
                                               :right right
                                               :value value))
              ((b-> newlet value) (make-instance 'b-tree
                                               :left left
                                               :right (b-insert right newlet)
                                               :value value))
              (b-= newlet value) (make-instance 'b-tree
                                              :left left
                                              :right right
                                              :value value)))
      (make-instance 'b-tree :value newlet)))

(defvar example-tree-0 '(20))
(defvar example-tree-1 '(5 (-10) (300)))

(defmacro vx (v) `(aref ,v 0))
(defmacro vy (v) `(aref ,v 0))

(defvar width 20)
(defvar half-width (round width 2))
(defvar childless-offset 20)

(defclass layout ()
  ((margin-y :initarg :margin-y :reader layout-margin-y)))

(defclass lp1 ()
  ((left :initarg :left :accessor lp1-left)
   (right :initarg :right :accessor lp1-right)
   (span :initarg :span :accessor lp1-span)
   (root :initarg :root :accessor lp1-root)))

(defun lp1 (tree-object)
  (let ((root half-width))
    (cond  ((and (b-left tree-object) (b-right tree-object))
            (let* (;; First of all just correctly position the children
                   (left (lp1 (b-left tree-object)))
                   (right (lp1 (b-right tree-object)))
                   ;; We need to define a transform. The steps are as follows
                   ;;
                   ;; For the left side
                   ;;
                   ;; - Do nothing
                   ;;
                   ;; For the right side
                   ;; 
                   ;; - Add span plus horizontal margin
                   ;;
                   ;; Now we compute the root positions and we compute the midpoint
                   ;;
                   ;; - Center on midpoint
                   )
              (flet ((pre-transform-left (x) x)
                     (pre-transform-right (x) (+ x (lp1-span left) width)))
                (let* (;; Compute the midpoint between the roots for placing the spans
                       (midpoint (/ (+ (pre-transform-left (lp1-root left))
                                       (pre-transform-right (lp1-root right)))
                                    2))
                       ;; Compute the vector from the span middle to the root middle
                       (rootward-vector (- root midpoint)))
                  (flet (;; Factor in pre transforms into correct span space and then the
                         ;; subsequent centering on the root midpoint
                         (transform-left (x) (+ (pre-transform-left x) rootward-vector))
                         (transform-right (x) (+ (pre-transform-right x) rootward-vector)))
                    (let* ((s-start (min (transform-left 0) 0))
                           (s-end (max (transform-right (lp1-span right)) width))
                           (span (- s-end s-start))
                           (new-root (- root s-start)))
                     (make-instance 'lp1
                                   :left (list (transform-left (lp1-root left)) left)
                                   :right (list (transform-right (lp1-root right)) right)
                                   :span span
                                   :root new-root)))))))
           ((b-no-kids tree-object)
           (make-instance 'lp1
                          :left nil
                          :right nil
                          :span width
                          :root root))
           ;; In this case there's only one kid
           (t (let* (;; The root is the horizontal distance from the left hand side of the span to the
                  ;; center at the top. The coordinate is then given by (x=root, y=0).
                  (lp1-child (lp1 (b-right tree-object)))
                  ;; We need to compute our own span. This is the full width that we take up after
                  ;; positioning. Our children correctly. To translate any point on a child to its
                  ;; "correct" position, you can:
                  ;;
                  ;; - Add the y margin on the y axis (20 pixels)
                  ;; - Center it on the root by adding the rootward vector (root - subject)
                  ;; - Shift it right by an amount equal to the "childless offset"
                  ;;
                  ;; Since the y offset can be inferred from the recursion depth, its not really
                  ;; needed.
                  (root-ward-vector (- root (lp1-root lp1-child))))
             (flet (;; The definition of the transform we discussed above can be captured in a local function.
                    (transform-x (x) (+ childless-offset (+ root-ward-vector x))))
              (let* (;; For the final result we need to store the relative position of the child anchor /
                     ;; root, so we'll just do it here
                     (transformed-child-root (transform-x (lp1-root lp1-child)))
                     ;; For the start we take the our own origin at zero and the transformed
                     ;; zero origin.
                     (s-start (min (transform-x 0) 0))
                     ;; For the end we just take the max of our width and the transformed endpoint
                     ;; which is just the transformation of the span.
                     (s-end (max (transform-x (lp1-span lp1-child)) width))
                     ;; Now to compute the new span we just compute the range assuming that
                     ;; s-end > s-start (all of maths is fucking infix)
                     (span (progn
                          (assert (>= s-end s-start))
                          (- s-end s-start)))
                     ;; For the root, we just compute the delta from the left hand side of the span
                     ;; to the previous proper root which we already know. This only really changes
                     ;; if there are components to the left. It looks like a vector pointing from the
                     ;; left to the middle, this it takes the form (middle - left)
                     (new-root (- root s-start)))
                (format t "end is ~a~%" s-end)
                (make-instance 'lp1
                            :left nil
                            :right (list transformed-child-root lp1-child)
                            :span span
                            :root new-root))))))))

(let ((kz (lp1 (b-insert (b-insert (b-insert nil 1) 2) 3))))
  kz)
(let ((kz (lp1 (b-insert (b-insert (b-insert nil 1) 2) (- 3)))))
  kz)

;; (let ((layout (make-instance 'layout :margin-y 10)))
;;   ;; (lp1 layout )
;;   )

;; (let* ((left-root-x (get-root-x (get-left-of tree-object)))
;;        (right-root-x (+ (get-span-size-x (get-left-of tree-object)) (get-root-x (get-right-of tree-object))))
;;        (root-middle (/ (+ left-root-x right-root-x) 2))
;;        ))

(defclass renderable-b-tree ()
  ((inner :initarg :inner :reader renderable-b-tree-inner)
   (x :initarg :x :reader renderable-b-tree-x)
   (y :initarg :y :reader renderable-b-tree-y)
   (width :initarg :span :reader renderable-b-tree-span)
   (height )
   (root :initarg :surface :reader renderable-b-tree-surface)))

(defun position-self-and-children (current-x b-tree)
  (cond ((not (or (b-tree-left b-tree) (b-tree-right b-tree)))
         ;; Fill this in bredrin
         )))

;; ---

(defclass naza-graphical-buffer (text-buffer)
  ())

(lem:define-major-mode naza-graphical-mode ()
    (:name "Graphical Mode"
     :keymap *naza-graphical-mode*)
  ;; (setf (lem:buffer-read-only-p (lem:current-buffer)) t)
  )

(lem:define-command kill-current-buffer () ()
  (lem:delete-buffer (lem:current-buffer)))

(lem:define-key *naza-graphical-mode* "q" 'kill-current-buffer)

(lem:define-command naza-graphical-mode-start () ()
  (let* ((name "naza-graphical-buffer")
         (buffer (or (lem:get-buffer name)
                     (lem/buffer/internal:make-buffer name)
                     (error "what the fuck"))))
    (change-class buffer 'naza-graphical-buffer)
    (change-buffer-mode buffer 'naza-graphical-mode)
    (lem:pop-to-buffer buffer)))

(defclass entity ()
  ((surface :initarg :surface
            :accessor entity-surface)
   (texture :initarg :texture
            :accessor entity-texture)))

(defun make-entity ()
  (let* ((surface (sdl2-ttf:render-utf8-blended (load-font) "some text" 255 255 255 0))
         (texture (sdl2:create-texture-from-surface (lem-sdl2:current-renderer) surface)))
    (make-instance 'entity :surface surface :texture texture)))

(setf entity nil)

(defun destroy-entity (entity)
  (with-slots (texture surface) entity
    (sdl2:destroy-texture texture)
    (sdl2:free-surface surface)))

(defvar cumbersome nil)

;; We override the render function for our custom buffer type
(defmethod lem-sdl2:render (texture window (buffer naza-graphical-buffer))
  (let ((renderer (lem-sdl2:current-renderer)))
    (sdl2:set-render-target renderer texture)
    (let ((sinusoidal-x (round  (+ 100 (* 50 (sin  (/ (get-internal-real-time) 1000000))) ))))
      (unless entity
        (setf entity (make-entity)))
      (sdl2:with-rects ((dest 100 sinusoidal-x (sdl2:surface-width (entity-surface entity)) (sdl2:surface-height (entity-surface entity))))
        (sdl2:render-copy renderer (entity-texture entity) :dest-rect dest))
      (sdl2:with-rects ((dest 100 sinusoidal-x 100 100))
        (sdl2:set-render-draw-color renderer 255 255 255 0)
        (sdl2:render-draw-rect renderer dest)))
    ;; (call-next-method)
    ))

(defun load-font ()
  (sdl2-ttf:open-font
   (lem-sdl2/resource:get-resource-pathname
    "resources/fonts/NotoSansMono-Regular.ttf")
   (lem-sdl2/font:font-config-size
    (lem-sdl2/display:display-font-config
     (lem-sdl2/display:current-display)))))