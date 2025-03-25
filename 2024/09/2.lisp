(defpackage :aoc-2024/day-09
  (:use #:cl #:aoc-2024 #:lem))

(in-package :aoc-2024/day-09)

(defun get/create-buffer (name)
  (or (lem:get-buffer name)
      (lem/buffer/internal:make-buffer name)
      (error "failed to execute get or create operation")))

(defun load-font ()
  (sdl2-ttf:open-font
   (lem-sdl2/resource:get-resource-pathname
    "resources/fonts/NotoSansMono-Regular.ttf")
   (lem-sdl2/font:font-config-size
    (lem-sdl2/display:display-font-config
     (lem-sdl2/display:current-display)))))

(defclass b-tree ()
  ((left :initarg :left :initform nil :accessor b-left)
   (right :initarg :right :initform nil :accessor b-right)
   (value :initarg :value :accessor b-value)))

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

(defun b-insert (b-tree newlet)
  (if b-tree
      (with-slots (left right value) b-tree
        (cond ((b-< newlet value) (b value (b-insert left newlet) right))
              ((b-> newlet value) (b value left (b-insert right newlet)))
              ((b-= newlet value) (b newlet left right))))
      (b newlet)))

(defmacro vx (v) `(aref ,v 0))
(defmacro vy (v) `(aref ,v 1))

(defclass lp1 ()
  ((left :initarg :left :accessor lp1-left)
   (left-root :initarg :left-root :reader lp1-left-root)
   (width :initarg :width :reader lp1-width)
   (right :initarg :right :reader lp1-right)
   (right-root :initarg :right-root :reader lp1-right-root)
   (span :initarg :span :reader lp1-span)
   (root :initarg :root :reader lp1-root)
   (render-root :initarg :render-root :reader lp1-render-root)))

(defgeneric width-for-lp1 (object)
  (:documentation "Get the width for an object"))

(defvar margin-x 10)
(defvar margin-y 60)
(defvar node-height 30)

(defun lp1-middle (lp1)
  (declare (type lp1 lp1))
  (+ (lp1-render-root lp1) (round (lp1-width lp1) 2)))

(defun lp1-v2 (tree)
  (declare (type b-tree tree))
  (let* ((node-width (width-for-lp1 (b-value tree)))
         (node-middle (round node-width 2)))
    (cond ((b-no-kids tree)
           (make-instance 'lp1 :width node-width :span node-width :render-root 0))
          ((and (b-left tree) (b-right tree))
           (let* ((left (lp1-v2 (b-left tree)))
                  (right (lp1-v2 (b-right tree)))
                  (left-root 0)
                  (left-middle (lp1-middle left))
                  (right-root (+ (lp1-span left) margin-x))
                  (right-middle (+ right-root (lp1-middle right)))
                  (overall-middle (round (+ left-middle right-middle) 2))
                  (rootward-vector (- node-middle overall-middle))
                  (span-start (min 0 (+ left-root rootward-vector)))
                  (span-end (max node-width (+ right-root (lp1-span right) rootward-vector))))
             (make-instance 'lp1
                            :width node-width
                            :span (- span-end span-start)
                            :render-root (- span-start)
                            :left left :left-root left-root
                            :right right :right-root right-root)))
          (t
           (let* ((child (lp1-v2 (or (b-left tree) (b-right tree))))
                  (rootward-vector (- node-middle (lp1-middle child)))
                  (offset-rootward-vector (funcall (if (b-left tree) #'- #'+) rootward-vector margin-x))
                  (child-root 0)
                  (span-start (min 0 (+ child-root offset-rootward-vector)))
                  (span-end (max node-width (+ (lp1-span child) offset-rootward-vector))))
             (make-instance 'lp1
                            :width node-width
                            :span (- span-end span-start)
                            :render-root (- span-start)
                            :left (when (b-left tree) child) :left-root (max 0 (+ child-root offset-rootward-vector))
                            :right (when (b-right tree) child) :right-root (min 0 (+ child-root offset-rootward-vector))))))))

(lp1-v2 (b-tree-map-to-entity kerberos-b-tree))

(defun lp1 (tree-object)
  (when tree-object
    (let* ((width (width-for-lp1 (b-value tree-object)))
           (half-width (round width 2)))
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
                     (pre-transform-right (x) (+ x (lp1-span left) margin-x)))
                (let* (;; Compute the midpoint between the roots for placing the spans
                       (midpoint (round (+ (pre-transform-left (lp1-root left))
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
                           (new-root (- root s-start))
                           (span-space-corner-root (- s-start)))
                      (make-instance 'lp1
                                     ;; The concept behind this here is that since our leftmost childis always right up
                                     ;; against the edge we never actually need to apply any offset to the leftmost span
                                     ;; on the contrary, we have to add the left child span plus our margin in order to get
                                     ;; the "render origin" for the right one.
                                     :left left :left-root 0
                                     :right right :right-root (+ (lp1-span left) margin-x)
                                     :span span
                                     :root new-root
                                     :width width
                                     :render-root span-space-corner-root)))))))
           ((b-no-kids tree-object)
            (make-instance 'lp1
                           :span width
                           :root root
                           :width width
                           :render-root 0))
           ;; In this case there's only one kid
           (t (flet ((child () (or (b-left tree-object) (b-right tree-object)))
                     (childless-offset (x) (+ x (if (b-left tree-object) (- 20) 20))))
                (let* (;; The root is the horizontal distance from the left hand side of the span to the
                       ;; center at the top. The coordinate is then given by (x=root, y=0).
                       (lp1-child (lp1 (child)))
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
                         (transform-x (x) (childless-offset (+ root-ward-vector x))))
                    (let* (;; For the final result we need to store the relative position of the child anchor /
                           ;; root, so we'll just do it here
                           (transformed-child-root (transform-x 0))
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
                      (destructuring-bind (side side-root)
                          (if (b-left tree-object) '(:left :left-root) '(:right :right-root))
                        (make-instance 'lp1
                                       side lp1-child
                                       side-root transformed-child-root
                                       :span span
                                       :root new-root
                                       :width width
                                       :render-root (- s-start)))))))))))))

(defvar kerberos-mapped-b-tree nil)
(defvar kerberos-b-tree nil)

(define-command wipe-naza-graphical-buffer-cache () ()
  (setf kerberos-mapped-b-tree nil))

(defun bind-lp1-with-b-tree (b-tree)
  (declare (type b-tree b-tree))
  (wipe-naza-graphical-buffer-cache)
  (setf kerberos-b-tree b-tree))

(bind-lp1-with-b-tree (b-insert (b-insert (b-insert nil 1) 2) 3))
(bind-lp1-with-b-tree (b-insert (b-insert (b-insert nil 3) 2) 1))
(bind-lp1-with-b-tree (b-insert (b-insert (b-insert nil 1) 2) (- 3)))
(bind-lp1-with-b-tree (b 5
                         (b 1)
                         (b 50
                            (b "love's gonna get you killed")
                            (b 65
                               (b (b "but pride's gonna be the death of"
                                     "you"
                                     (b "and me"
                                        (b "and you")
                                        (b "and me"))))))))

;; ---

(defclass naza-graphical-buffer (text-buffer) ())

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
  ((surface :initarg :surface :accessor entity-surface)
   (texture :initarg :texture :accessor entity-texture)))

(defmethod width-for-lp1 ((entity entity))
  (sdl2:surface-width (entity-surface entity)))

(defun make-entity (value)
  (let* ((surface (sdl2-ttf:render-utf8-blended (load-font) (format nil " ~a " value) 255 255 255 0))
         (texture (sdl2:create-texture-from-surface (lem-sdl2:current-renderer) surface)))
    (make-instance 'entity :surface surface :texture texture)))

(defun b-tree-map (b-tree function)
  (declare (type b-tree b-tree))
  (b (funcall function (b-value b-tree))
     (and (b-left b-tree) (b-tree-map (b-left b-tree) function))
     (and (b-right b-tree) (b-tree-map (b-right b-tree) function))))

(defun b-tree-map-to-entity (b-tree)
  (declare (type b-tree b-tree))
  (b-tree-map b-tree #'make-entity))

(defun log-it (buffer-name k)
  (declare (type string buffer-name))
  (declare (type string k))
  (let ((buffer (get/create-buffer buffer-name)))
    (with-current-buffer buffer
      (insert-string (buffer-end-point buffer) k))))
  
(defun recursively-render-entity-mapped-b-tree (mapped-b-tree lp1 origin)
  (declare (type b-tree mapped-b-tree)
           (type lp1 lp1)
           (type vector origin))
  (sdl2:set-render-draw-color (lem-sdl2:current-renderer) 255 255 255 0)
  (sdl2:with-rects ((dest-rect (+ (vx origin) (lp1-render-root lp1)) (vy origin)
                               (width-for-lp1 (b-value mapped-b-tree)) node-height))
    (sdl2:render-copy (lem-sdl2:current-renderer) (entity-texture (b-value mapped-b-tree)) :dest-rect dest-rect)
    (sdl2:render-draw-rect (lem-sdl2:current-renderer) dest-rect))
  (when (b-left mapped-b-tree)
    (sdl2:render-draw-line (lem-sdl2:current-renderer)
                           (+ (vx origin) (lp1-middle lp1))
                           (+ (vy origin) node-height)
                           (+ (vx origin) (lp1-middle (lp1-left lp1)))
                           (+ (vy origin) margin-y))
    (recursively-render-entity-mapped-b-tree
     (b-left mapped-b-tree)
     (lp1-left lp1)
     (vector (vx origin) (+ (vy origin) margin-y))))
  (when (b-right mapped-b-tree)
    (sdl2:render-draw-line (lem-sdl2:current-renderer)
                           (+ (vx origin) (lp1-middle lp1))
                           (+ (vy origin) node-height)
                           (+ (vx origin) (lp1-right-root lp1) (lp1-middle (lp1-right lp1)))
                           (+ (vy origin) margin-y))
    (recursively-render-entity-mapped-b-tree
     (b-right mapped-b-tree)
     (lp1-right lp1)
     (vector (+ (vx origin) (lp1-right-root lp1)) (+ (vy origin) margin-y)))))

(let ((kerberos-mapped-b-tree (b-tree-map-to-entity kerberos-b-tree)))
  (recursively-render-entity-mapped-b-tree kerberos-mapped-b-tree (lp1 kerberos-mapped-b-tree) (vector 0 0)))

(defun destroy-entity (entity)
  (declare (type entity entity))
  (with-slots (texture surface) entity
    (sdl2:destroy-texture texture)
    (sdl2:free-surface surface)))

(defun sinisoidal-render-function (texture window buffer)
  (let ((renderer (lem-sdl2:current-renderer)))
    (sdl2:set-render-target renderer texture)
    (let ((sinusoidal-x (round  (+ 100 (* 50 (sin  (/ (get-internal-real-time) 1000000))) ))))
      (unless entity
        (setf entity (make-entity "rizzler")))
      (sdl2:with-rects ((dest 100 sinusoidal-x (sdl2:surface-width (entity-surface entity)) (sdl2:surface-height (entity-surface entity))))
        (sdl2:render-copy renderer (entity-texture entity) :dest-rect dest))
      (sdl2:with-rects ((dest 100 sinusoidal-x 100 100))
        (sdl2:set-render-draw-color renderer 255 255 255 0)
        (sdl2:render-draw-rect renderer dest)))))

;; We override the render function for our custom buffer type
(defmethod lem-sdl2:render (texture window (buffer naza-graphical-buffer))
  (let ((renderer (lem-sdl2:current-renderer)))
    (sdl2:set-render-target renderer texture)
    (unless kerberos-mapped-b-tree
      (setq kerberos-mapped-b-tree (b-tree-map-to-entity kerberos-b-tree)))
    (recursively-render-entity-mapped-b-tree kerberos-mapped-b-tree (lp1 kerberos-mapped-b-tree) (vector 0 20))))