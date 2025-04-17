(defpackage :aoc-2024/day-09
  (:use #:cl #:aoc-2024 #:lem #:alexandria #:aoc-2024/dsa/tree)
  (:import-from #:aoc-2024/dsa/tree))

(in-package :aoc-2024/day-09)

(defun current-font-size ()
  (lem-sdl2/font:font-config-size
   (lem-sdl2/display:display-font-config
    (lem-sdl2/display:current-display))))

(defun load-font ()
  (sdl2-ttf:open-font
   (lem-sdl2/resource:get-resource-pathname
    "resources/fonts/NotoSansMono-Regular.ttf")
   (current-font-size)))

(defun get/create-buffer (name)
  (declare (type string name))
  (or (lem:get-buffer name)
      (lem/buffer/internal:make-buffer name)
      (error "failed to execute get or create operation")))

(defun adjust-window-according-to-layout (layout)
  (declare (type layout layout))
  (when-let* ((buffer-windows (lem-core::get-buffer-windows (get/create-buffer "naza-graphical-buffer")))
              (window (first buffer-windows)))
    (let (
          (target (* 2 (/ (+ 20 (layout-span layout)) (current-font-size)))))
      (loop :while (> (lem-core::window-width window) target)
            :do (shrink-window-width window 1))
      (loop :while (< (lem-core::window-width window) target)
            :do (grow-window-width window 1))
      (redraw-display :force t))))

(defmacro vx (v) `(aref ,v 0))
(defmacro vy (v) `(aref ,v 1))

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

(defparameter kerberos-mapped-b-tree nil)
(defparameter kerberos-b-tree nil)

(define-command wipe-naza-graphical-buffer-cache () ()
  (setf kerberos-mapped-b-tree nil))

(defun b-right-rotate (tree)
  (let* ((right (b-right tree))
         (orphan (b-left right)))
    (setf (b-right tree) orphan)
    (setf (b-left right) tree)
    right))

(defun b-left-rotate (tree)
  (let* ((left (b-left tree))
         (orphan (b-right left)))
    (setf (b-left tree) orphan)
    (setf (b-right left) tree)
    left))
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

(defmethod width-for-layout ((entity entity))
  (sdl2:surface-width (entity-surface entity)))

(defgeneric make-entity (value))

(defmethod make-entity (value)
  (make-entity (make-instance 'annotated :value value :color '(255 255 255))))

(defclass annotated ()
  ((color :initarg :color :accessor a-color)
   (value :initarg :value :accessor a-value)))

;; Definitely not efficient...
(defmethod b-< ((a annotated) (b number)) (b-< (a-value a) b))
(defmethod b-< ((a number) (b annotated)) (b-< a (a-value b)))
(defmethod b-> ((a annotated) (b number)) (b-> (a-value a) b))
(defmethod b-> ((a number) (b annotated)) (b-> a (a-value b)))

(defmethod b-< ((a annotated) (b annotated)) (b-< (a-value a) (a-value b)))
(defmethod b-> ((a annotated) (b annotated)) (b-> (a-value a) (a-value b)))
(defmethod b-= ((a annotated) (b annotated)) (b-= (a-value a) (a-value b)))

(defun annotate (value color)
  (make-instance 'annotated :value value :color color))

(defmethod make-entity ((value annotated))
  (let* ((surface (apply #'sdl2-ttf:render-utf8-blended `(,(load-font) ,(format nil " ~a " (a-value value)) ,@(a-color value) 0)))
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
  
(defun recursively-render-entity-mapped-b-tree (mapped-b-tree layout origin)
  (declare (type b-tree mapped-b-tree)
           (type layout layout)
           (type vector origin))
  (sdl2:set-render-draw-color (lem-sdl2:current-renderer) 255 255 255 0)
  (sdl2:with-rects ((dest-rect (+ (vx origin) (layout-render-root layout)) (vy origin)
                               (width-for-layout (b-value mapped-b-tree)) node-height))
    (sdl2:render-copy (lem-sdl2:current-renderer) (entity-texture (b-value mapped-b-tree)) :dest-rect dest-rect)
    (sdl2:render-draw-rect (lem-sdl2:current-renderer) dest-rect))
  (when (b-left mapped-b-tree)
    (sdl2:render-draw-line (lem-sdl2:current-renderer)
                           (+ (vx origin) (layout-middle layout))
                           (+ (vy origin) node-height)
                           (+ (vx origin) (layout-middle (layout-left layout)))
                           (+ (vy origin) margin-y))
    (recursively-render-entity-mapped-b-tree
     (b-left mapped-b-tree)
     (layout-left layout)
     (vector (vx origin) (+ (vy origin) margin-y))))
  (when (b-right mapped-b-tree)
    (sdl2:render-draw-line (lem-sdl2:current-renderer)
                           (+ (vx origin) (layout-middle layout))
                           (+ (vy origin) node-height)
                           (+ (vx origin) (layout-right-root layout) (layout-middle (layout-right layout)))
                           (+ (vy origin) margin-y))
    (recursively-render-entity-mapped-b-tree
     (b-right mapped-b-tree)
     (layout-right layout)
     (vector (+ (vx origin) (layout-right-root layout)) (+ (vy origin) margin-y)))))
(defun destroy-entity (entity)
  (declare (type entity entity))
  (with-slots (texture surface) entity
    (sdl2:destroy-texture texture)
    (sdl2:free-surface surface)))

;; We override the render function for our custom buffer type
(defmethod lem-sdl2:render (texture window (buffer naza-graphical-buffer))
  (let ((renderer (lem-sdl2:current-renderer)))
    (sdl2:set-render-target renderer texture)
    (unless kerberos-mapped-b-tree
      (setq kerberos-mapped-b-tree (b-tree-map-to-entity kerberos-b-tree)))
    (recursively-render-entity-mapped-b-tree kerberos-mapped-b-tree (layout kerberos-mapped-b-tree) (vector 15 20))))


;;; --- "user logic"

(defun bind-layout-with-b-tree (b-tree)
  (declare (type b-tree b-tree))
  (wipe-naza-graphical-buffer-cache)
  (setf kerberos-b-tree b-tree)
  (adjust-window-according-to-layout (layout (b-tree-map-to-entity kerberos-b-tree))))

(bind-layout-with-b-tree (b-insert (b-insert (b-insert nil 1) 2) 3))
(bind-layout-with-b-tree (b-insert (b-insert (b-insert nil 3) 2) 1))
(bind-layout-with-b-tree (b-insert (b-insert (b-insert nil 1) 2) (- 3)))
(bind-layout-with-b-tree (b 5
                            (b 1)
                            (identity (b (annotate 50 '(10 255 10))
                                         (b (annotate 40 '(255 10 10)))
                                         (b (annotate 40 '(50 50 255))
                                            (b-tree-map (b 60
                                               (b 56
                                                  (b 55)
                                                  (b 57)))
                                                        (lambda (tree) (annotate tree '(100 100 255)))))))))

(defun fill-heights (b-tree)
  (declare (type b-tree b-tree))
  (let ((left (b-left b-tree))
        (right (b-right b-tree)))
    (when left (fill-heights left))
    (when right (fill-heights right))
    (setf (b-height b-tree)
          (1+ (max (if left (b-height left) 0)
                   (if right (b-height right) 0))))
    b-tree))

(bind-layout-with-b-tree (fill-heights (b 5
   (b 1)
   (identity (b (annotate 50 '(10 255 10))
                (b (annotate 40 '(255 10 10)))
                (b (annotate 40 '(50 50 255))
                   (b-tree-map (b 60
                                  (b 56
                                     (b 55)
                                     (b 57)))
                               (lambda (tree) (annotate tree '(100 100 255)))))))))

)

(bind-layout-with-b-tree (b-insert (b 5
                            (b 1)
                            (identity (b (annotate 50 '(10 255 10))
                                         (b (annotate 40 '(255 10 10)))
                                         (b (annotate 40 '(50 50 255))
                                            (b-tree-map (b 60
                                                           (b 56
                                                              (b 55)
                                                              (b 57)))
                                                        (lambda (tree) (annotate tree '(100 100 255)))))))) 66.132))

(let ((kerberos-mapped-b-tree (b-tree-map-to-entity kerberos-b-tree)))
  (recursively-render-entity-mapped-b-tree kerberos-mapped-b-tree (layout kerberos-mapped-b-tree) (vector 0 0)))