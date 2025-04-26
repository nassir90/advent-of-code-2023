(defpackage :aoc-2024/day-09
  (:use #:lem #:cl #:aoc-2024 #:alexandria #:aoc-2024/font #:aoc-2024/dsa/tree #:aoc-2024/graphics/tree/layout)
  (:import-from #:aoc-2024/dsa/tree)
  (:import-from #:aoc-2024/graphics/tree/layout))

(in-package :aoc-2024/day-09)

;; RENDERING LGOIC SECTION

(defmacro vx (v) `(aref ,v 0))
(defmacro vy (v) `(aref ,v 1))

;; LEM MODE OPTIONS

(defclass naza-graphical-buffer (text-buffer)
  ((mapped-b-tree)
   (b-tree)))

(defparameter kerberos-mapped-b-tree nil)
(defparameter kerberos-b-tree nil)

(define-command wipe-naza-graphical-buffer-cache () ()
  (setf kerberos-mapped-b-tree nil))

(defun get/create-buffer (name)
  (declare (type string name))
  (or (lem:get-buffer name)
      (lem/buffer/internal:make-buffer name)
      (error "failed to execute get or create operation")))

(lem:define-major-mode naza-graphical-mode ()
  (:name "Graphical Mode"
   :keymap *naza-graphical-mode*))

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


(defun adjust-window-according-to-layout (layout)
  (declare (type layout layout))
  (when-let* ((buffer-windows (lem-core::get-buffer-windows (get/create-buffer "naza-graphical-buffer")))
              (window (first buffer-windows)))
    (let ((target (* 2 (/ (+ 20 (layout-span layout)) (current-font-size)))))
      (loop :while (> (lem-core::window-width window) target)
            :do (shrink-window-width window 1))
      (loop :while (< (lem-core::window-width window) target)
            :do (grow-window-width window 1))
      (redraw-display :force t))))

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

(defmethod b-< ((a annotated) (b number))
  (b-< (a-value a) b))

(defmethod b-< ((a number) (b annotated))
  (b-< a (a-value b)))

(defmethod b-< ((a annotated) (b annotated))
  (b-< (a-value a) (a-value b)))

(defmethod b-= ((a annotated) (b annotated))
  (b-= (a-value a) (a-value b)))

(defun annotate (value color)
  (make-instance 'annotated :value value :color color))

(defmethod make-entity ((value annotated))
  (let* ((surface (apply #'sdl2-ttf:render-utf8-blended `(,(aoc-2024/font:load-font) ,(format nil " ~a " (a-value value)) ,@(a-color value) 0)))
         (texture (sdl2:create-texture-from-surface (lem-sdl2:current-renderer) surface)))
    (make-instance 'entity :surface surface :texture texture)))

(defun b-map-to-entity (b-tree)
  (declare (type b-tree b-tree))
  (b-map b-tree #'make-entity))

(defun log-it (buffer-name k)
  (declare (type string buffer-name))
  (declare (type string k))
  (let ((buffer (get/create-buffer buffer-name)))
    (with-current-buffer buffer
      (insert-string (buffer-end-point buffer) k))))

(defun destroy-entity (entity)
  (declare (type entity entity))
  (with-slots (texture surface) entity
    (sdl2:destroy-texture texture)
    (sdl2:free-surface surface)))

(defmethod lem-sdl2:render (texture window (buffer naza-graphical-buffer))
  (let ((renderer (lem-sdl2:current-renderer)))
    (sdl2:set-render-target renderer texture)
    (unless kerberos-mapped-b-tree
      (setq kerberos-mapped-b-tree (b-map-to-entity kerberos-b-tree)))
    (recursively-render-entity-mapped-b-tree kerberos-mapped-b-tree (layout kerberos-mapped-b-tree) (vector 15 20))))


;;; GRAPHICAL BUFFER MANIPULATION SECTION

(defun bind-layout-with-b-tree (b-tree)
  (declare (type b-tree b-tree))
  (wipe-naza-graphical-buffer-cache)
  (setf kerberos-b-tree b-tree)
  (adjust-window-according-to-layout (layout (b-map-to-entity kerberos-b-tree))))

(defun some-logic-to-ignore ()
  (bind-layout-with-b-tree (b-insert (b-insert (b-insert nil 1) 2) 3))
  (bind-layout-with-b-tree (b-insert (b-insert (b-insert nil 3) 2) 1))
  (bind-layout-with-b-tree (b-insert (b-insert (b-insert nil 1) 2) (- 3)))
  (bind-layout-with-b-tree (b 5
                              (b 1)
                              (identity (b (annotate 50 '(10 255 10))
                                           (b (annotate 40 '(255 10 10)))
                                           (b (annotate 40 '(50 50 255))
                                              (b-map (b 60
                                                        (b 56
                                                           (b 55)
                                                           (b 57)))
                                                     (lambda (tree) (annotate tree '(100 100 255)))))))))

  (bind-layout-with-b-tree (b-with-recursively-fixed-heights (b 5
                                                                (b 1)
                                                                (identity (b (annotate 50 '(10 255 10))
                                                                             (b (annotate 40 '(255 10 10)))
                                                                             (b (annotate 40 '(50 50 255))
                                                                                (b-map (b 60
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
                                                        (b-map (b 60
                                                                  (b 56
                                                                     (b 55)
                                                                     (b 57)))
                                                               (lambda (tree) (annotate tree '(100 100 255)))))))) 66.132))

  (let ((kerberos-mapped-b-tree (b-map-to-entity kerberos-b-tree)))
    (recursively-render-entity-mapped-b-tree kerberos-mapped-b-tree (layout kerberos-mapped-b-tree) (vector 0 0))))

(some-logic-to-ignore)

;; STRING LOGIC SECTION

(defun string-to-list (string)
  (loop for character across string collect (read-from-string (string character))))

;; SOLUTION SECTION

(defclass entry ()
  ((key :initarg :key :accessor key)
   (value :initarg :value :accessor vaue)))

(defmethod b-< ((a entry) (b entry))
  (< (entry-key a) (entry-key b)))

(defmethod b-= ((a entry) (b entry))
  (= (entry-key a) (entry-key b)))

(defun entry (key value)
  (make-instance 'entry :key key :value value))

;; Class used for both spaces and blocks
(defstruct span
  (location nil :type integer)
  (size nil :type integer))

(defmethod b-< ((a span) (b span))
  (< (span-location a) (span-location b)))

(defmethod b-= ((a span) (b span))
  (= (span-location a) (span-location b)))

(defun nigz (queue)
  (let ((spaces nil)
        (blocks nil)
        (cursor 0))
    (labels ((process-space ()
               (when-let ((size (pop queue)))
                 (setf spaces (b-insert spaces (make-span :location cursor :size size)))
                 (setf cursor (+ cursor size))
                 (process-block)))
             (process-block ()
               (when-let ((size (pop queue)))
                 (push (make-span :location cursor :size size) blocks)
                 (setf cursor (+ cursor size))
                 (process-space))))
      (process-block))
    spaces))

(defmethod print-object ((span span) stream)
  (format stream "#<location=~A size=~A>" (span-location span) (span-size span)))

(let ((spaces nil))
  (b-loop (space (nigz (string-to-list "1111")))
    (push space spaces))
  spaces)

(defun solution ()
  (dolist (block block-queue)
    (when-let ((space (catch 'found
                        (b-loop (space spaces)
                          (when (space-fits block)
                            (throw 'found space))))))
      (move-block-into-space-and-mutate-spaces block space))))


(aoc-2024/dsa/tree:b-insert nil 3)
