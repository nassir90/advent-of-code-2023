(ql:quickload "uiop")
(ql:quickload "str")
(ql:quickload "cl-ppcre")

(defvar file "input")
(defvar line-length nil)
(defun read-input () (uiop:read-file-lines file))

(defgeneric traverser-advance (r)
  (:documentation "advance the traverser and return a new index"))

(defclass diagonal-traverser ()
  ((rows :initarg :rows)
   (columns :initarg :columns)
   (location :initform 0)
   (stage :initform 0)))

(defun coordinates-from-location (d)
  (with-slots (rows location) d
    (if (< location rows)
        (list location 0)
        (list (1- rows) (1+ (- location rows))))))

(defun stage-limit-from-location (d)
  (destructuring-bind (row column) (coordinates-from-location d)
    (with-slots (rows location) d
      (if (< location rows)
          (1+ row)
          (- rows column)))))

(defmethod traverser-advance ((d diagonal-traverser))
  (with-slots (rows columns location stage) d
    (destructuring-bind (row column) (coordinates-from-location d)
      (let ((stage-limit (stage-limit-from-location d)))
        (when (> stage-limit 0)
          (if (< stage stage-limit)
              (prog1 (list (- row stage) (+ column stage))
                (incf stage))
              (progn
                (setf stage 0)
                (incf location)
                (traverser-advance d))))))))

(defun at-new-row (d)
  (= (slot-value d 'stage) (stage-limit-from-location d)))

(defun diagonal-transform (input)
  (loop
    with f = (make-instance 'diagonal-traverser
                            :rows (length input)
                            :columns (length input))
    with a
    with l
    for (row column) = (traverser-advance f)
    while (and row column)
    do (push (elt (elt input row) column) l)
       (when (at-new-row f)
         (push (reverse l) a)
         (setf l ()))
    finally (return (reverse a))))

(defun null-transform (input)
  (loop for i from 0 below line-length
        collect (loop for j from 0 below line-length
                      collect (elt (elt input i) j))))

(defun clockwise-transform (input)
  (loop for i from 0 below line-length
        collect (loop for j from (1- line-length) downto 0
                 collect (elt (elt input j) i))))

(defun dump-grid (list)
  (loop for line in list
        do (loop for c in line do (format t "~A" c))
        (format t "~%")))

(defun char-array-to-string (char-array)
  (str:join "" (mapcar #'(lambda (a) (format nil "~A" a)) char-array)))

(defun count-xmas-matches (grid)
  (loop for line in grid
        sum (+ (cl-ppcre:count-matches "XMAS" (char-array-to-string line))
               (cl-ppcre:count-matches "SAMX" (char-array-to-string line))))) 

(defun clockwise-diagonal-transform (input)
  (diagonal-transform (clockwise-transform input)))

(defun main ()
  (let* ((input (read-input))
         (line-length (length (car input))))
    (let ((null (count-xmas-matches (null-transform input)))
          (clockwise (count-xmas-matches (clockwise-transform input)))
          (diagonal (count-xmas-matches (diagonal-transform input)))
          (clockwise-diagonal (count-xmas-matches (clockwise-diagonal-transform input))))
      (format t "null: ~A~%" null)
      (format t "clockwise: ~A~%" clockwise)
      (format t "diagonal: ~A~%" diagonal)
      (format t "clockwise-diagonal: ~A~%" clockwise-diagonal)
      (format t "total: ~A~%" (+ null clockwise diagonal clockwise-diagonal)))))

(main)
