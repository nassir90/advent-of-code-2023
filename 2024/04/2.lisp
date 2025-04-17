(ql:quickload "uiop")
(ql:quickload "str")
(ql:quickload "cl-ppcre")

(defvar file "input")
(defun read-input () (uiop:read-file-lines file))

(defgeneric traverser-advance (r)
  (:documentation "advance the traverser and return a new index"))

(defclass diagonal-traverser ()
  "A class I made to separate the index-generating logic of the diagonal transform from the actual
characters that were being transformed so that I could reduce the amount of stuff I had to keep in
my head. Might not be the best abstraction but oh well. I wanted to do some CL OOP anyway."
  ((rows :initarg :rows)
   (columns :initarg :columns)
   (location :initform 0)
   (stage :initform 0)))

(defun coordinates-from-location (d)
  (with-slots (rows location) d
    (if (< location rows) (list location 0) (list (1- rows) (1+ (- location rows))))))

(defun stage-limit-from-location (d)
  (destructuring-bind (row column) (coordinates-from-location d)
    (with-slots (rows location) d
      (if (< location rows) (1+ row) (- rows column)))))

(defmethod traverser-advance ((d diagonal-traverser))
  "Go upwards the current diagonal or wrap to the next diagonal strip if at the end of the current strip.
This function returns the location of the traverser before advancing. I.e. the first invocation of this
function will always yield (0 0)"
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
  "After collecting a coordinate from the diagonal traverser, check this to see if you just reached
the end of a diagonal strip. If so you should store the collected coordinates and create a new list
for a new strip"
  (= (slot-value d 'stage) (stage-limit-from-location d)))

(defun diagonal-transform (input)
  (loop
    with d = (make-instance 'diagonal-traverser :rows (length input) :columns (length input))
    with strips
    with diagonal-strip
    for (row column) = (traverser-advance d)
    while (and row column)
    do (push (elt (elt input row) column) diagonal-strip)
       (when (at-new-row d)
         (push diagonal-strip strips)
         (setf diagonal-strip ()))
    finally (return strips)))

(defun null-transform (input)
  (let ((line-length (length input)))
    (loop for i from 0 below line-length
        collect (loop for j from 0 below line-length
                      collect (elt (elt input i) j)))))

(defun clockwise-transform (input)
  (let ((line-length (length input)))
    (loop for i from 0 below line-length
          collect (loop for j from (1- line-length) downto 0
                        collect (elt (elt input j) i)))))

(defun clockwise-diagonal-transform (input)
  (diagonal-transform (clockwise-transform input)))

(defun index-transform (input)
  (loop for row below (length input)
        collect (loop for column below (length input)
                      collect (list row column))))

(defun dump-grid (list)
  "This is just a 2D array printing function"
  (loop for line in list
        do (loop for c in line do (format t "~A" c))
        (format t "~%")))

(defun char-array-to-string (char-array)
  (str:join "" (mapcar #'(lambda (a) (format nil "~A" a)) char-array)))

(defun count-xmas-matches (grid)
  (loop for line in grid
        sum (+ (cl-ppcre:count-matches "XMAS" (char-array-to-string line))
               (cl-ppcre:count-matches "SAMX" (char-array-to-string line)))))

(defun collect-mas-centerpoints (grid indices)
  "Iterate over the rows of the grid and try to find the string MAS.
This returns the row and column that the A was found at"
  (loop with centerpoints
        for line in grid
        for indices-line in indices
        do (cl-ppcre:do-matches (start end "MAS" line)
             (push (elt indices-line (1+ start)) centerpoints))
        do (cl-ppcre:do-matches (start end "SAM" line)
             (push (elt indices-line (1+ start)) centerpoints))
        finally (return centerpoints)))

(defun compute-mas-intersections (input transform h)
  "Look at all points crossed by diagonals going one way and look at points crossed by diagonals
going the other way and store frequencies in a hash table. Naturally when a hash table value is
greater than 2, it means that there's an intersection there"
  (let ((indices (index-transform input)))
    (loop for elt in (append (collect-mas-centerpoints (diagonal-transform input)
                                                       (diagonal-transform indices))
                             (collect-mas-centerpoints (clockwise-diagonal-transform input)
                                                       (clockwise-diagonal-transform indices)))
          do (let ((key (intern (format nil "~A" elt))))
               (if (gethash key h)
                   (incf (gethash key h))
                   (setf (gethash key h) 1))))))

(defun main-part-2 ()
  (let ((h (make-hash-table)))
    (compute-mas-intersections (read-input) #'diagonal-transform h)
    (loop for v being the hash-values in h count (= v 2))))

(main-part-2)

;; 1870 is too high
;; 1850 is just right

(defun main-part-1 ()
  (let* ((input (read-input))
         (null (count-xmas-matches (null-transform input)))
         (clockwise (count-xmas-matches (clockwise-transform input)))
         (diagonal (count-xmas-matches (diagonal-transform input)))
         (clockwise-diagonal (count-xmas-matches (clockwise-diagonal-transform input)))
         (total (+ null clockwise diagonal clockwise-diagonal)))
    (format t "null: ~A, clockwise: ~A, diagonal: ~A, clockwise-diagonal: ~A | total: ~A~%"
               null      clockwise      diagonal      clockwise-diagonal       total)))

(main-part-1)

;; sane solution for part 2 without OOP coolaid

(defun generate-all-crosses (input)
  (loop
    for row from 1 below (1- (length input))
    append (flet ((at (row column) (elt (elt input row) column)))
             (loop
               for column from 1 below (1- (length input))
               collect (list (list (at (1- row) (1- column)) (at row column) (at (1+ row) (1+ column)))
                             (list (at (1+ row) (1- column)) (at row column) (at (1- row) (1+ column))))))))

(defun mas (s)
  (or (equal '(#\M #\A #\S) s) (equal '(#\S #\A #\M) s)))

(defun main-part-2-a ()
  (loop for (first second) in (generate-all-crosses (read-input))
        count (and (mas first) (mas second))))


(main-part-2-a)
