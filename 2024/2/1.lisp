(ql:quickload "str")
(ql:quickload "uiop")

(loop for line in (uiop:read-file-lines "/home/naza/projects/aoc/2024/2/input")
      sum (let* ((split (str:split " " line))
                (list (mapcar #'read-from-string split))
                (safe (line-safe list)))
           (if safe 1 0)))

(defun safe (a b)
  (and (<= 1 (abs (- a b)) 3)))

(defun pair-delta (a b)
  (signum (- a b)))

(defun generate-2-pairs (sequence)
  (loop for i from 0 below (1- (length sequence))
        collect (list (nth i sequence)
                      (nth (1+ i) sequence))))

(defun 2-pairs-safe (pairlist &optional previous-pair-delta)
  (if (not pairlist)
      t
      (let ((current-pair (car pairlist))
            (current-pair-delta (apply #'pair-delta (car pairlist))))
        (let ((delta-valid (or (not previous-pair-delta) (= previous-pair-delta current-pair-delta)))
              (relative-valid (apply #'safe current-pair)))
          (when (and delta-valid relative-valid)
            (2-pairs-safe (cdr pairlist) current-pair-delta))))))

(defun line-safe (sequence)
  (2-pairs-safe (generate-2-pairs sequence)))