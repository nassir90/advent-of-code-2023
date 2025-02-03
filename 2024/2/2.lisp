;; File for part 2 of day 2 2024

(ql:quickload "str")
(ql:quickload "uiop")

(defvar input "/home/naza/projects/aoc/2024/2/input")
(setf input "/home/naza/projects/aoc/2024/2/example")

(defun main ()
  (loop for line in (uiop:read-file-lines input)
      sum (let* ((split (str:split " " line))
                (list (mapcar #'read-from-string split))
                (safe (line-safe list)))
           (if safe 1 0))))

(defun safe (a b)
  (and (<= 1 (abs (- a b)) 3)))

(defun pair-delta (a b)
  (signum (- a b)))

(defun generate-2-pairs (sequence)
  (loop for i from 0 below (1- (length sequence))
        collect (list (nth i sequence)
                      (nth (1+ i) sequence))))

(defun 2-pairs-safe (pairlist &optional previous-pair-delta have-removed-something)
  (if (not pairlist)
      t
      (let ((current-pair (car pairlist))
            (current-pair-delta (apply #'pair-delta (car pairlist))))
        (let ((delta-valid (or (not previous-pair-delta) (= previous-pair-delta current-pair-delta)))
              (relative-valid (apply #'safe current-pair)))
          (if (and delta-valid relative-valid)
              ;; If we're safe, just keep on recursing
              (2-pairs-safe (cdr pairlist) current-pair-delta have-removed-something)
              ;; Otherwise try remove the next number and proceed
              (unless have-removed-something
                (2-pairs-safe (cons (car pairlist) (cddr pairlist)) previous-pair-delta t)))))))

(defun line-safe (sequence)
  (2-pairs-safe (generate-2-pairs sequence)))