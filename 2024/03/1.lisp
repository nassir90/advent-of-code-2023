(ql:quickload "uiop")
(ql:quickload "cl-ppcre")

(defun handle-line (line)
  (format t "===line===~%" left right)
  (let ((sum 0))
    (cl-ppcre:do-matches-as-strings (match "mul\\(\\d+,\\d+\\)" line)
      (cl-ppcre:do-register-groups (left right) ("(\\d+),(\\d+)" match)
        (format t "~A * ~A~%" left right)
        (setf sum (+ sum (* (read-from-string left) (read-from-string right))))))
    sum))


(defun main ()
  (loop for line in (uiop:read-file-lines "input")
        sum (handle-line line)))