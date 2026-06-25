(defun sum (list)
  (if list (+ (car list) (sum (cdr list))) 0))
