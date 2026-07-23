#!/opt/homebrew/bin/clisp
; comment
(defconstant december   12)
(defconstant xmas-d     25)
(defconstant days-year 365)

(defun days (m)
  (if
   (member m '(4 6 9 11)) 30
   (if (= m 2) 28 31)))

(defun cum-days-beg(m)
  (if (= m 1) 0
      (+ (days (- m 1))
         (cum-days-beg (- m 1 )))))

(loop for i from 1 to 6 do
  (princ "Enter Month no:  ")
  (setq m (read))
  (princ "Enter Day no  :  ")
  (setq d (read))
  (setq dl (+ d      (cum-days-beg m))
        xd (+ xmas-d (cum-days-beg december))
        days-to      (- xd dl))
(when (< days-to 0)
  (setq days-to (+ days-to days-year)))
(format t
        "~d days to Christmas. ~%~2%" days-to))

  

