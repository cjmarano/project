#!/usr/local/bin/clisp

(defun roller (num)
  (pprint (random (parse-integer (nth 0 num))))
  )

(setf userput *args*)
(setf *random-state* (make-random-state t))
(roller userput)
