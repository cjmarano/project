<<<<<<< HEAD
#!/opt/homebrew/bin/clisp
=======
#!/usr/local/bin/clisp
>>>>>>> b3d08f2b7b7969fcc8e0f0c1f1af39f38341eb63
(defun demo-function (flag)
   (print 'entering-outer-block)
   
   (block outer-block
      (print 'entering-inner-block)
      (print (block inner-block

         (if flag
            (return-from outer-block 3)
            (return-from inner-block 5)
         )

         (print 'This-wil--not-be-printed))
      )

      (print 'left-inner-block)
      (print 'leaving-outer-block)
   t)
)
(demo-function t)
(terpri)
(demo-function nil)
