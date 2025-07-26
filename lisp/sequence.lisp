#!/opt/homebrew/bin/sbcl
(setq x (vector 'a 'b 'c 'a 'b 'c 'd 'e 'f 'g))
(write (length x))
(terpri)
(write (elt x 0))
(terpri)
(write (elt x 1))
(terpri)
(write (elt x 2))
(terpri)
(write (elt x 3))
(terpri)
(write (elt x 4))
(terpri)
(write (elt x 5))
(terpri)
(write (elt x 6))
(terpri)
(write (elt x 7))
(terpri)
(write (elt x 8))
(terpri)
(write (elt x 9))
(terpri)
(write (count 9 '(0 1 2 3 4 5 6 7 8 9)))








