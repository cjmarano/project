;; print all Unicode chars up to 2^16 whose name contains "BULLET"
(mapc
  (lambda (x)
    (let ((nn (get-char-code-property x 'name)))
      (when
          (and (not (null nn))
               (string-match "BULLET" nn))
        (insert-char x)
        (insert " " nn "\n") ) ) )
  (number-sequence 0 (expt 2 16)))
