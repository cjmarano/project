;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with ‘C-x C-f’ and enter text in its buffer.

sh -c "if [ -n \"\$*\" ]; then exec /usr/local/bin/emacsclient --alternate-editor= --display=\"\$DISPLAY\" \"\$@\"; else exec emacsclient --alternate-editor= --create-frame; fi" sh %F
