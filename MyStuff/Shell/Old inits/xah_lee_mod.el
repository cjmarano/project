;; copy, move, rename etc to the other pane
(setq dired-dwim-target t)

      ;; remember cursor position
(when (>= emacs-major-version 25) (save-place-mode 1))


;; make typing delete/overwrites selected text
(delete-selection-mode 1)

;; disable shift select
(setq shift-select-mode nil)

;; for isearch-forward, make these equivalent: space newline tab hyphen underscore
(setq search-whitespace-regexp "[-_ \t\n]+")

;; 2015-07-04 bug of pasting in emacs.
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16737#17
;; http://xahlee.info/emacs/misc/emacs_bug_cant_paste_2015.html
;; (setq x-selection-timeout 300)
;; (setq save-interprogram-paste-before-kill t)
;; (setq x-select-enable-clipboard-manager nil)


;; Emacs: Jump to Previous Position
;; http://xahlee.info/emacs/emacs/emacs_jump_to_previous_position.html

;; repeated C-u set-mark-command move cursor to previous mark in current buffer
(setq set-mark-command-repeat-pop t)

(setq mark-ring-max 10)
(setq global-mark-ring-max 10)

;;; rendering related for coding/editting

;; force line wrap to wrap at word boundaries
;; http://xahlee.info/emacs/emacs/emacs_toggle-word-wrap.html
(setq-default word-wrap t)

(electric-indent-mode 0)

(set-default 'tab-always-indent 'complete)

;; no mixed tab space
(setq-default indent-tabs-mode nil)
 ; gnu emacs at least 23.1 to 28 default is t

;; gnu emacs default to 8. tooo big. but problem of diff value is that some elisp source code in gnu emacs expected 8 to look nice, cuz they use mixed tab and space. but in golang, 8 is too much. also, python and others, standardize to 4
(setq-default tab-width 4)

(setq sentence-end-double-space nil )

(if (version< emacs-version "28.1")
    (defalias 'yes-or-no-p 'y-or-n-p)
  (setq use-short-answers t))

;; 2021-12-21. fuck Alan Mackenzie
;; Emacs Lisp Doc String Curly Quote Controversy
;; http://xahlee.info/emacs/misc/emacs_lisp_curly_quote_controversy.html
(setq text-quoting-style 'grave)
