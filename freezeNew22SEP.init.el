(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	         '("elpa"  . "https://elpa.gnu.org/packages/")t)
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/")t)
;; (add-to-list 'package-archives
;;              '("org" . "https://orgmode.org/elpa/")t)

(package-initialize)

(require 'use-package)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))) 

(defun packages-require (&rest packs)
  "Install and load a package. If the package is not available installs it automaticaly."
  (mapc  (lambda (package)
           (unless (package-installed-p package)
             (package-install package)
             )
	       )

         packs

         ))

(setq load-prefer-newer t)
(add-to-list 'load-path "~/.emacs.d/elpa/auto-compile-20250901.1341")
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(setq user-emacs-directory "~/.cache/emacs/")

(use-package no-littering)

(require 'no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 110))

(server-start)

(setq history-length 10)
(setq savehist-mode t)
;; (setq org-indent-mode t)
(setq-default cursor-type 'hbar)
(setq set-cursor-color "Cyan")
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(global-font-lock-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(transient-mark-mode t)
(global-display-line-numbers-mode t)
(column-number-mode t)
(tool-bar-mode -1)
(global-hl-line-mode +1)
(setq global-auto-revert-mode 1)
(setq auto-revert-use-notify t) 
(setq use-short-answers t)
(setq ediff-split-window-function 'split-window-horizontally)
(setq file-name-shadow-properties '(invisible t))
(file-name-shadow-mode)

(delete-selection-mode 1)

(keymap-global-set "M-p" 'previous-buffer)
(keymap-global-set "M-n" 'next-buffer)
(keymap-global-set "M-o" 'other-window)
(keymap-global-set "M-g" 'recentf)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(recentf-mode 1)
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(when (display-graphic-p)
  (context-menu-mode))

;; (file-name-shadow-mode 1)
;; (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
(setq delete-by-moving-to-trash t)
(setq dired-dwim-target t)

(add-hook 'dired-load-hook
  (lambda ()
    (require 'dired-x)))

(setq ediff-diff-options "--text")

(use-package dashboard
:ensure t
:init
(progn
(setq dashboard-items '((recents   . 10)
                      (bookmarks . 5)
                      (projects  . 5)
                      ;; (agenda    . 5)
                      ))
(setq dashboard-item-shortcuts '((recents   . "r")
                                 (bookmarks . "m")
                                 (projects  . "p")
                                 ;; (agenda    . "a")
                                 ))

(setq dashboard-show-shortcuts nil)
(setq dashboard-center-contents nil)
(setq dashboard-banner-logo-title "New")
(setq dashboard-set-file-icons t)
(setq dashboard-set-heading-icons t)
(setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
(setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
(setq dashboard-startup-banner "~/Pictures/Trefoil.png")
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq dashboard-set-init-info t)
;;  (setq dashboard-set-footer nil) 

(setq dashboard-startupify-list '(dashboard-insert-banner
                                  dashboard-insert-newline
                                  dashboard-insert-banner-title
                                  dashboard-insert-newline
                                  dashboard-insert-navigator
                                  dashboard-insert-newline
                                  dashboard-insert-init-info
                                  dashboard-insert-items
                                  dashboard-insert-newline))
  )

:config
(dashboard-setup-startup-hook))

(require 'doom-modeline)
(doom-modeline-mode 1)
(use-package nerd-icons
  :ensure t)
(setq doom-modeline-buffer-file-name-style 'file-name)
;; (setq doom-modeline-major-mode-color-icon t)
;; (setq doom-modeline-major-mode-icon t)
(setq doom-modeline-buffer-encoding t)
(setq doom-modeline-env-version t)
(setq doom-modeline-env-enable-python t)
(setq doom-modeline-env-enable-ruby t)
(setq doom-modeline-env-enable-rust t)
(setq doom-modeline-minor-modes t)
(setq doom-modeline-lsp t)
(setq doom-modeline-lsp-icon t)
(setq doom-modeline-buffer-name t)
(setq doom-modeline-project-detection 'auto)

(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)
;; Make regular Isearch interpret the empty space as regular
;; expression matching any character between words you give it.
(setq search-whitespace-regexp ".*?")

(require 'nerd-icons-completion)
(nerd-icons-completion-mode)

(use-package nerd-icons-completion
  :config)

(use-package show-font
  :ensure t
  :bind
  (("C-c s f" . show-font-select-preview)
   ("C-c s t" . show-font-tabulated)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package magit
  :defer t
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
  (keymap-global-set "C-x g" 'magit-status) 

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay .9))

(use-package vterm
  :defer t
  :ensure t)

(use-package vertico
 :init
 (vertico-mode))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(setq denote-directory (expand-file-name "~/project/org/notes/"))
(setq denotes-known-keywords '("emacs" "init" "general" "shell"))
(setq denote-file-type nil)
(add-hook 'dired-mode-hook #'denote-dired-mode)
(keymap-global-set "s-b" 'denote)

(setq completion-styles '(substring basic))

(use-package consult
;; Replace bindings. Lazily loaded due by `use-package'.
:bind (;; C-c bindings in `mode-specific-map'
       ("C-c M-x" . consult-mode-command)
       ("C-c h" . consult-history)
       ("C-c k" . consult-kmacro)
       ("C-c m" . consult-man)
       ("C-c i" . consult-info)
       ([remap Info-search] . consult-info)
       ;; C-x bindings in `ctl-x-map'
       ("C-x M-:" . consult-complex-command)    
       ("C-x b" . consult-buffer)               
       ("C-x 4 b" . consult-buffer-other-window)
       ("C-x 5 b" . consult-buffer-other-frame) 
       ("C-x t b" . consult-buffer-other-tab)   
       ("C-x r b" . consult-bookmark)           
       ("C-x p b" . consult-project-buffer)))

(use-package consult-denote
  :ensure t
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

(use-package marginalia
;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
;; available in the *Completions* buffer, add it to the
;; `completion-list-mode-map'.
:bind (:map minibuffer-local-map
            ("M-A" . marginalia-cycle))

;; The :init section is always executed.
:init
;; Marginalia must be activated in the :init section of use-package such that
;; the mode gets enabled right away. Note that this forces loading the
;; package.
(marginalia-mode))

;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)

;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; Look up *F*unctions (excludes macros).
;;
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
(global-set-key (kbd "C-h F") #'helpful-function)

(require 'smartparens-config)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'common-lisp-mode-hook #'smartparens-mode)
(add-hook 'lisp-mode-hook #'smartparens-mode)

;; ---------------------------------------------------------
;; Start of Org section ------------------------------------
;; ---------------------------------------------------------
(use-package org
  :pin gnu
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (defun efs/org-font-setup ()
    ;; Replace list hyphen with dot
    (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Set faces for heading levels
(with-eval-after-load 'org-faces
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1))))
  (set-face-attribute (car face) nil :font "Noto Serif" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block unspecified :inherit 'fixed-pitch)
(set-face-attribute 'org-code unspecified :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table unspecified :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim unspecified :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword unspecified :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line unspecified :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox unspecified :inherit 'fixed-pitch)))

(setq org-hide-emphasis-markers t)

(require 'org-indent)
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  
  (add-to-list 'org-emphasis-alist
               '("_" (:foreground "red")
                 ))

  (add-to-list 'org-emphasis-alist
               '("+" (:foreground "LightGreen")
                 ))

(defun efs/org-mode-setup ()
;;    (org-indent-mode)
(variable-pitch-mode 1)
(visual-line-mode 1))
;; ---------------------------------------------------------

(setq org-agenda-files
      '("~/project/org/journal/journal.org"
        "~/project/org/notes/notes.org"
        "~/project/org/tasks/tasks.org"
        "~/project/org/daily/daily.org"))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")
        (sequence "COMPLETED(c)")))

(setq org-refile-targets
      '(("Archive.org" :maxlevel . 1)
        ("Tasks.org" :maxlevel . 1)))

(setq org-tag-alist                   
      '((:startgroup)
                                      ; Put mutually exclusive tags here
        (:endgroup)
        ("@note" . ?t)
        ("@code" . ?c)
        ("@init" . ?i)))

(setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/project/org/tasks/tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "~/project/org/journal/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/org/notes.org")
          
           )
          ))

(keymap-set global-map "C-c j" 
              (lambda () (interactive) (org-capture nil "jj")))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/projects/org/roam")
  (org-roam-completion-everywhere t)

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

;; entries below seem to be additional, not required
(keymap-set global-map "C-c l" 'org-store-link)
(keymap-set global-map "C-c a" 'org-agenda)
(keymap-set global-map "C-c c" 'org-capture)
(setq org-log-done 'time)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   ;; (ruby . t)
   ;; (eshell . t)
   ;; (lisp . t)
   ;; (rust . t)
   ))

(require 'org-tempo)

;; (add-to-list 'org-structure-template-alist '("l" . "src emacs-lisp"))
;; (add-to-list 'org-structure-template-alist '("L" . "src lisp"))
;; (add-to-list 'org-structure-template-alist '("p" . "src python"))
;; (add-to-list 'org-structure-template-alist '("r" . "src ruby"))
;; (add-to-list 'org-structure-template-alist '("s" . "src shell"))

(let ((org-confirm-babel-evaluate nil)))

;; ---------------------------------------------------------
;; Org section ends here -----------------------------------
;; ---------------------------------------------------------

(use-package eglot
  :ensure nil
  :defer t
  :hook (python-mode . eglot-ensure)
  :hook (rust-mode . eglot-ensure))

    (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))
    (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) "pylsp")))
    (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((rust-mode rust-ts-mode) "rust-analyzer")))  

(setq python-indent-guess-indent-offset t)  
(setq python-indent-guess-indent-offset-verbose nil)

(setq python-python-command "$HOME/.pyenv/shims/python3")
(setq python-shell-completion-native-enable nil)

(use-package rustic
  :ensure nil
  :defer t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ;;              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))

  :config
;; comment to disable rustfmt on save
(add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
;; so that run C-c C-c C-r works without having to confirm, but don't try to
;; save rust buffers that are not file visiting. Once
;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
;; no longer be necessary.
(when buffer-file-name
  (setq-local buffer-save-without-query t))
(add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; (use-package rust-playground :ensure)

(use-package toml-mode
  :ensure nil
  :defer t)

(setq-local lsp-inlay-hint-enable t)
;; below from https://github.com/rksm/emacs-rust-config
(use-package lsp-mode
  :ensure nil
  :defer t
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
;; what to use when checking on-save. "check" is default, I prefer clippy
(lsp-rust-analyzer-cargo-watch-command "clippy")
(lsp-eldoc-render-all t)
(lsp-idle-delay 0.6)
;; enable / disable the hints as you prefer:
(lsp-inlay-hint-enable t)
;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
(lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
(lsp-rust-analyzer-display-chaining-hints t)
(lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
(lsp-rust-analyzer-display-closure-return-type-hints t)
(lsp-rust-analyzer-display-parameter-hints nil)
(lsp-rust-analyzer-display-reborrow-hints nil)
:config
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure nil
  :defer t
  :commands lsp-ui-mode
  :custom
(lsp-ui-peek-always-show t)
(lsp-ui-sideline-show-hover t)
(lsp-ui-doc-enable nil))
;; end lsp-mode additions for rust

(require 'tree-sitter)
(require 'tree-sitter-langs)
;; (global-tree-sitter-mode)
;; or just for rust-mode
(add-hook 'rust-mode-hook #'tree-sitter-mode)
;; Load the language definition for Rust, if it hasn't been loaded.
;; Return the language object.
(tree-sitter-require 'rust)
(tree-sitter-require 'python)

(add-hook 'after-init-hook 'global-company-mode)
(use-package company
  :ensure
  ;;  :after lsp-mode
  ;;  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
          (:map python-mode-map
              ("<tab>" . company-indent-or-complete-common))
  :custom
(company-minimum-prefix-length 1)
(company-idle-delay 0.5))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; -----------Current Lisp Section ---------------------------------
(setq inferior-lisp-program "/opt/homebrew/bin/sbcl")
(require 'slime-autoloads)
(eval-after-load "slime"  '(progn (slime-setup '(slime-fancy))))

(load (expand-file-name "~/.quicklisp/slime-helper.el"))
  
;; Enable Paredit.
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
(require 'paredit)

;; Enable Rainbow Delimiters.
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

;; Setup load-path, autoloads and your lisp system
(add-to-list 'load-path "~/.emacs.d/elpa")

;; *** also redundant? see 527 to 533 ***
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (paredit-mode t)
;;             (rainbow-delimiters-mode t)
;;             (show-paren-mode 1)
;;             ))

;; Eldoc for ielm
;; docs say this enabled by default now.
;; (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
;; (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
;; (add-hook 'ielm-mode-hook 'eldoc-mode)
;; ------------end lisp ------------------------------------

(projectile-mode +1)
;; Recommended keymap prefix on macOS
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Recommended keymap prefix on Windows/Linux
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


(defun add-or-switch-project-dwim (dir)
  (interactive (list (read-directory-name "Add to known projects: ")))
  (projectile-add-known-project dir)
  (find-file dir)
  (treemacs-add-and-display-current-project))

(keymap-global-set "C-c p" 'add-or-switch-project-dwim) 

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 180 :family "JetBrainsMono Nerd Font"))))
 '(cursor ((t (:background "light green"))))
 '(org-headline-done ((t (:foreground "gray80"))))
 '(org-level-1 ((t (:inherit outline-1 :background "gray22" :box (:line-width (1 . 1) :style released-button) :weight bold :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :background "gray23" :box (:line-width (1 . 1) :style released-button) :height 1.2)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist '(("" . "~/.backups")))
 '(company-box-enable-icon t)
 '(company-box-icons-alist 'company-box-icons-images)
 '(custom-enabled-themes '(sanityinc-tomorrow-eighties))
 '(custom-safe-themes
        '("ba4f725d8e906551cfab8c5f67e71339f60fac11a8815f51051ddb8409ea6e5c"
        "ad7d874d137291e09fe2963babc33d381d087fa14928cb9d34350b67b6556b6d"
        "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350"
        "04aa1c3ccaee1cc2b93b246c6fbcd597f7e6832a97aaeac7e5891e6863236f9f"
        default))
 '(dashboard-startupify-list
   '(dashboard-insert-banner dashboard-insert-newline
                             dashboard-insert-banner-title
                             dashboard-insert-newline
                             dashboard-insert-init-info
                             dashboard-insert-items
                             dashboard-insert-newline))
 '(denote-known-keywords '("emacs" "init" "general" "testing"))
 '(elpy-rpc-python-command "python3") 
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3")
 '(org-agenda-files '("~/project/org/3.org"))
 '(org-faces-easy-properties
   '((todo . :background) (tag . :foreground) (priority . :foreground)))
 '(org-id-locations-file "$HOME/.cache/emacs/var/org/id-locations.el")
 '(org-startup-folded 'fold)
;; '(org-tempo-keywords-alist nil)
 '(package-selected-packages
   '(0blayout 0xc all-the-icons all-the-icons-nerd-fonts auto-compile
              bind-key cargo cargo-mode color-theme-sanityinc-tomorrow
              company-box consult consult-denote dashboard denote
              diffview dired-single dired-subtree doom-modeline eglot
              elpy exec-path-from-shell external-completion flycheck
              flycheck-pyflakes flycheck-rust helpful jsonrpc kkp
              lsp-ui lua-mode magit marginalia material-theme
              nerd-icons-completion nerd-icons-dired no-littering
              ob-rust orderless org-bullets org-roam paredit
              projectile rainbow-delimiters rustic seq show-font slime
              smartparens toml-mode track-changes tree-sitter-langs
              treemacs treesit-auto use-package vertico vterm
              which-key))
 '(savehist-additional-variables '(kill-ring register-alist\ ) t)
 '(sort-fold-case t)
 '(warning-suppress-log-types '((use-package))))

'(python-shell-interpeter "$HOME/.pyenv/shims/python3")

;; duplicate of above? add-hook is different than selected packages.
;; below is for delimiters in all programming modes.
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(elpy-enable)

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
;; added to supress flymake error message when compliing python (12AUG2022)
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

(setq gc-cons-threshold (expt 2 23)) ;; 8MB
(setq gc-cons-percentage 0.5)

;; User-Defined init.el ends here
