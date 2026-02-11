;;; ... -*- lexical-binding: t -*-
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	         '("elpa"  . "https://elpa.gnu.org/packages/")t)
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/")t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/")t)

(package-initialize)

(require 'use-package)

(setq package-native-compile t)

(defun my-exec-path-from-shell-initialize ()
     (when (memq window-system '(mac ns x))
       (exec-path-from-shell-initialize)))

(use-package exec-path-from-shell
  :init
  (add-hook 'after-init-hook 'my-exec-path-from-shell-initialize))


use-package use-package
  :config
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-hook-name-suffix nil
        use-package-verbose t))

(use-package package
:config (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(setq user-emacs-directory "~/.cache/emacs/")

(use-package no-littering)

(require 'no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 110))

(server-start)
(setq history-length 10)
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
(setq-default create-lockfiles nil)
(delete-selection-mode 1)


(use-package display-line-numbers
  :hook (prog-mode-hook . display-line-numbers-mode))

(recentf-mode 1)
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(use-package dashboard
:ensure t
:init
(progn
(setq dashboard-items '((recents   . 10)
                      (bookmarks . 5)
                      (projects  . 5)
                      (agenda    . 5)))
(setq dashboard-item-shortcuts '((recents   . "r")
                                 (bookmarks . "m")
                                 (projects  . "p")
                                 (agenda    . "a")
                                 ))

(setq dashboard-show-shortcuts nil)
(setq dashboard-center-contents nil)
(setq dashboard-banner-logo-title "Big")
(setq dashboard-set-file-icons t)
(setq dashboard-set-heading-icons t)
(setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
(setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
(setq dashboard-startup-banner "~/Pictures/Trefoil.png")
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq dashboard-set-init-info t)

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

(require 'nerd-icons-completion)
(nerd-icons-completion-mode)

(use-package nerd-icons-completion
  :config)

;; Save backup files to a dedicated directory.
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))

;; Use copying to create backup files.
;;
;; The default is nil, which means Emacs moves file to backup and then copy it
;; back. First seen on https://idiomdrottning.org/bad-emacs-defaults.
(setq backup-by-copying t)

;; Make numeric backup versions unconditionally.
(setq version-control t)

;; No not delete backup files.
(setq delete-old-versions -1)

;; Back up files even covered by version control.
(setq vc-make-backup-files t)

(defun my/insert-current-date ()
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %a]")))

(keymap-global-set "C-c s d" #'my/insert-current-date)

(setq ring-bell-function 'ignore)

(use-package vertico
  :bind
  (:map vertico-map
        ([escape] . minibuffer-keyboard-quit)
        ("<prior>" . vertico-scroll-down)
        ("<next>" . vertico-scroll-up))
  :init
  (vertico-mode)
(setq vertico-count 25))

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

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(setq denote-directory (expand-file-name "~/project/org/notes/"))
(setq denotes-known-keywords '("emacs" "init" "general" "shell"))
(setq denote-file-type nil)
(add-hook 'dired-mode-hook #'denote-dired-mode)
(keymap-global-set "s-b" 'denote)

(setq completion-styles '(substring basic))

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
(marginalia-mode)

(use-package smartparens
  :ensure smartparens  ;; install the package
  :hook (emacs-lisp-mode-hook common-lisp-mode-hook lisp-mode-hook)
  :config
  ;; load default config
  (require 'smartparens-config))


(use-package org
  :init
  ;; fix for "Invalid function: org-element-with-disabled-cache"
  ;; https://www.reddit.com/r/emacs/comments/1hayavx/invalid_function_orgelementwithdisabledcache/
  ;; [2026-01-16 Fri] fixed in v9.7+
  ;; (setq native-comp-jit-compilation-deny-list '(".*org-element.*"))

  ;; Adding a "/" so that =find-file= finds the files under =~/org/=.
  (setq org-directory "~/org/")

  (defun my/expand-org-file-name (filename)
    (expand-file-name filename org-directory))
    (setq my/org-tasks-file (my/expand-org-file-name "tasks.org"))
  (setq my/org-references-file (my/expand-org-file-name "references.org"))
  (setq my/org-blog-file (my/expand-org-file-name "blog/content/content.org"))
)

  :config
  ;; Note: WAIT(w) is an experimental state for short-term wait (~10
  ;; mins ~ few hours) within today so that I can switch to anther task
  ;; and come back later.
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "WAIT(w)" "|" "DONE(d)")
  ))
  
org-capture - Capturing
(use-package org-capture
  :ensure org
  :bind ("C-c c" . org-capture)
  :config
  ;; The default file for capturing.
  (setq org-default-notes-file my/org-inbox-file)

  ;; Org Capture Templates
  ;;
  ;; See https://orgmode.org/manual/Template-elements.html#index-org_002ddefault_002dnotes_002dfile-1
  (setq org-capture-templates nil)

;; Archive to the same file under "* Archive"
;; Alternative: Archive to the same heading with `org-archive-to-archive-sibling' (C-c C-x A).
(setq org-archive-location "::* Archive")

  ;; Use python3.
  (setq org-babel-python-command "python3")
  (setq org-confirm-babel-evaluate nil))

(setq org-babel-default-header-args:emacs-lisp '((:lexical . "yes")))

(setq org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))
(setq org-insert-heading-respect-content t)

(use-package eglot
  ;; create pyrightconfig.json with `{"venvPath": ".", "venv": ".venv"}`
  :hook (python-mode-hook . eglot-ensure)
  :bind
  (:map eglot-mode-map
        ("M-l r" . eglot-rename)
        ("M-l TAB" . eglot-format)
        ("M-RET" . eglot-code-actions)
        ("M-l h" . eglot-inlay-hints-mode)))

(use-package python
  :config
  (setq-default python-indent-guess-indent-offset nil)
  (setq-default python-indent-offset 4))

(use-package pyvenv
  ;; Note: activate with `pyvenv-mode`, a global minor mode.
  :config
  (pyvenv-activate (expand-file-name "venv" user-emacs-directory)))

(setq gc-cons-threshold (expt 2 23)) ;; 8MB
(setq gc-cons-percentage 0.5)



