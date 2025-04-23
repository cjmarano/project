;; Shortened init

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

;; compile packages.

(defun fixed-native-compile-async-skip-p
    (native-compile-async-skip-p file load selector)
  (let* ((naive-elc-file (file-name-with-extension file "elc"))
         (elc-file       (replace-regexp-in-string
                          "\\.el\\.elc$" ".elc" naive-elc-file)))
    (or (gethash elc-file comp--no-native-compile)
        (funcall native-compile-async-skip-p file load selector))))

(advice-add 'native-compile-async-skip-p
    :around 'fixed-native-compile-async-skip-p)

;; init.el --- user init file
(setq load-prefer-newer t)
(add-to-list 'load-path "~/.emacs.d/elpa/auto-compile-20250301.1627")
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(require 'use-package)


;; added 20JUL2023 ---------------------------------------------
(add-to-list 'default-frame-alist '(height . 70))
(add-to-list 'default-frame-alist '(width . 100))

;; basic setup -------------------------------------------------------
(tool-bar-mode -1)
(column-number-mode t)
(global-font-lock-mode 1)
(setq inhibit-splash-screen t) 
(setq inhibit-startup-screen t)
(setq global-auto-revert-mode 1)
(setq use-short-answers t)
(setq ediff-split-window-function 'split-window-horizontally)
(keymap-global-set "M-p" 'previous-buffer)
(keymap-global-set "M-n" 'next-buffer)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Make everything use UTF-8:

;; (use-package emacs
;;   :init
;;   (set-charset-priority 'unicode)
;;   (setq locale-coding-system 'utf-8
;;         coding-system-for-read 'utf-8
;;         coding-system-for-write 'utf-8)
;;   (set-terminal-coding-system 'utf-8)
;;   (set-keyboard-coding-system 'utf-8)
;;   (set-selection-coding-system 'utf-8)
;;   (prefer-coding-system 'utf-8)
;;   (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))


(use-package emacs
  :init
  (set-face-attribute 'default nil
    :font "Hack Nerd Font Mono"
    :height 180))

(use-package emacs
  :init
  (defun ab/enable-line-numbers ()
    "Enable relative line numbers"
    (interactive)
    (display-line-numbers-mode)
    (setq display-line-numbers 'relative))
  (add-hook 'prog-mode-hook #'ab/enable-line-numbers))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package nerd-icons)

(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.5) ; Open after .5s instead of 1s
  :config
  (which-key-mode))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
  (keymap-global-set "C-x g" 'magit-status) 

(use-package diff-hl
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

(use-package vterm)

;; SPC and ' will open terminal. Press again to bg it.
(use-package vterm-toggle)

;; (use-package emacs
;;   :init
;;   (setq-default fill-column 80)
;;   (set-face-attribute 'fill-column-indicator nil
;;                       :foreground "#717C7C" ; katana-gray
;;                       :background "transparent")
;;   (global-display-fill-column-indicator-mode 1))

(use-package emacs
  :config
  (setq backup-directory-alist `(("." . "~/.saves"))))

(use-package company-mode
  :init
  (global-company-mode))

(use-package emacs
    :hook (rust-mode . eglot-ensure))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package rust-mode)

(use-package rg)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi-tritanopia)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
