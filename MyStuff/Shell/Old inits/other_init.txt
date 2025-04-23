
(setq gc-cons-threshold 10000000)

;; Restore after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 1000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

;; package management
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq load-prefer-newer t)

(require 'use-package)

;; auto install when necessary
(setq use-package-always-ensure t)

;; backups and other internal files should be 
(use-package no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(use-package recentf
  :config (recentf-mode 1)
  :custom ((recentf-max-menu-items 40)))

;; gui / global
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(column-number-mode t)
(global-display-line-numbers-mode t)
(size-indication-mode t)
(delete-selection-mode t)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq visible-bell t)
(scroll-bar-mode -1)
(setq-default frame-title-format "%b (%f)")
(global-prettify-symbols-mode +1)
(display-time-mode 1)

(set-face-attribute 'default nil :font "RobotoMono Nerd Font" :height 100 :weight 'regular)
(set-face-attribute 'bold nil :font "RobotoMono Nerd Font" :height 100 :weight 'bold)
(set-face-attribute 'italic nil :font "BlexMono Nerd Font" :height 100 :weight 'regular :slant 'italic)
(set-fontset-font t 'unicode (font-spec :name "RobotoMono Nerd Font" :size 10) nil)

(use-package color-theme-sanityinc-tomorrow)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme tsl/light-theme t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config))

(use-package all-the-icons)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 16)
	   (doom-modeline-buffer-file-name-style 'truncate-all)
	   (doom-modeline-minor-modes t)))

(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode)
  :custom ((which-key-idle-delay 1.0)
	   (which-key-enable-extended-define-key t)))

(use-package paredit
  :config (paredit-mode 1))
(use-package adjust-parens
  :config (adjust-parens-mode 1))
(use-package rainbow-delimiters
  :config (rainbow-delimiters-mode 1))
(use-package paren
  :custom ((show-paren-delay 0))
  :config
  (set-face-background 'show-paren-match "#ffaaaa")
  (show-paren-mode 1))

(use-package rainbow-mode
  :config
  (rainbow-mode 1))


(use-package company
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.7)
  :config
  (global-company-mode 1))

(use-package company-flx
  :config (company-flx-mode 1))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package magit
  :commands magit-status)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package yasnippet
  :config (yas-global-mode 1))



(global-eldoc-mode)

(use-package flycheck)

