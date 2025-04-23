(tool-bar-mode -1)             


 (use-package emacs
  :init
  (set-face-attribute 'default nil
    :font "Hack Nerd Font Mono"
    :height 180))

(use-package diff-hl
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

;; SPC and ' will open terminal. Press again to bg it.
(use-package vterm-toggle)

(use-package emacs
  :init
  (setq-default fill-column 80)
  (set-face-attribute 'fill-column-indicator nil
                      :foreground "#717C7C" ; katana-gray
                      :background "transparent")
  (global-display-fill-column-indicator-mode 1))

(use-package emacs
  :config
  (setq backup-directory-alist `(("." . "~/.saves"))))

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
