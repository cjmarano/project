(use-package emacs
    :init
    (tool-bar-mode -1)
    (when scroll-bar-mode
      (scroll-bar-mode -1))
    (load-theme 'wombat)
    (set-face-attribute 'default nil :font "CaskaydiaCove Nerd Font Mono" :height 160)
    (fido-vertical-mode)
    :config
    (setq treesit-language-source-alist
          '((ruby "https://github.com/tree-sitter/tree-sitter-ruby"))))

  (use-package ruby-ts-mode
    :mode "\\.rb\\'"
    :mode "Rakefile\\'"
    :mode "Gemfile\\'"
    :hook (ruby-ts-mode . subword-mode)
    :bind (:map ruby-ts-mode-map
                ("C-c r b" . treesit-beginning-of-defun)
                ("C-c r e" . treesit-end-of-defun))
    :custom
    (ruby-indent-level 2)
    (ruby-indent-tabs-mode nil))

  (use-package eldoc
    :init
    (global-eldoc-mode))

  (use-package eglot
    :hook (prog-mode . eglot-ensure)
    :init
    (setq eglot-stay-out-of '(flymake))
    :bind (:map
           eglot-mode-map
           ("C-c c a" . eglot-code-actions)
           ("C-c c o" . eglot-code-actions-organize-imports)
           ("C-c c r" . eglot-rename)
           ("C-c c f" . eglot-format)))

  (use-package flymake
    :hook (prog-mode . flymake-mode)
    :bind (:map flymake-mode-map
                ("C-c ! n" . flymake-goto-next-error)
                ("C-c ! p" . flymake-goto-prev-error)
                ("C-c ! l" . flymake-show-buffer-diagnostics)))
