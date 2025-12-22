;;; Set up the package manager

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;;; Basic behaviour

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

;;; Tweak the looks of Emacs
 
;; Those three belong in the early-init.el, but I am putting them here
;; for convenience.  If the early-init.el exists in the same directory
;; as the init.el, then Emacs will read+evaluate it before moving to
;; the init.el.
(menu-bar-mode 1)
(tool-bar-mode -1)

;; part after :family is wrong for default, fixed-pitch 
;; (let ((mono-spaced-font "Hack Nerd Font Mono")
;;       (proportionately-spaced-font "FiraCode Nerd Font"))
;;   (set-face-attribute 'default nil :family Hack Nerd Font Mono :height 100)
;;   (set-face-attribute 'fixed-pitch nil :family Hack Nerd Font Mono :height 1.0)
;;   (set-face-attribute 'variable-pitch nil :family Hack Nerd Font Propo :height 1.0))

;; (use-package modus-themes
;;   :ensure t
;;   :config
;;   (load-theme 'modus-vivendi-tinted :no-confirm-loading))

;; Remember to do M-x and run `nerd-icons-install-fonts' to get the
;; font files.  Then restart Emacs to see the effect.
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;;; Configure the minibuffer and completions

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))

(use-package savehist
  :ensure nil ; it is built-in
  :hook (after-init . savehist-mode))

;;; The file manager (Dired)

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

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
   '("088cd6f894494ac3d4ff67b794467c2aa1e3713453805b93a8bcb2d72a0d1b53"
     "fffef514346b2a43900e1c7ea2bc7d84cbdd4aa66c1b51946aade4b8d343b55a"
     "0adcffc4894e2dd21283672da7c3d1025b5586bcef770fdc3e2616bdb2a771cd"
     "a9028cd93db14a5d6cdadba789563cb90a97899c4da7df6f51d58bb390e54031"
     "7771c8496c10162220af0ca7b7e61459cb42d18c35ce272a63461c0fc1336015"
     "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
     "c5975101a4597094704ee78f89fb9ad872f965a84fb52d3e01b9102168e8dc40"
     "7235b77f371f46cbfae9271dce65f5017b61ec1c8687a90ff30c6db281bfd6b7"
     "b4b5da90759ab14719f1e1d8d0138ff72d2901e8a63748b172944627513cfffb"
     "ba4f725d8e906551cfab8c5f67e71339f60fac11a8815f51051ddb8409ea6e5c"
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
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3")
 '(org-agenda-files '("~/project/org/3.org"))
 '(org-faces-easy-properties
   '((todo . :background) (tag . :foreground) (priority . :foreground)))
 '(org-id-locations-file "$HOME/.cache/emacs/var/org/id-locations.el" t)
 '(org-startup-folded 'fold)
 '(org-tempo-keywords-alist nil)
 '(package-selected-packages
   '(0blayout 0xc 2048-game all-the-icons all-the-icons-nerd-fonts
              auto-compile bind-key cargo cargo-mode
              color-theme-sanityinc-tomorrow company-box consult
              consult-denote dashboard denote diffview dired-single
              dired-subtree doom-modeline eglot elpy
              exec-path-from-shell external-completion flycheck
              flycheck-pyflakes flycheck-rust helpful jsonrpc kkp
              lsp-ui lua-mode magit marginalia material-theme
              modus-themes nerd-icons-completion nerd-icons-dired
              no-littering ob-rust orderless org-bullets org-roam
              paredit projectile rainbow-delimiters rustic seq
              show-font slime smartparens toml-mode track-changes
              trashed tree-sitter-langs treemacs treesit-auto
              use-package vertico vterm which-key))
 '(savehist-additional-variables '(kill-ring register-alist\ ) t)
 '(sort-fold-case t)
 '(warning-suppress-log-types '((use-package))))
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

