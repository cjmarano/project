(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("elpa"  . "https://elpa.gnu.org/package/s")t)
(add-to-list 'package-archives
	     '("nongnu" . "https://elpa.nongnu.org/nongnu/")t)

(package-initialize)

(defun packages-require (&rest packs)
  "Install and load a package. If the package is not available
   installs it automaticaly."
  (mapc  (lambda (package)
           (unless (package-installed-p package)
                   (package-install package)
                   )
	   )

         packs

         ))


(defvar myPackages
  '(
    elpy                            ;; Emacs Lisp Python Environment
    flycheck                        ;; On the fly syntax checking
    )
  )

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org")
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

(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
	    (paredit-mode t)
            (rainbow-delimiters-mode t)
	    (show-paren-mode 1)
            ))

(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path 
        (append
         (split-string-and-unquote path ":")
         exec-path)))

(setq mac-command-modifier 'control)

;; Rust stuff goes here.
(require 'rust-mode)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(add-hook 'rust-mode-hook
          (lambda () (prettify-symbols-mode)))

;; Test -rmove if it breaks org
(setq load-path (cons "~/Build/Emacs/org-mode/lisp" load-path))
(setq load-path (cons "~/Build/Emacs/org-mode/contrib/lisp" load-path))
;; End test code -- added to this .emacs on 18MAY2022

;; test to suppress ls does not support dired.
(setq ls-lisp-use-insert-directory-program nil)
 (require 'ls-lisp)

(setq python-python-command "/Users/charles.marano/.pyenv/versions/3.10.3/bin/python")
(setq python-shell-completion-native-enable nil)

(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
;; font-lock-mode set on 20SEP2022
(global-font-lock-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(transient-mark-mode t)
(global-linum-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)


'(python-shell-interpeter "users/charles.marano/.pyenv/shims/python")

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; ====================================
;; Development Setup
;; ====================================
;; Enable elpy
(elpy-enable)

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
;; added to supress flymake error message when compliing python (12AUG2022)
(remove-hook 'flymake-diagnostic-funtions 'flymake-proc-legacy-flymake)

;; User-Defined init.el ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes '(material))
 '(custom-safe-themes
   '("db86c52e18460fe10e750759b9077333f9414ed456dc94473f9cf188b197bc74" "70cfdd2e7beaf492d84dfd5f1955ca358afb0a279df6bd03240c2ce74a578e9e" default))
 '(elpy-rpc-python-command "python3")
 '(package-selected-packages
   '(display-theme material-theme rust-mode use-package org-roam htmlize elpy buttercup org magit org-roam zenburn-theme rust-mode flycheck elpy rainbow-delimiters powerline dash pkg-info use-package diffview)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 150 :family "Hack")))))
