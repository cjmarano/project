(deftheme ducky
  "My-Theme- 2019-11-13.")

(custom-theme-set-variables
 'ducky
 '(add-to-list (quote default-frame-alist))
 '(font-lock-global-modes (quote (not speedbar-mode)))
 '(package-selected-packages (quote (rustic rust-mode rum-mode markdown-mode projectile f s dash xterm-color)))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(blink-cursor-mode nil)
 '(display-time-mode t)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(uniquify-buffer-name-style (quote forward)))

(provide-theme 'ducky)
