(require-package 'diff-hl)

;; diff-hl-mode is annoying -- changed to keybinding instead
;(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
;(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

(define-key prog-mode-map [(kbd "C-c d")] 'turn-on-diff-hl-mode)

(provide 'init-vc)
