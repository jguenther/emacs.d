(require-package 'diff-hl)

(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

(defun tak/add-diff-hl-toggle-command ()
  (define-key tak/vc-toggle-map "d" 'diff-hl-mode))

(add-hook 'prog-mode-hook 'tak/add-diff-hl-toggle-command)
(add-hook 'vc-dir-mode-hook 'tak/add-diff-hl-toggle-command)


(provide 'init-vc)
