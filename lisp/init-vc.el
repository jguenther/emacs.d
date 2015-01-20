(require-package 'diff-hl)

;; diff-hl-mode can be annoying -- change to key in toggle-map instead
(defun tak/add-diff-hl-toggle-command ()
  (define-key tak/vc-toggle-map "d" 'diff-hl-mode))

(add-hook 'prog-mode-hook 'tak/add-diff-hl-toggle-command)
(add-hook 'vc-dir-mode-hook 'tak/add-diff-hl-toggle-command)


(provide 'init-vc)
