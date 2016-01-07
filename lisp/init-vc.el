(require-package 'diff-hl)

(diminish 'diff-hl-mode)
(setq diff-hl-command-prefix "^Xu"
      diff-hl-flydiff-delay 0.8)

(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode t)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode t)

(add-hook 'prog-mode-hook 'diff-hl-flydiff-mode t)
(add-hook 'vc-dir-mode-hook 'diff-hl-flydiff-mode t)

(after-load 'diff-hl
  (define-key diff-hl-mode-map (kbd "C-x u =") #'diff-hl-diff-goto-hunk)
  (define-key diff-hl-mode-map (kbd "C-x u v") #'diff-hl-revert-hunk)
  (define-key diff-hl-mode-map (kbd "C-x u p") #'diff-hl-previous-hunk)
  (define-key diff-hl-mode-map (kbd "C-x u n") #'diff-hl-next-hunk)
  )

(provide 'init-vc)
