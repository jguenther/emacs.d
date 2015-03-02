;; smart-mode-line

(require-package 'smart-mode-line)

(setq-default
 sml/modified-char "*"
 sml/modified-time-string "Modified: %Y-%m-%d %T."
 sml/show-frame-identification t
 sml/show-file-name nil
 )

(sml/setup)
(sml/apply-theme 'dark)

(provide 'init-smart-mode-line)
