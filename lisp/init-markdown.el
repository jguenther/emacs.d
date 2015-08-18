(require-package 'markdown-mode)

(after-load 'whitespace-cleanup-mode
  (push 'markdown-mode whitespace-cleanup-mode-ignore-modes))


(require-package 'mkdown)
(after-load 'markdown-mode
  (setq-default markdown-css-paths mkdown-css-file-name))

(provide 'init-markdown)
