(require-package 'markdown-mode)

(after-load 'whitespace-cleanup-mode
  (push 'markdown-mode whitespace-cleanup-mode-ignore-modes)
  (push 'gfm-mode whitespace-cleanup-mode-ignore-modes))

(require-package 'mkdown)
(after-load 'markdown-mode
  (setq-default markdown-css-paths mkdown-css-file-name))

(add-to-list 'auto-mode-alist '("README\\.\\(?:md\\|markdown\\)\\'" . gfm-mode))

(dolist (hook '(markdown-mode-hook
                gfm-mode-hook))
  (add-hook hook (lambda ()
                   (setq-local electric-indent-inhibit t))))

(provide 'init-markdown)
