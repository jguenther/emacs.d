(require-package 'markdown-mode)

(setq auto-mode-alist
      (cons '("\\.\\(md\\|markdown\\)\\'" . markdown-mode) auto-mode-alist))

(require-package 'mkdown)
(after-load 'markdown-mode
  (setq-default markdown-css-path mkdown-css-file-name))

(provide 'init-markdown)
