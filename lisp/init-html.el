(require-package 'tidy)

(require-package 'tagedit)
(after-load 'sgml-mode
  (tagedit-add-paredit-like-keybindings)
  (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1)))
  (add-hook #'sgml-mode-hook #'linum-mode)
  )

(add-auto-mode 'html-mode "\\.\\(jsp\\|tmpl\\)\\'")

(after-load 'html-mode
  (add-hook #'html-mode-hook #'linum-mode)
  (add-hook #'html-mode-hook (lambda () (tidy-build-menu html-mode-map)))
  )


;; Note: ERB is configured in init-ruby-mode

(provide 'init-html)
