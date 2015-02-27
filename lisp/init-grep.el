(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(when (executable-find "ag")
  (require-package 'ag)
  (require-package 'wgrep-ag)
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "M-?") 'ag-project))

(after-load 'grep
					; avoid rgrep matching ELPA archives
  (add-to-list 'grep-find-ignored-directories
	       (join-path user-emacs-directory "elpa" "archives")))

(provide 'init-grep)
