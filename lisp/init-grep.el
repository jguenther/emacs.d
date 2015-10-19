(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind")
  (if (executable-find "glocate")
      (setq locate-command "glocate")))


(when (executable-find "ag")
  (require-package 'ag)
  (require-package 'wgrep-ag)
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "M-?") 'ag-project))

(after-load 'grep
					; avoid rgrep matching ELPA archives
  (add-to-list 'grep-find-ignored-directories "elpa/archives")
                                        ; homebrew .cask dir
  (add-to-list 'grep-find-ignored-directories ".cask")
                                        ; coverage testing results files
  (add-to-list 'grep-find-ignored-directories "htmlcov"))


(provide 'init-grep)
