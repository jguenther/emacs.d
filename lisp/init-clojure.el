;; See also init-clojure-cider.el

(when (maybe-require-package 'clojure-mode)
  (require-package 'cljsbuild-mode)
  (require-package 'elein)
  (when (maybe-require-package 'cljr-helm)
    (require-package 'clj-refactor)

    (after-load 'clojure
      (define-key clojure-mode-map (kbd "C-c r") cljr-helm))

    (require-package 'clojure-cheatsheet)
    (require-package 'clojure-mode-extra-font-locking)
    (require-package 'clojure-quick-repls)
    (require-package 'inf-clojure)
    )

  (after-load 'clojure-mode
    (add-hook 'clojure-mode-hook 'sanityinc/lisp-setup)
    (add-hook 'clojure-mode-hook 'subword-mode)))


(provide 'init-clojure)
