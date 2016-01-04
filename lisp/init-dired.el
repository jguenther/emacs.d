(require-package 'dired+)
(require-package 'dired-sort)
(require-package 'dired-hacks-utils)
(require-package 'dired-filter)
(require-package 'dired-imenu)
(require-package 'dired-sort-menu)
(require-package 'dired-toggle-sudo)

(setq-default diredp-hide-details-initially-flag nil
              dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(after-load 'dired
  (require 'dired+)
  (require 'dired-sort)
  (require 'dired-hacks-utils)
  (require 'dired-filter)
  (require 'dired-imenu)
  (require 'dired-sort-menu)
  (when (fboundp 'global-dired-hide-details-mode)
    (global-dired-hide-details-mode -1))
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  )

(when (maybe-require-package 'diff-hl)
  (after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode t)
    (add-hook 'dired-mode-hook 'diff-hl-flydiff-mode t)))

(provide 'init-dired)
