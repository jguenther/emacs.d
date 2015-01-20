(require-package 'dired+)
(require-package 'dired-sort)
(require-package 'dired-hacks-utils)
(require-package 'dired-filter)
(require-package 'dired-imenu)
(require-package 'dired-sort-menu)

(setq-default diredp-hide-details-initially-flag nil
              dired-dwim-target t)

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
  (add-hook 'dired-mode-hook
            (lambda () (guide-key/add-local-guide-key-sequence "%"))))

(provide 'init-dired)
