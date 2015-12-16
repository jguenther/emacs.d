(require-package 'go-mode)
(require 'go-mode-autoloads)

(require-package 'go-dlv)
(require-package 'go-stacktracer)
(require-package 'go-rename)
(require-package 'go-scratch)
(require-package 'go-eldoc)
(require-package 'go-errcheck)
(require-package 'go-complete)

;; (require-package 'go-autocomplete)

(require-package 'go-snippets)
(require-package 'go-direx)
(require-package 'go-projectile)

(after-load 'exec-path-from-shell
  (dolist (var '("GOPATH"))
    (add-to-list 'exec-path-from-shell-variables var)))

(after-load 'go-mode
  (require 'go-complete)
  (add-hook 'completion-at-point-functions 'go-complete-at-point)

  ;; (require 'go-autocomplete)
  ;; (require 'auto-complete-config)
  ;; (ac-config-default)

  (require 'go-direx)
  (require 'go-dlv)
  (require 'go-eldoc)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (require 'go-errcheck)
  (require 'go-projectile)
  (require 'go-rename)
  (require 'go-scratch)
  (require 'go-snippets)
  (require 'go-stacktracer))


(provide 'init-golang)
