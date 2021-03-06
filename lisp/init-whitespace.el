(setq-default show-trailing-whitespace nil)


;;; Whitespace

(defun sanityinc/no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook
                org-mode-hook
                ))
  (add-hook hook #'sanityinc/no-trailing-whitespace))

(require-package 'whitespace-cleanup-mode)
(setq whitespace-cleanup-mode-only-if-initially-clean nil)
(global-whitespace-cleanup-mode t)
(diminish 'whitespace-cleanup-mode)

(after-load 'whitespace-cleanup-mode
  (dolist (mode '(markdown-mode
                  special-mode
                  view-mode
                  comint-mode
                  cider-repl-mode
                  haskell-interactive-mode
                  org-mode
                  which-key-mode))
    (add-to-list 'whitespace-cleanup-mode-ignore-modes mode)))

(global-set-key [remap just-one-space] 'cycle-spacing)


(provide 'init-whitespace)
