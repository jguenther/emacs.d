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
(global-whitespace-cleanup-mode t)
(diminish 'whitespace-cleanup-mode)

(after-load 'whitespace-cleanup-mode
  (add-to-list 'whitespace-cleanup-mode-ignore-modes 'which-key-mode)
  )

(global-set-key [remap just-one-space] 'cycle-spacing)


(provide 'init-whitespace)
