(defun tak/disable-flycheck-outside-home-dir ()
  "Turns off `flycheck-mode' unless `buffer-file-name' is in the
  user's home directory."

  (if (and (buffer-file-name)
           (not (file-in-directory-p (buffer-file-name) (expand-file-name "~/"))))
      (flycheck-mode -1))
  )

(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)

  ;; Override default flycheck triggers
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.8)

  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

  (setq-default flycheck-temp-prefix (expand-file-name "flycheck/_flycheck_" temporary-file-directory))
  )


(provide 'init-flycheck)
