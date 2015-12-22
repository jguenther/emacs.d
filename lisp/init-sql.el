(require-package 'sql-indent)
(after-load 'sql
  (require 'sql-indent))

(defun sanityinc/pop-to-sqli-buffer ()
  "Switch to the corresponding sqli buffer."
  (interactive)
  (if sql-buffer
      (progn
        (pop-to-buffer sql-buffer)
        (goto-char (point-max)))
    (sql-set-sqli-buffer)
    (when sql-buffer
      (sanityinc/pop-to-sqli-buffer))))

(defun sanityinc/maybe-set-dash-db-docset (&rest args)
  (when (and
         (sanityinc/dash-installed-p)
         sql-product
         (eq sql-product 'postgres))
    (set (make-local-variable 'dash-at-point-docset) "psql")))

(after-load 'sql
  (define-key sql-mode-map (kbd "C-c C-z") 'sanityinc/pop-to-sqli-buffer)
  (add-hook 'sql-interactive-mode-hook 'sanityinc/never-indent)
  (when (package-installed-p 'dash-at-point)
    (add-hook 'sql-mode-hook 'sanityinc/maybe-set-dash-db-docset)
    (add-hook 'sql-interactive-mode-hook 'sanityinc/maybe-set-dash-db-docset)
    (advice-add #'sql-set-product :after #'sanityinc/maybe-set-dash-db-docset)
    ))

(setq-default sql-input-ring-file-name
              (expand-file-name ".sqli_history" user-emacs-directory))

;; See my answer to https://emacs.stackexchange.com/questions/657/why-do-sql-mode-and-sql-interactive-mode-not-highlight-strings-the-same-way/673
(defun sanityinc/font-lock-everything-in-sql-interactive-mode ()
  (unless (eq 'oracle sql-product)
    (sql-product-font-lock nil nil)))
(add-hook 'sql-interactive-mode-hook 'sanityinc/font-lock-everything-in-sql-interactive-mode)


(after-load 'page-break-lines
  (push 'sql-mode page-break-lines-modes))

(provide 'init-sql)
