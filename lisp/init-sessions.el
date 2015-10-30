;; save a list of open files in ~/.emacs.d/.emacs.desktop
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600)

;; only save desktop when in graphical mode, (i.e. don't save in -nw)
(if (display-graphic-p)
    (desktop-save-mode 1)
  (desktop-save-mode 0))

(defun time-desktop-read (orig-function &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig-function args)
      (message "Desktop restored in %.2fms"
               (sanityinc/time-subtract-millis (current-time)
                                               start-time)))))
(advice-add #'desktop-read :around #'time-desktop-read)

;;----------------------------------------------------------------------------
;; Restore histories and registers after saving
;;----------------------------------------------------------------------------
(setq-default history-length 1000)
(savehist-mode t)

(require-package 'psession)
(add-hook 'after-init-hook #'psession-mode)

(after-load 'psession
  (setq psession-object-to-save-alist
        (append '((comint-input-ring . "comint-input-ring.el")
                  (compile-history . "compile-history.el")
                  (desktop-missing-file-warning . "esktop-missing-file-warning.el")
                  (extended-command-history . "extended-command-history.el")
                  (face-name-history . "face-name-history.el")
                  (file-name-history . "file-name-history.el")
                  (grep-find-history . "grep-find-history.el")
                  (grep-history . "grep-history.el")
                  (magit-read-rev-history . "magit-read-rev-history.el")
                  (minibuffer-history . "minibuffer-history.el")
                  (org-clock-history . "org-clock-history.el")
                  (org-refile-history . "org-refile-history.el")
                  (org-tags-history . "org-tags-history.el")
                  (query-replace-history . "query-replace-history.el")
                  (read-expression-history . "read-expression-history.el")
                  (regexp-history . "regexp-history.el")
                  (regexp-search-ring . "regexp-search-ring.el")
                  (register-alist . "egister-alist.el")
                  (shell-command-history . "shell-command-history.el")
                  (tak/last-makefile-target . "tak.el")
                  (tak/makefile-hist        . "tak.el")
                  (tags-file-name . "tags-file-name.el")
                  (tags-table-list . "tags-table-list.el")
                  (dired-regexp-history . "dired-regexp-history.el")
                  (search-ring . "search-ring.el"))
                psession-object-to-save-alist)

        psession-save-buffers-unwanted-buffers-regexp
        ".*[.]org$\\|diary$\\|[.]newsticker-cache$\\|[.]dir-locals.el$"))

(after-load 'desktop
  (dolist (mode '(
                  shell-mode
                  eshell-mode
                  paradox-menu-mode
                  realgud-short-key-mode
                  git-timemachine-mode
                  ))
    (add-to-list 'desktop-modes-not-to-save mode)))

(provide 'init-sessions)
