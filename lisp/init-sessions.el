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

(defun tak/psession-but-no-save-buffers-mode ()
  (psession-mode +1)
  (dolist (fn '(psession--dump-some-buffers-to-list
                psession--restore-some-buffers
                psession-save-last-winconf
                psession-restore-last-winconf))
    (remove-hook 'kill-emacs-hook fn)
    (remove-hook 'emacs-startup-hook fn)))

(add-hook 'after-init-hook #'tak/psession-but-no-save-buffers-mode)

(after-load 'psession
  (setq psession-object-to-save-alist
        (append '((comint-input-ring            . "comint-input-ring.el")
                  (compile-history              . "compile-history.el")
                  (dired-regexp-history         . "dired-regexp-history.el")
                  (extended-command-history     . "extended-command-history.el")
                  (face-name-history            . "face-name-history.el")
                  (file-name-history            . "file-name-history.el")
                  (grep-find-history            . "grep-find-history.el")
                  (grep-history                 . "grep-history.el")
                  (magit-revision-history       . "magit-read-rev-history.el")
                  (org-clock-history            . "org-clock-history.el")
                  (org-refile-history           . "org-refile-history.el")
                  (org-tags-history             . "org-tags-history.el")
                  (query-replace-history        . "query-replace-history.el")
                  (read-expression-history      . "read-expression-history.el")
                  (regexp-history               . "regexp-history.el")
                  (regexp-search-ring           . "regexp-search-ring.el")
                  (register-alist               . "register-alist.el")
                  (search-ring                  . "search-ring.el")
                  (shell-command-history        . "shell-command-history.el")
                  (tak/last-makefile-target     . "tak.el")
                  (tak/makefile-hist            . "tak.el")
                  (tags-file-name               . "tags-file-name.el")
                  (tags-table-list              . "tags-table-list.el"))
                psession-object-to-save-alist)

        psession-save-buffers-unwanted-buffers-regexp
        ".*[.]org$\\|diary$\\|[.]newsticker-cache$\\|[.]dir-locals.el$\\|/Projects/\\|/Library/Python/"))

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save '(desktop-missing-file-warning
                                (minibuffer-history . 50)
                                tags-file-name
                                tags-table-list))

(after-load 'desktop
  (dolist (mode '(
                  shell-mode
                  eshell-mode
                  paradox-menu-mode
                  realgud-short-key-mode
                  git-timemachine-mode
                  comint-mode
                  ))
    (add-to-list 'desktop-modes-not-to-save mode)))

(provide 'init-sessions)
