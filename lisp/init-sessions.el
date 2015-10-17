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

(require-package 'session)

(setq session-save-file (expand-file-name ".session" user-emacs-directory))
(setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
(add-hook 'after-init-hook 'session-initialize)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((comint-input-ring        . 50)
                (compile-history          . 30)
                desktop-missing-file-warning
                (dired-regexp-history     . 20)
                (extended-command-history . 30)
                (face-name-history        . 20)
                (file-name-history        . 100)
                (grep-find-history        . 30)
                (grep-history             . 30)
                (ido-buffer-history       . 100)
                (ido-last-directory-list  . 100)
                (ido-work-directory-list  . 100)
                (ido-work-file-list       . 100)
                (magit-read-rev-history   . 50)
                (minibuffer-history       . 50)
                (org-clock-history        . 50)
                (org-refile-history       . 50)
                (org-tags-history         . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                register-alist
                (search-ring              . 20)
                (shell-command-history    . 50)
                tak/last-makefile-target
                (tak/makefile-hist        . 20)
                tags-file-name
                tags-table-list)))

(after-load 'session
 (setq-default session-set-file-name-exclude-regexp
              (concat session-set-file-name-exclude-regexp
                      "\\|\\.emacs\\.d/elpa/"
                      "\\|/\\.git/\\(COMMIT\\|MERGE\\)_\\(EDIT\\)?MSG")))

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
