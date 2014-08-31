;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------
(add-auto-mode 'tcl-mode "Portfile\\'")
(fset 'yes-or-no-p 'y-or-n-p)

(dolist (hook (if (fboundp 'prog-mode)
                  '(prog-mode-hook ruby-mode-hook)
                '(find-file-hooks)))
  (add-hook hook 'goto-address-prog-mode))

(defun tak/file-is-pm-or-t ()
  "Tests whether the current buffer has an associated file and
   that the file is a .pm file"
  (and (not (eq nil (buffer-file-name)))
       (or (string-equal "pm" (file-name-extension (buffer-file-name)))
           (string-equal "t" (file-name-extension (buffer-file-name))))))

(defun tak/maybe-make-buffer-file-executable ()
  "Call executable-make-buffer-file-executable-if-script-p unless
  the file has certain extensions"
  (if (not (tak/file-is-pm-or-t))
      (executable-make-buffer-file-executable-if-script-p)))

;(add-hook 'after-save-hook 'tak/maybe-make-buffer-file-executable)

(setq goto-address-mail-face 'link)

(setq-default regex-tool-backend 'perl)

(add-auto-mode 'conf-mode "Procfile")


(provide 'init-misc)
