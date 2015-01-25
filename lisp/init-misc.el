;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------
(add-auto-mode 'tcl-mode "Portfile\\'")
(fset 'yes-or-no-p 'y-or-n-p)

(dolist (hook (if (fboundp 'prog-mode)
                  '(prog-mode-hook ruby-mode-hook)
                '(find-file-hooks)))
  (add-hook hook 'goto-address-prog-mode))

(setq goto-address-mail-face 'link)

(setq-default regex-tool-backend 'perl)

(add-auto-mode 'conf-mode "Procfile")

(defcustom non-exec-script-file-extensions '("pm" "t")
  "A list of script file extensions that should not be made executable.
These are used by `tak/buffer-file-ext-is-noexec-p'.  See also:
`tak/maybe-make-buffer-file-executable-if-script-p'."
  :type '(repeat string)
  :options '("pm" "t")
  :group 'files)

(defun tak/buffer-file-ext-is-exec-p ()
  "Returns non-nil if the buffer has a file and the file either
doesn't have an extension or the extension isn't in
`non-exec-script-file-extensions'. Returns nil otherwise."
  (let* ((file buffer-file-name)
         (ext (if file (file-name-extension file))))
    (if file
        (if ext
            (not (member ext non-exec-script-file-extensions))
          t))))

(defun tak/maybe-make-buffer-file-executable ()
  "Call executable-make-buffer-file-executable-if-script-p
if tak/buffer-file-ext-is-noexec-p returns `nil'."
  (if (tak/buffer-file-ext-is-exec-p)
      (executable-make-buffer-file-executable-if-script-p)))

(add-hook 'after-save-hook
          'tak/maybe-make-buffer-file-executable t nil)

(provide 'init-misc)
