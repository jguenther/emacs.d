(setq-default compilation-scroll-output t)

(require-package 'alert)

;; Customize `alert-default-style' to get messages after compilation

(defun sanityinc/alert-after-compilation-finish (buf result)
  "Use `alert' to report compilation RESULT if BUF is hidden."
  (unless (catch 'is-visible
            (walk-windows (lambda (w)
                            (when (eq (window-buffer w) buf)
                              (throw 'is-visible t))))
            nil)
    (alert (concat "Compilation " result)
           :buffer buf
           :category 'compilation)))

(after-load 'compile
  (add-hook 'compilation-finish-functions
            'sanityinc/alert-after-compilation-finish))

(defcustom compilation-makefile-filenames '("Makefile" "makefile" "GNUmakefile")
  "A list of compilation file (Makefile) filenames.
   These are used by `tak/get-nearest-compilation-file' and
  `tak/compile-nearest-compilation-file'."
  :type '(repeat string)
  :options '("Makefile" "makefile" "GNUmakefile")
  :group 'compilation)

(defun tak/get-nearest-compilation-file ()
  "Recursively search for the nearest Makefile up the directory tree.
   Search starts at default-directory and continues until a
   compilation file is found or until the root directory is
   reached. Compilation filenames for which to search are set in
   `compilation-makefile-filenames'."
  (let ((dir default-directory)
        (parent-dir (file-name-directory (directory-file-name default-directory)))
        (nearest-compilation-file nil))
    (while (and (not (string= dir parent-dir))
                (not nearest-compilation-file))
      (dolist (filename compilation-makefile-filenames)
        (setq file-path (concat dir filename))
        (when (file-readable-p file-path)
          (setq nearest-compilation-file file-path)))
      (setq dir parent-dir
            parent-dir (file-name-directory (directory-file-name parent-dir))))
    nearest-compilation-file))

(defvar tak/last-makefile-target "Last target executed by make command")
(defvar tak/makefile-hist "Makefile target history")

(defun tak/compile-nearest-compilation-file (&optional target)
  "Runs compile on the nearest compilation file.
   See `tak/get-nearest-compilation-file' for details."
  (interactive "P")

  (let ((nearest-compilation-file-result (tak/get-nearest-compilation-file)))
    (if nearest-compilation-file-result
        (let* ((compilation-command-default
                (format "make -C %s -f %s"
                        (file-name-directory nearest-compilation-file-result)
                        nearest-compilation-file-result))
               (target-exclude-regexp "\\(^\\.\\)\\|[\\$\\%]")
               ;; from http://danamlund.dk/emacs/make-runner.html
               (makefile-targets
                (with-temp-buffer
                  (insert-file-contents nearest-compilation-file-result)
                  (goto-char (point-max))
                  (let ((targets nil))
                    (while (re-search-backward "^\\([^:\n#[:space:]]+?\\):"
                                               (not 'bound) 'noerror)
                      (unless (string-match target-exclude-regexp
                                            (match-string 1))
                        (setq targets (cons (match-string 1) targets))))
                    targets)))
               (makefile-target
                (if current-prefix-arg
                    (completing-read
                     "Target: "
                     makefile-targets
                     nil nil tak/last-makefile-target
                     'tak/makefile-hist)
                  target))
               (compilation-command
                (if makefile-target
                    (format "%s %s" compilation-command-default makefile-target)
                  compilation-command-default)))
          (compile compilation-command)
          (setq tak/last-makefile-target makefile-target))
      (error "No suitable compilation file found."))))

(defun tak/print-nearest-compilation-file ()
  "Displays a message containing the nearest compilation filename."
  (interactive)
  (message "Nearest compilation filename: %s" (tak/get-nearest-compilation-file)))

(defvar sanityinc/last-compilation-buffer nil
  "The last buffer in which compilation took place.")

(defun sanityinc/save-compilation-buffer (orig-function &rest args)
  "Save the compilation buffer to find it later."
  (setq sanityinc/last-compilation-buffer next-error-last-buffer))

(defun sanityinc/find-prev-compilation (orig-function &rest args)
  "Find the previous compilation buffer, if present, and recompile there."
  (let* ((edit-command (car args)))
    (if (and (null edit-command)
             (not (derived-mode-p 'compilation-mode))
             sanityinc/last-compilation-buffer
             (buffer-live-p (get-buffer sanityinc/last-compilation-buffer)))
        (with-current-buffer sanityinc/last-compilation-buffer
          (apply orig-function args))
      (apply orig-function args))))

(after-load 'compile
  (advice-add #'compilation-start :after #'sanityinc/save-compilation-buffer)
  (advice-add #'recompile :around #'sanityinc/find-prev-compilation)
  )

(global-set-key [f5] 'tak/compile-nearest-compilation-file)
(global-set-key (kbd "S-<f5>") 'tak/print-nearest-compilation-file)
(global-set-key [f7] 'recompile)

(defun sanityinc/shell-command-in-view-mode (orig-function &rest args)
  "Put \"*Shell Command Output*\" buffers into view-mode."
  (unless output-buffer
    (with-current-buffer "*Shell Command Output*"
      (view-mode 1))))
(advice-add #'shell-command-on-region :after #'sanityinc/shell-command-in-view-mode)

(after-load 'compile
  (require 'ansi-color)
  (defun sanityinc/colourise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'sanityinc/colourise-compilation-buffer))


(provide 'init-compile)
