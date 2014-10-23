(setq-default compilation-scroll-output t)

(require-package 'alert)

;; Customize `alert-default-style' to get messages after compilation

(defun sanityinc/alert-after-compilation-finish (buf result)
  "Use `alert' to report compilation RESULT if BUF is hidden."
  (let ((buf-is-visible nil))
    (walk-windows (lambda (w)
                    (when (eq (window-buffer w) buf)
                      (setq buf-is-visible t))))
    (unless buf-is-visible
      (alert (concat "Compilation " result)
             :buffer buf
             :category 'compilation))))

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
        (nearest-compilation-file 'nil))
    (while (and (not (string= dir parent-dir))
                (not nearest-compilation-file))
      (dolist (filename compilation-makefile-filenames)
        (setq file-path (concat dir filename))
        (when (file-readable-p file-path)
          (setq nearest-compilation-file file-path)))
      (setq dir parent-dir
            parent-dir (file-name-directory (directory-file-name parent-dir))))
    nearest-compilation-file))

(defun tak/compile-nearest-compilation-file ()
  "Runs compile on the nearest compilation file.
   See tak/get-nearest-compilation-file for details."
  (interactive)
  (let ((nearest-compilation-file-result (tak/get-nearest-compilation-file)))
    (compile (format "make -C %s -f %s"
                     (file-name-directory nearest-compilation-file-result)
                     nearest-compilation-file-result))))

(defun tak/print-nearest-compilation-file ()
  "Displays a message containing the nearest compilation filename."
  (interactive)
  (message "Nearest compilation filename: %s" (tak/get-nearest-compilation-file)))

(global-set-key [f5] 'tak/compile-nearest-compilation-file)
(global-set-key (kbd "S-<f5>") 'tak/print-nearest-compilation-file)
(global-set-key [f7] 'recompile)

(provide 'init-compile)
