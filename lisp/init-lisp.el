(require-package 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))
(after-load 'elisp-slime-nav
  (diminish 'elisp-slime-nav-mode))
(require-package 'lively)



;; highlight sexps under evaluation
(require-package 'eval-sexp-fu)
(require 'eval-sexp-fu)

(setq-default eval-sexp-fu-flash-face 'highlight-symbol-face)

(dolist (hook '(lisp-mode-hook
                inferior-lisp-mode-hook
                emacs-lisp-mode-hook))
  (add-hook hook #'turn-on-eval-sexp-fu-flash-mode))


;; Make C-x C-e run 'eval-region if the region is active

(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (let ((start (min (point) (mark)))
            (end   (max (point) (mark))))
        (prog1
            (eval-region start end)
          (eval-sexp-fu-flash '(start . end))))
    (pp-eval-last-sexp prefix)))

(define-eval-sexp-fu-flash-command pp-eval-last-sexp
  (eval-sexp-fu-flash (when (ignore-errors (preceding-sexp))
                        (with-esf-end-of-sexp
                          (bounds-of-thing-at-point 'sexp)))))

(global-set-key (kbd "M-:") 'pp-eval-expression)

(after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'sanityinc/eval-last-sexp-or-region)
  (define-key emacs-lisp-mode-map (kbd "C-x M-p") 'eval-print-last-sexp))

(defun sanityinc/make-read-only (orig-function &rest args)
  "Enable `view-mode' in the output buffer - if any - so it can be closed with `\"q\"."
  (let* ((expression (car args))
         (out-buffer-name (cadr args)))
    (message (buffer-file-name))
    (apply orig-function args)
    (when (get-buffer out-buffer-name)
      (with-current-buffer out-buffer-name
        (view-mode 1)))))
(advice-add #'pp-display-expression :around #'sanityinc/make-read-only)



(defun sanityinc/maybe-set-bundled-elisp-readonly ()
  "If this elisp appears to be part of Emacs, then disallow editing."
  (when (and (buffer-file-name)
             (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
    (setq buffer-read-only t)
    (view-mode 1)))

(add-hook 'emacs-lisp-mode-hook 'sanityinc/maybe-set-bundled-elisp-readonly)


;; Use C-c C-z to toggle between elisp files and an ielm session
;; I might generalise this to ruby etc., or even just adopt the repl-toggle package.

(defvar sanityinc/repl-original-buffer nil
  "Buffer from which we jumped to this REPL.")
(make-variable-buffer-local 'sanityinc/repl-original-buffer)

(defvar sanityinc/repl-switch-function 'switch-to-buffer-other-window)

(defun sanityinc/switch-to-ielm ()
  (interactive)
  (let ((orig-buffer (current-buffer)))
    (if (get-buffer "*ielm*")
        (funcall sanityinc/repl-switch-function "*ielm*")
      (ielm))
    (setq sanityinc/repl-original-buffer orig-buffer)))

(defun sanityinc/repl-switch-back ()
  "Switch back to the buffer from which we reached this REPL."
  (interactive)
  (if sanityinc/repl-original-buffer
      (funcall sanityinc/repl-switch-function sanityinc/repl-original-buffer)
    (error "No original buffer.")))

(after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'sanityinc/switch-to-ielm))
(after-load 'ielm
  (define-key ielm-map (kbd "C-c C-z") 'sanityinc/repl-switch-back))

;; ----------------------------------------------------------------------------
;; Hippie-expand
;; ----------------------------------------------------------------------------

(defun my/emacs-lisp-module-name ()
  "Search the buffer for `provide' declaration."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp "^(provide '" nil t)
      (symbol-name (symbol-at-point)))))

;; Credit to Chris Done for this one.
(defun my/try-complete-lisp-symbol-without-namespace (old)
  "Hippie expand \"try\" function which expands \"-foo\" to \"modname-foo\" in elisp."
  (unless old
    (he-init-string (he-lisp-symbol-beg) (point))
    (when (string-prefix-p "-" he-search-string)
      (let ((mod-name (my/emacs-lisp-module-name)))
        (when mod-name
          (setq he-expand-list (list (concat mod-name he-search-string)))))))
  (when he-expand-list
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list nil)
    t))

(defun set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t)
  (add-to-list 'hippie-expand-try-functions-list 'my/try-complete-lisp-symbol-without-namespace t))


;; ----------------------------------------------------------------------------
;; Automatic byte compilation
;; ----------------------------------------------------------------------------
(when (maybe-require-package 'auto-compile)
  (auto-compile-on-save-mode 1)
  (auto-compile-on-load-mode 1))

(setq auto-compile-display-buffer nil)
(setq auto-compile-mode-line-counter t)

;; ----------------------------------------------------------------------------
;; Load .el if newer than corresponding .elc
;; ----------------------------------------------------------------------------
(setq load-prefer-newer t)

;; ----------------------------------------------------------------------------
;; Highlight current sexp
;; ----------------------------------------------------------------------------

(require-package 'hl-sexp)

;; Prevent flickery behaviour due to hl-sexp-mode unhighlighting before each command
(defun hl-sexp-unflicker (orig-function &rest args)
  (when turn-on
    (remove-hook 'pre-command-hook #'hl-sexp-unhighlight)))

(after-load 'hl-sexp
  (advice-add #'hl-sexp-mode :after #'hl-sexp-unflicker))


;;; Support byte-compilation in a sub-process, as
;;; required by highlight-cl

(defun sanityinc/byte-compile-file-batch (filename)
  "Byte-compile FILENAME in batch mode, ie. a clean sub-process."
  (interactive "fFile to byte-compile in batch mode: ")
  (let ((emacs (car command-line-args)))
    (compile
     (concat
      emacs " "
      (mapconcat
       'shell-quote-argument
       (list "-Q" "-batch" "-f" "batch-byte-compile" filename)
       " ")))))


;; ----------------------------------------------------------------------------
;; Enable desired features for all lisp modes
;; ----------------------------------------------------------------------------
(require-package 'rainbow-delimiters)
(require-package 'redshank)
(after-load 'redshank
  (diminish 'redshank-mode))

(require-package 'ipretty)
(maybe-require-package 'aggressive-indent)

(defun sanityinc/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (rainbow-delimiters-mode t)
  (enable-paredit-mode)
  (when (fboundp 'aggressive-indent-mode)
    (aggressive-indent-mode)
    (diminish 'aggressive-indent-mode))
  (turn-on-eldoc-mode)
  (redshank-mode)
  (ipretty-mode 1)
  (add-hook 'after-save-hook #'check-parens nil t))

(defun sanityinc/emacs-lisp-setup ()
  "Enable features useful when working with elisp."
  (set-up-hippie-expand-for-elisp)
  (ac-emacs-lisp-mode-setup))

(defconst sanityinc/elispy-modes
  '(emacs-lisp-mode ielm-mode)
  "Major modes relating to elisp.")

(defconst sanityinc/lispy-modes
  (append sanityinc/elispy-modes
          '(lisp-mode inferior-lisp-mode lisp-interaction-mode))
  "All lispy major modes.")

(require 'derived)

(dolist (hook (mapcar #'derived-mode-hook-name sanityinc/lispy-modes))
  (add-hook hook 'sanityinc/lisp-setup))

(dolist (hook (mapcar #'derived-mode-hook-name sanityinc/elispy-modes))
  (add-hook hook 'sanityinc/emacs-lisp-setup))

(if (boundp 'eval-expression-minibuffer-setup-hook)
    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  (require-package 'eldoc-eval)
  (require 'eldoc-eval)
  (eldoc-in-minibuffer-mode 1))

(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))

(require-package 'cl-lib-highlight)
(after-load 'lisp-mode
  (cl-lib-highlight-initialize))

;; ----------------------------------------------------------------------------
;; Delete .elc files when reverting the .el from VC or magit
;; ----------------------------------------------------------------------------

;; When .el files are open, we can intercept when they are modified
;; by VC or magit in order to remove .elc files that are likely to
;; be out of sync.

;; This is handy while actively working on elisp files, though
;; obviously it doesn't ensure that unopened files will also have
;; their .elc counterparts removed - VC hooks would be necessary for
;; that.

(defvar sanityinc/vc-reverting nil
  "Whether or not VC or Magit is currently reverting buffers.")

(defun sanityinc/maybe-remove-elc (orig-function &rest args)
  "If reverting from VC, delete any .elc file that will now be out of sync."
  (apply orig-function args)
  (when sanityinc/vc-reverting
    (when (and (eq 'emacs-lisp-mode major-mode)
               buffer-file-name
               (string= "el" (file-name-extension buffer-file-name)))
      (let ((elc (concat buffer-file-name "c")))
        (when (file-exists-p elc)
          (message "Removing out-of-sync elc file %s" (file-name-nondirectory elc))
          (delete-file elc))))))
(advice-add #'revert-buffer :around #'sanityinc/maybe-remove-elc)

(defun sanityinc/reverting (orig-function &rest args)
  (let ((sanityinc/vc-reverting t))
    (apply orig-function args)))

(advice-add #'magit-revert-buffers :around #'sanityinc/reverting)
(advice-add #'vc-revert-buffer-internal :around #'sanityinc/reverting)


(require-package 'macrostep)

(after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand))



;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "C-h K") 'find-function-on-key)



(when (maybe-require-package 'rainbow-mode)
  (defun sanityinc/enable-rainbow-mode-if-theme ()
    (when (string-match "\\(color-theme-\\|-theme\\.el\\)" (buffer-name))
      (rainbow-mode 1)))

  (add-hook 'emacs-lisp-mode-hook 'sanityinc/enable-rainbow-mode-if-theme))

(when (maybe-require-package 'highlight-quoted)
  (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))


(when (maybe-require-package 'flycheck)
  (require-package 'flycheck-package)
  (after-load 'flycheck
    (flycheck-package-setup)))



;; ERT
(after-load 'ert
  (define-key ert-results-mode-map (kbd "g") 'ert-results-rerun-all-tests))


(defun sanityinc/cl-libify-next ()
  "Find next symbol from 'cl and replace it with the 'cl-lib equivalent."
  (interactive)
  (let ((case-fold-search nil))
    (re-search-forward
     (concat
      "("
      (regexp-opt
       ;; Not an exhaustive list
       '("loop" "incf" "plusp" "first" "decf" "minusp" "assert"
         "case" "destructuring-bind" "second" "third" "defun*"
         "defmacro*" "return-from" "labels" "cadar" "fourth"
         "cadadr") t)
      "\\_>")))
  (let ((form (match-string 1)))
    (backward-sexp)
    (cond
     ((string-match "^\\(defun\\|defmacro\\)\\*$")
      (kill-sexp)
      (insert (concat "cl-" (match-string 1))))
     (t
      (insert "cl-")))
    (when (fboundp 'aggressive-indent-indent-defun)
      (aggressive-indent-indent-defun))))



(defun display-prefix (arg)
  "Display the value of the raw prefix arg."
  (interactive "P")
  (message "%s" arg))



(provide 'init-lisp)
