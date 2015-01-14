(require 'help-mode)

(defun help-mode-revert-buffer-noconfirm (ignore-auto noconfirm)
  "Always revert `help-mode' buffers without confirmation."
  (help-mode-revert-buffer ignore-auto t))

(add-hook 'help-mode-hook
          (lambda ()
            (define-key help-mode-map [(meta left)] 'help-go-back)
            (define-key help-mode-map [(meta right)] 'help-go-forward)
            (set (make-local-variable 'revert-buffer-function)
                 'help-mode-revert-buffer-noconfirm)))

;;; describe this point lisp only
;;
;; http://www.emacswiki.org/emacs/DescribeThingAtPoint
(defun describe-foo-at-point ()
  "Show the documentation of the Elisp function and variable near point.
   This checks in turn:
     -- for a function name where point is
     -- for a variable name where point is
     -- for a surrounding function call"
  (interactive)
  (let (sym)
    ;; sigh, function-at-point is too clever.  we want only the first half.
    (cond ((setq sym (ignore-errors
                       (with-syntax-table emacs-lisp-mode-syntax-table
                         (save-excursion
                           (or (not (zerop (skip-syntax-backward "_w")))
                               (eq (char-syntax (char-after (point))) ?w)
                               (eq (char-syntax (char-after (point))) ?_)
                               (forward-sexp -1))
                           (skip-chars-forward "`'")
                           (let ((obj (read (current-buffer))))
                             (and (symbolp obj) (fboundp obj) obj))))))
           (describe-function sym))
          ((setq sym (variable-at-point)) (describe-variable sym))
          ;; now let it operate fully -- i.e. also check the
          ;; surrounding sexp for a function call.
          ((setq sym (function-at-point)) (describe-function sym)))))

(global-set-key "\C-hV" 'describe-foo-at-point)


(provide 'init-help)
