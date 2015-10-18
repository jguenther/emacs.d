;;----------------------------------------------------------------------------
;; Fill column indicator
;;----------------------------------------------------------------------------
(require-package 'fill-column-indicator)
(defun sanityinc/prog-mode-fci-settings ()
  (turn-on-fci-mode)
  (when show-trailing-whitespace
    (set (make-local-variable 'whitespace-style) '(face trailing))
    (whitespace-mode 1)))

;;(add-hook 'prog-mode-hook 'sanityinc/prog-mode-fci-settings)

(defun sanityinc/fci-enabled-p ()
  (and (boundp 'fci-mode) fci-mode))

(defvar sanityinc/fci-mode-suppressed nil)

(defun suppress-fci-mode (orig-function &rest args)
  "Suspend fci-mode while popups are visible"
  (let ((fci-enabled (sanityinc/fci-enabled-p)))
    (when fci-enabled
      (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-enabled)
      (turn-off-fci-mode))))
(advice-add #'popup-create :before #'suppress-fci-mode)

(defun restore-fci-mode (orig-function &rest arg)
  "Restore fci-mode when all popups have closed"
  (when (and sanityinc/fci-mode-suppressed
             (null popup-instances))
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))

(advice-add #'popup-delete :after #'restore-fci-mode)

;; Regenerate fci-mode line images after switching themes
(defun recompute-fci-face (orig-function &rest args)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (sanityinc/fci-enabled-p)
        (turn-on-fci-mode)))))
(advice-add #'enable-theme :after #'recompute-fci-face)


(provide 'init-fci)
