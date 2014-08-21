;; Use C-f during file selection to switch to regular find-file
(require 'ido)

(defvar ido-cur-item nil)

(add-hook 'ido-setup-hook 'ido-my-keys)

(defun ido-my-keys ()
  "Set up the keymap for `ido'."

;;   ;; common keys

;;   ;; keys used in file and dir environment
  (when (memq ido-cur-item '(file dir))
    ; remove remap and map ido-delete-backward-updir to M-backspace
    (define-key ido-completion-map [remap delete-backward-char] nil)
    (define-key ido-completion-map [(meta backspace)] 'ido-delete-backward-updir)
    ))

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length 0)
(setq ido-use-virtual-buffers t)

(when (eval-when-compile (>= emacs-major-version 24))
 (require-package 'ido-ubiquitous)
 (ido-ubiquitous-mode t))

;; Use smex to handle M-x
(when (eval-when-compile (>= emacs-major-version 24))
  (require-package 'smex)
  ;; Change path for ~/.smex-items
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key [remap execute-extended-command] 'smex))

(require-package 'idomenu)

;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

;; http://www.reddit.com/r/emacs/comments/21a4p9/use_recentf_and_ido_together/cgbprem
(add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up] 'previous-history-element)))

(provide 'init-ido)
