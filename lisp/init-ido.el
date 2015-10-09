;; Use C-f during file selection to switch to regular find-file
(require 'ido)

;; suppress warnings
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)

(defun tak/ido-setup ()
                                        ; common keys
  (define-key ido-completion-map (kbd "<C-return>") 'ido-select-text)
  (define-key ido-file-dir-completion-map (kbd "C-l") nil)
  (define-key ido-common-completion-map (kbd "C-l") 'ido-toggle-literal)
  )
;;(add-hook 'ido-setup-hook 'tak/ido-setup)

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length 0)
(setq ido-use-virtual-buffers t)

(when (maybe-require-package 'ido-ubiquitous)
  (defvar ido-context-switch-command nil)
                                        ; suppress warning
  (ido-ubiquitous-mode t))

;; Use smex to handle M-x
(when (maybe-require-package 'smex)
  ;; Change path for ~/.smex-items
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key [remap execute-extended-command] 'smex))

;; (require-package 'idomenu)
;; (autoload 'idomenu "idomenu" nil t)

;; Allow the same buffer/file to be open in different frames
(setq ido-default-buffer-method 'selected-window)
(setq ido-default-file-method 'selected-window)

;; http://www.reddit.com/r/emacs/comments/21a4p9/use_recentf_and_ido_together/cgbprem
(add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up] 'previous-history-element)))

;; display ido lists vertically
(require-package 'ido-vertical-mode)
(require 'ido-vertical-mode)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
(ido-vertical-mode t)

(require-package 'ido-describe-bindings)

(provide 'init-ido)
