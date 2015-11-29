;; Use C-f during file selection to switch to regular find-file

(defun tak/ido-setup ()
                                        ; common keys
  (define-key ido-completion-map (kbd "<C-return>") 'ido-select-text)
  (define-key ido-file-dir-completion-map (kbd "C-l") nil)
  (define-key ido-common-completion-map (kbd "C-l") 'ido-toggle-literal)
  )
(add-hook 'ido-setup-hook 'tak/ido-setup)

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

;; Allow the same buffer/file to be open in different frames
(setq ido-default-buffer-method 'selected-window)
(setq ido-default-file-method 'selected-window)

;; http://www.reddit.com/r/emacs/comments/21a4p9/use_recentf_and_ido_together/cgbprem
(add-hook 'ido-setup-hook
          (defun tak/setup-ido ()
            (define-key ido-completion-map [up] #'previous-history-element)
            (define-key ido-completion-map [down] #'next-history-element)
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
            (define-key ido-completion-map (kbd "C-c C-t") 'ido-toggle-prefix)
            (define-key ido-completion-map [left] 'ido-prev-match)
            (define-key ido-completion-map [right] 'ido-next-match)))

;; display ido lists vertically
(require-package 'ido-vertical-mode)
(require 'ido-vertical-mode)

(ido-vertical-mode t)

(require-package 'ido-describe-bindings)



(require-package 'flx)
(require-package 'flx-ido)
(require-package 'flx-isearch)

(after-load 'ido
  (require 'flx-ido)
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  ;; disable ido faces to see flx highlights.
  ;; (setq ido-use-faces nil)
  ;; disable ido-flx highlights
  ;; (setq flx-ido-use-faces nil)
  )



;; ido-other-window
(quelpa '(ido-other-window :fetcher github :repo "gvalkov/ido-other-window"))
(require 'ido-other-window)


(provide 'init-ido)
