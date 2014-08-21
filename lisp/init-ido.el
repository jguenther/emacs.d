;; Use C-f during file selection to switch to regular find-file
(require 'ido)

; avoid warnings
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)

(add-hook 'ido-setup-hook 'ido-my-keys)

(defun ido-my-keys ()
  "Set up the keymap for `ido'."

  ;; common keys
;  (define-key ido-completion-map "\C-e" 'ido-edit-input)
;  (define-key ido-completion-map "\t" 'ido-complete) ;; complete partial
;  (define-key ido-completion-map "\C-j" 'ido-select-text)
;  (define-key ido-completion-map "\C-m" 'ido-exit-minibuffer)
  (define-key ido-completion-map "?" 'ido-completion-help) ;; list completions
  (define-key ido-completion-map [(control ? )] 'ido-restrict-to-matches)
  (define-key ido-completion-map [(control ?@)] 'ido-restrict-to-matches)

  ;; cycle through matches
  (define-key ido-completion-map "\C-r" 'ido-prev-match)
  (define-key ido-completion-map "\C-s" 'ido-next-match)
  (define-key ido-completion-map [right] 'ido-next-match)
  (define-key ido-completion-map [left] 'ido-prev-match)

  ;; toggles
  (define-key ido-completion-map "\C-t" 'ido-toggle-regexp) ;; same as in isearch
  (define-key ido-completion-map "\C-p" 'ido-toggle-prefix)
  (define-key ido-completion-map "\C-c" 'ido-toggle-case)
  (define-key ido-completion-map "\C-a" 'ido-toggle-ignore)

  ;; keys used in file and dir environment
  (when (memq ido-cur-item '(file dir))
    (define-key ido-completion-map "\C-b" 'ido-enter-switch-buffer)
    (define-key ido-completion-map "\C-d" 'ido-enter-dired)
    (define-key ido-completion-map "\C-f" 'ido-fallback-command)

    ;; cycle among directories
    ;; use [left] and [right] for matching files
    (define-key ido-completion-map [down] 'ido-next-match-dir)
    (define-key ido-completion-map [up]   'ido-prev-match-dir)

    ;; backspace functions
    (define-key ido-completion-map [backspace] 'ido-delete-backward-updir)
    ;(define-key ido-completion-map "\d"        'ido-delete-backward-updir)
    (define-key ido-completion-map [(meta backspace)] 'ido-delete-backward-word-updir)
    (define-key ido-completion-map [(control backspace)] 'ido-up-directory)

    ;; I can't understand this
    ;(define-key ido-completion-map [(meta ?d)] 'ido-wide-find-dir)
    ;(define-key ido-completion-map [(meta ?f)] 'ido-wide-find-file)
    ;(define-key ido-completion-map [(meta ?k)] 'ido-forget-work-directory)
    ;(define-key ido-completion-map [(meta ?m)] 'ido-make-directory)

    (define-key ido-completion-map [(meta down)] 'ido-next-work-directory)
    (define-key ido-completion-map [(meta up)] 'ido-prev-work-directory)
    (define-key ido-completion-map [(meta left)] 'ido-prev-work-file)
    (define-key ido-completion-map [(meta right)] 'ido-next-work-file)

    ;; search in the directories
    ;; use C-_ to undo this
    (define-key ido-completion-map [(meta ?s)] 'ido-merge-work-directories)
    (define-key ido-completion-map [(control ?\_)] 'ido-undo-merge-work-directory)
    )

  (when (eq ido-cur-item 'file)
    ;(define-key ido-completion-map "\C-k" 'ido-delete-file-at-head)
    ;(define-key ido-completion-map "\C-l" 'ido-toggle-literal)
    ;(define-key ido-completion-map "\C-o" 'ido-copy-current-word)
    ;(define-key ido-completion-map "\C-v" 'ido-toggle-vc)
    ;(define-key ido-completion-map "\C-w" 'ido-copy-current-file-name)
    )

  (when (eq ido-cur-item 'buffer)
    (define-key ido-completion-map "\C-b" 'ido-fallback-command)
    (define-key ido-completion-map "\C-f" 'ido-enter-find-file)
    (define-key ido-completion-map "\C-k" 'ido-kill-buffer-at-head)
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
