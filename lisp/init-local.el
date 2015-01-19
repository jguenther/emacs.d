(setq
 auto-revert-verbose t
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.d/saves"))
 delete-old-versions t
 fill-column 79
 guide-key/idle-delay 2.0
                                        ; scale text down
 guide-key/text-scale-amount -1.5
 kept-new-versions 6
 kept-old-versions 2

 ;; magit
 git-gutter-fr+-side 'right-fringe
 magit-revert-backup t
 global-magit-wip-save-mode t
 magit-diff-refine-hunk t
 magit-highlight-whitespace nil
 magit-repo-dirs (quote ("~/.emacs.d" "~/code/PRIPchip"))
 
 message-log-max 10000
 version-control t
 visible-bell t

 ;; org-mode
 org-replace-disputed-keys t
 org-agenda-files (list "~/org/agenda/")
 org-modules '(org-bbdb org-bibtex org-ctags org-docview org-gnus org-habit org-id org-info org-inlinetask org-irc org-mhe org-mouse org-protocol org-rmail org-w3m org-annotate-file org-bookmark org-bullets org-checklist org-choose org-collector org-drill org-elisp-symbol org-eshell org-eval-light org-eval org-expiry org-favtable org-git-link org-panel org-registry org-secretary org-toc org-track)
 
 ;; open new remote tabs in chrome using chrome-open-url script
 ;; not yet working
 ;;browse-url-browser-function 'browse-url-generic
 ;;browse-url-generic-program "chrome-open-url"
 )

(dolist (path '("/home/jguenther/.emacs-lisp"
                "/usr/local/share/emacs/site-lisp"
                "/home/jguenther/share/emacs/site-lisp"
                "/usr/share/emacs/site-lisp"))
  (add-to-list 'load-path path))

(global-set-key "\M-g" 'goto-line)
(mouse-wheel-mode 1)

(require-package 'scroll-restore)
(require 'scroll-restore)
(scroll-restore-mode 1)
                                        ; make cursor invisible when offscreen
(setq scroll-restore-handle-cursor nil)
(dolist (cmd '(scroll-left scroll-right))
  (add-to-list 'scroll-restore-commands cmd))

(require-package 'bookmark+)
(after-load 'bookmark
  (require 'bookmark+))

(require-package 'doremi)
(require 'doremi)
(require-package 'doremi-frm)
(require 'doremi-frm)
(require-package 'doremi-cmd)
(require 'doremi-cmd)
(autoload 'define-doremi "doremi-mac"
  "Define a Do Re Mi command." nil 'macro)

(require-package 'help+)
(require-package 'help-fns+)

(after-load 'help
  (require 'help+)
  (require 'help-fns+))

(require-package 'thumb-frm)
(require 'thumb-frm)

(require-package 'menu-bar+)
(after-load 'menu-bar
  (require 'menu-bar+))

(defun try-to-add-imenu ()
  (condition-case nil (imenu-add-to-menubar "imenu") (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)

;; imenu bindings
(global-set-key [S-mouse-3] 'imenu)
(global-set-key (kbd "C-'") 'imenu-anywhere)
(global-set-key (kbd "C-S-i") 'imenu)

(global-font-lock-mode 1)

(show-paren-mode 1)

(column-number-mode 1)
(line-number-mode 1)

(defsubst yank-secondary ()
  "Insert the secondary selection at point.
  Moves point to the end of the inserted text.  Does not change mark."
  (interactive) (insert (x-get-selection 'SECONDARY)))

(require-package 'cursor-chg)
(require 'cursor-chg)
(toggle-cursor-type-when-idle 1) ; Turn on cursor change when Emacs is idle
(change-cursor-mode 1)           ; Turn on change for overwrite, read-only, and
                                        ; input mode

(require-package 'cperl-mode)
(defalias 'perl-mode 'cperl-mode)

(add-to-list 'auto-mode-alist
             '("\\.\\([pP][Llm]\\|al\\|t\\|xs\\|tl\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(add-hook 'cperl-mode-hook 'imenu-add-menubar-index)

(add-hook 'cperl-mode-hook
          (lambda ()
            (when (require 'auto-complete nil t)
              (auto-complete-mode t)
              (make-variable-buffer-local 'ac-sources)
              ;; (setq ac-sources
              ;;       '(ac-source-perl-completion))
              )))

(add-hook 'cperl-mode-hook
          (lambda ()
            (require 'init-perlysense)))

;; arma script syntax is very similar to c
(add-to-list 'auto-mode-alist '("\\.sq[fm]$" . c-mode))

(set-scroll-bar-mode t)

(require-package 'git-gutter-fringe+)
(after-load 'magit
  (require 'git-gutter-fringe+)
  (global-git-gutter+-mode t)
  (git-gutter+-enable-fringe-display-mode)
  (diminish 'git-gutter+-mode))

(require-package 'win-switch)
(require 'win-switch)
(setq win-switch-other-window-first t
      win-switch-idle-time 1.5
      win-switch-window-threshold 0
      win-switch-set-wrap-around 1)

;; workaround for https://github.com/genovese/win-switch/issues/4
(define-prefix-command 'win-switch-dispatch-once)
(map-keymap (lambda (event binding)
              (define-key win-switch-dispatch-once (vector event) binding))
            win-switch-once-map)

(define-key ctl-x-map "\C-o" 'win-switch-dispatch-once)

(win-switch-setup-keys-ijkl "\C-xO")

(require-package 'info+)
                                        ; Load `info+' and removes its
                                        ; mousewheel bindings.
(after-load 'info
  (require 'info+)
  (define-key Info-mode-map (kbd "<mouse-4>") nil)
  (define-key Info-mode-map (kbd "<mouse-5>") nil))

(global-set-key "\C-\M-_" 'undo-tree-redo)

(add-to-list 'auto-mode-alist
             '("\\(inputrc\\|bashrc\\)\\'" . sh-mode))

(global-set-key (kbd "C-S-k") 'kill-whole-line)

;;; Custom keymaps

;;
;; http://endlessparentheses.com/launcher-keymap-for-standalone-features.html
;;
(define-prefix-command 'launcher-map)
;; C-x l is `count-lines-page' by default. If you
;; use that, you can try s-l or <C-return>.
(define-key ctl-x-map "l" 'launcher-map)
;;(global-set-key (kbd "s-l") 'launcher-map)
(define-key launcher-map "c" #'calc)
(define-key launcher-map "d" #'ediff-buffers)
(define-key launcher-map "f" #'find-dired)
(define-key launcher-map "g" #'lgrep)
(define-key launcher-map "G" #'rgrep)
(define-key launcher-map "h" #'man) ; Help
(define-key launcher-map "i" #'package-install-from-buffer)
(define-key launcher-map "n" #'nethack)
(define-key launcher-map "p" '(lambda () (interactive)
                                        ; don't autorefresh package list
                                (paradox-list-packages t)))
(define-key launcher-map "s" #'shell)
(define-key launcher-map "t" #'proced) ; top
(define-key launcher-map "a" #'ansi-term)

;;
;; http://endlessparentheses.com/the-toggle-map-and-wizardry.html
;;
(define-prefix-command 'endless/toggle-map)
;; The manual recommends C-c for user keys, but C-x t is
;; always free, whereas C-c t is used by some modes.
(define-key ctl-x-map "t" 'endless/toggle-map)
(define-key endless/toggle-map "c" #'column-number-mode)
(define-key endless/toggle-map "d" #'toggle-debug-on-error)
(define-key endless/toggle-map "e" #'toggle-debug-on-error)
(define-key endless/toggle-map "f" #'auto-fill-mode)
(define-key endless/toggle-map "l" #'toggle-truncate-lines)
(define-key endless/toggle-map "t" #'toggle-truncate-lines)
(define-key endless/toggle-map "q" #'toggle-debug-on-quit)
(define-key endless/toggle-map "S" #'dired-toggle-sudo)

;;doesn't work well with multimonitor setup -- doesn't maximize window, instead
;;it resizes it offscreen
;;
;;(require 'tabula-rasa)
;;(define-key endless/toggle-map "D" #'tabula-rasa-mode)

;; darkroom seems to be causing problems
;;(require-package 'darkroom)
;;(autoload 'darkroom-tentative-mode "darkroom" nil t)
;;(define-key endless/toggle-map "D" #'darkroom-tentative-mode)

(require-package 'minimap)
(autoload 'minimap-toggle "minimap" nil t)
(after-load 'minimap
  (setq minimap-resizes-buffer t)
  (setq minimap-width-fraction 0.17))
(define-key endless/toggle-map "m" #'minimap-toggle)

;; can't find defun for endless/toggle-theme
;;(define-key endless/toggle-map "t" #'endless/toggle-theme)

;;; Generalized version of `read-only-mode'.
(define-key endless/toggle-map "r" #'dired-toggle-read-only)
(autoload 'dired-toggle-read-only "dired" nil t)
(define-key endless/toggle-map "w" #'whitespace-mode)

(defvar tak/magit-highlight-whitespace magit-highlight-whitespace
  "Enable or disable whitespace highlighting in `magit-mode`.

Used by `tak/toggle-magit-highlight-whitespace`.")

(defun tak/toggle-magit-highlight-whitespace ()
  "Toggles highlighting of whitespace in `magit-mode` buffers.

Toggles the value of `tak/magit-highlight-whitespace`, assigning its
new value to `magit-highlight-whitespace`. Does not currently
differentiate `t` from `status` in this variable, and will toggle
between `nil` and `t` no matter the original value of
`magit-highlight-whitespace`."
  (interactive)   
  (let ((highlight (setq tak/magit-highlight-whitespace
                         (not tak/magit-highlight-whitespace))))
    (setq magit-highlight-whitespace highlight)
    (message "Whitespace highlighting %s"
             (if magit-highlight-whitespace
                 '"enabled"
               '"disabled"))))

;; vc/magit-mode bindings
(define-prefix-command 'tak/vc-toggle-map)
(define-key endless/toggle-map "v" #'tak/vc-toggle-map)
(define-key tak/vc-toggle-map "h" #'magit-diff-toggle-refine-hunk)
(define-key tak/vc-toggle-map "w" #'tak/toggle-magit-highlight-whitespace)

(add-to-list 'aggressive-indent-excluded-modes 'cperl-mode t)
(global-aggressive-indent-mode t)

(require 'init-private nil t)

;; enable disabled commands in custom.el instead of init.el
(defadvice en/disable-command (around put-in-custom-file activate)
  "Put declarations in `custom-file'."
  (let ((user-init-file custom-file))
    ad-do-it))

;; adapted from http://www.emacswiki.org/emacs/DisabledCommands
(defun enable-all-disabled-commands (&optional just-list-them)
  "Enable all commands, reporting on which were disabled.

If `just-list-them' is non-nil, prints disabled commands but doesn't
enable them."
  (interactive)
  (with-output-to-temp-buffer "*Disabled commands*"
    (mapatoms
     (function
      (lambda (symbol)
        (when (get symbol 'disabled)
          (unless just-list-them (put symbol 'disabled nil))
          (prin1 symbol)
          (princ "\n")))))))

(defun list-disabled-commands ()
  "Prints a list of disabled commands in a new temporary buffer.

See also: `enable-all-disabled-commands'."
  (interactive)
  (enable-all-disabled-commands t))

(defun tak/maybe-revert-buffer ()
  "Reverts the current buffer without confirmation if it is unmodified.
If buffer has been modified since it was last read from disk or saved,
the user will be asked for confirmation before the buffer is reverted."
  (interactive)
  (save-excursion
    (revert-buffer t (not (buffer-modified-p)))))

(define-key ctl-x-map "R" 'tak/maybe-revert-buffer)

(require-package 'workgroups2)
(require 'workgroups2)
(setq wg-prefix-key (kbd "C-x w"))
(setq wg-session-file (concat user-emacs-directory ".workgroups"))

;; don't use this in all buffers -- workgroups-mode rebinds `winner-undo' and
;; `winner-redo', but wg undo command doesn't work unless there's an active
;; workgroup
;;(workgroups-mode 1)

(dolist (regex '("/.git/" "/sudo:"))
  (add-to-list 'recentf-exclude regex))



;; set initial and default frame parameters
(dolist (parameter '((width . 85)
                     (height . 50)))
  (add-to-list 'default-frame-alist parameter)
  (add-to-list 'initial-frame-alist parameter))

(dolist (parameter '((left . -10)
                     (top . 35)
                     (user-position . t)))
  (add-to-list 'initial-frame-alist parameter))

(defun tak/set-default-frame-parameters ()
  (interactive)
  (dolist (parameter '((width . 90)
                       (height . 50)
                       (left . -10)
                       (top . 35)))
    (set-frame-parameter nil (car parameter) (cdr parameter))))

(global-set-key "\C-c0" 'tak/set-default-frame-parameters)



;;; guide-key setup
(add-hook 'org-mode-hook
          (lambda ()
            (guide-key/add-local-guide-key-sequence "C-c")
            (guide-key/add-local-guide-key-sequence "C-c C-x")
            (guide-key/add-local-highlight-command-regexp "org-")))

(add-to-list 'guide-key/guide-key-sequence "C-x r" t)


;;; Scrolling

;; swap scroll-left and scroll-right default binds
(global-set-key (kbd "C-x >") 'scroll-left)
(global-set-key (kbd "C-x <") 'scroll-right)

(global-set-key (kbd "<mouse-7>") 'scroll-left)
(global-set-key (kbd "<mouse-6>") 'scroll-right)



(require-package 'goto-last-change)
(global-set-key (kbd "C-x C-\\") 'goto-last-change)

(provide 'init-local)
