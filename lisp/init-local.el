(setq-default
 auto-revert-verbose t
 revert-without-query (quote (".*"))
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.d/saves"))
 make-backup-files t
 vc-make-backup-files nil
 delete-old-versions t
 fill-column 79
 guide-key/idle-delay 2.0
                                        ; scale text down
 guide-key/text-scale-amount -1.15
 kept-new-versions 6
 kept-old-versions 2

 ;; magit
 git-gutter-fr+-side 'right-fringe
 git-gutter-fr:side 'right-fringe
 
 magit-diff-refine-hunk t
 magit-diff-highlight-trailing nil
 magit-repository-directories (quote ("~/.emacs.d"
                                      "~/org"
                                      "~/code/scripts"
                                      "~/dotfiles"
                                      ))
 message-log-max 10000

 guide-key/recursive-key-sequence-flag t

 scroll-restore-handle-cursor nil

 ;; shell-mode
 shell-completion-execonly nil

 version-control t
 visible-bell t

 ;; flycheck-mode
 flycheck-check-syntax-automatically '(idle-change mode-enabled)
 flycheck-idle-change-delay 3
 flycheck-perlcritic-severity 4
 
 ;; org-mode
 org-replace-disputed-keys t
 org-agenda-files "~/org/agenda/agenda.org"
 
 ;; open new remote tabs in chrome using chrome-open-url script
 ;; not yet working
 ;;browse-url-browser-function 'browse-url-generic
 ;;browse-url-generic-program "chrome-open-url"

 ;; disable display reordering by default for performance reasons
 ;;bidi-display-reordering nil

 ;; use this instead for now, is recommended way of doing this -- test and see
 ;; if it helps perf in large files with long lines
 bidi-paragraph-direction 'left-to-right

 ;; csv-mode
 csv-separators '(",")

 initial-scratch-message ";; *scratch*\n\n"

 ;; auto-completion
 ac-use-fuzzy t
 )


;;(dolist (path '("/home/jguenther/.emacs-lisp"
;;                "/usr/local/share/emacs/site-lisp"
;;                "/home/jguenther/share/emacs/site-lisp"
;;                "/usr/share/emacs/site-lisp"))
;;  (add-to-list 'load-path path))

(mouse-wheel-mode 1)

;;scroll-restore seems to cause more problems than it's worth
;; (require-package 'scroll-restore)
;; (require 'scroll-restore)
;; (scroll-restore-mode 1)
;;                                         ; make cursor invisible when offscreen
;; (dolist (cmd '(scroll-left scroll-right))
;;   (add-to-list 'scroll-restore-commands cmd))

;; (require-package 'bookmark+)
;; (after-load 'bookmark
;;   (require 'bookmark+))

;; (require-package 'doremi)
;; (require 'doremi)
;; (require-package 'doremi-frm)
;; (require 'doremi-frm)
;; (require-package 'doremi-cmd)
;; (require 'doremi-cmd)
;; (autoload 'define-doremi "doremi-mac" "Define a Do Re Mi command." nil 'macro)

(require-package 'help+)
(require-package 'help-fns+)
(require-package 'help-mode+)

(after-load 'help
  (require 'help+)
  (require 'help-fns+)
  (require 'help-mode+)
  )

;;(require-package 'thumb-frm)
;;(require 'thumb-frm)

;;(require-package 'menu-bar+)
;; (after-load 'menu-bar
;;   (require 'menu-bar+))

(defun try-to-add-imenu ()
  (condition-case nil (imenu-add-to-menubar "imenu") (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)

(defun tak/auto-revert-mode-off ()
  (interactive)
  (auto-revert-mode -1))

(when *is-a-mac*
  ;; rebind home/end for macos
  (global-set-key (kbd "<home>") 'move-beginning-of-line)
  (global-set-key (kbd "<end>") 'move-end-of-line)
  
  ;; unbind CMD-w kill-frame
  (global-unset-key (kbd "s-w"))

  ;; paste utf-8
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

  ;; disable global-auto-revert-mode due to corruption bug
  (add-hook 'find-file-hook 'tak/auto-revert-mode-off)
  )

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
Moves point to the end of the inserted text. Does not change mark."
  (interactive) (insert (x-get-selection 'SECONDARY)))

;;(require-package 'cursor-chg)
;;(require 'cursor-chg)
;;(toggle-cursor-type-when-idle 1) ; Turn on cursor change when Emacs is idle
;;(change-cursor-mode 1)           ; Turn on change for overwrite, read-only, and
                                        ; input mode

(require-package 'cperl-mode)
(defalias 'perl-mode 'cperl-mode)

(add-to-list 'ac-modes 'cperl-mode)

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
              ;;(make-variable-buffer-local 'ac-sources)
              ;; (setq ac-sources
              ;;       '(ac-source-perl-completion))
              )))

;; (after-load 'cperl-mode
;;   (require 'init-perlysense))

;; arma script syntax is very similar to c
(add-to-list 'auto-mode-alist '("\\.sq[fm]$" . c-mode))

(set-scroll-bar-mode t)



;; magit setup

(require-package 'git-timemachine)
(require-package 'git-wip-timemachine)

;;(require-package 'git-gutter-fringe)
;;(require-package 'git-gutter)

(after-load 'magit
  ;; (require 'git-gutter)
  ;; (require 'git-gutter-fringe)
  
  ;; (global-git-gutter-mode t)
  ;; (diminish 'git-gutter-mode)

  (magit-wip-after-save-mode 1)
  (diminish 'magit-wip-after-save-local-mode)

  (global-magit-file-buffer-mode)
  )

;; (global-set-key (kbd "C-c u t") 'git-gutter:toggle)

;; (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
;; (global-set-key (kbd "C-c u =") 'git-gutter:popup-hunk)

;; ;; Jump to next/previous hunk
;; (global-set-key (kbd "C-c u p") 'git-gutter:previous-hunk)
;; (global-set-key (kbd "C-c u n") 'git-gutter:next-hunk)

;; ;; Stage current hunk
;; (global-set-key (kbd "C-c u s") 'git-gutter:stage-hunk)

;; ;; Revert current hunk
;; (global-set-key (kbd "C-c u r") 'git-gutter:revert-hunk)



(require-package 'win-switch)
(require 'win-switch)
(setq-default win-switch-other-window-first t
              win-switch-idle-time 1.5
              win-switch-window-threshold 0
              win-switch-set-wrap-around 1)

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



;;; launcher keymap
(define-key launcher-map "c" #'calc)
(define-key launcher-map "d" #'ediff-buffers)
(define-key launcher-map "f" #'find-dired)
(define-key launcher-map "g" #'lgrep)
(define-key launcher-map "G" #'rgrep)
(define-key launcher-map "F" #'grep-find)
(define-key launcher-map "h" #'man) ; Help
(define-key launcher-map "i" #'package-install-from-buffer)
(define-key launcher-map "n" #'nethack)
(define-key launcher-map "p" #'paradox-list-packages)
(define-key launcher-map "s" #'shell)
(define-key launcher-map "t" #'proced) ; top
(define-key launcher-map "a" #'ansi-term)

;; toggle keymap
(define-key endless/toggle-map "c" #'column-number-mode)
(define-key endless/toggle-map "d" #'toggle-debug-on-error)
(define-key endless/toggle-map "e" #'toggle-debug-on-error)
(define-key endless/toggle-map "f" #'auto-fill-mode)
(define-key endless/toggle-map "l" #'toggle-truncate-lines)
(define-key endless/toggle-map "t" #'toggle-truncate-lines)
(define-key endless/toggle-map "q" #'toggle-debug-on-quit)
(define-key endless/toggle-map "g" #'toggle-debug-on-quit)
(define-key endless/toggle-map "S" #'dired-toggle-sudo)

(define-key endless/toggle-map "C" #'comment-or-uncomment-region)
(define-key endless/toggle-map "#" #'comment-or-uncomment-region)



;;doesn't work well with multimonitor setup -- doesn't maximize window, instead
;;it resizes it offscreen
;;
;;(require 'tabula-rasa)
;;(define-key endless/toggle-map "D" #'tabula-rasa-mode)

;; darkroom seems to be causing problems
;;(require-package 'darkroom)
;;(autoload 'darkroom-tentative-mode "darkroom" nil t)
;;(define-key endless/toggle-map "D" #'darkroom-tentative-mode)

;;(require-package 'minimap)
;;(autoload 'minimap-toggle "minimap" nil t)
;; (after-load 'minimap
;;   (setq-default minimap-resizes-buffer t)
;;   (setq-default minimap-width-fraction 0.17))
;; (define-key endless/toggle-map "m" #'minimap-toggle)

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
(define-key tak/vc-toggle-map "h" #'magit-diff-toggle-refine-hunk)
(define-key tak/vc-toggle-map "w" #'tak/toggle-magit-highlight-whitespace)

(after-load 'aggressive-indent
  (add-to-list 'aggressive-indent-excluded-modes 'cperl-mode t))

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

(global-set-key (kbd "C-x C-S-R") 'revert-buffer)

;;(require-package 'workgroups2)
;;(require 'workgroups2)
;;(setq-default wg-prefix-key (kbd "C-x w"))
;;(setq-default wg-session-file (concat user-emacs-directory ".workgroups"))

;; don't use this in all buffers -- workgroups-mode rebinds `winner-undo' and
;; `winner-redo', but wg undo command doesn't work unless there's an active
;; workgroup
;;(workgroups-mode 1)

(dolist (regex (list "/\\.git/" "/sudo:" (concat user-emacs-directory "elpa") "\\.asv\\'"))
  (add-to-list 'recentf-exclude regex))



;; set initial and default frame parameters
(dolist (parameter '((width . 90)
                     (height . 55)))
  (add-to-list 'default-frame-alist parameter)
  (add-to-list 'initial-frame-alist parameter)
  )

(dolist (parameter '((left . -10)
                     (top . 23)
                     (user-position . t)))
  (add-to-list 'initial-frame-alist parameter))

(defun tak/set-default-frame-parameters ()
  (interactive)
  (let* ((params-mac     '((width . 100)
                           (height . 55)
                           (left . -1)
                           (top . 23)))
         
         (params-default '((width . 90)
                           (height . 50)
                           (left . -10)
                           (top . 35)))
         (params (if *is-a-mac*
                     params-mac
                   params-default)))
    (dolist (parameter params)
      (set-frame-parameter nil (car parameter) (cdr parameter))
      )))

(global-set-key (kbd "C-x C-9") 'tak/set-default-frame-parameters)



;;; guide-key setup

(after-load 'guide-key
  (dolist (key '(
                 (cperl-mode "C-o")
                 "M-g"
                 "ESC"
                 "C-h"
                 ))
    (add-to-list 'guide-key/guide-key-sequence key t)))


;;; Scrolling

;; swap scroll-left and scroll-right default binds
(global-set-key (kbd "C-x >") 'scroll-left)
(global-set-key (kbd "C-x <") 'scroll-right)

; mouse buttons are different on osx
(when *is-a-mac*
  (global-set-key (kbd "<wheel-right>") 'scroll-left)
  (global-set-key (kbd "<wheel-left>") 'scroll-right))



(require-package 'goto-last-change)
(global-set-key (kbd "C-x C-\\") 'goto-last-change)



;; http://oremacs.com/2015/01/20/introducing-hydra/
;;(require-package 'hydra)



;; org-mode setup

(require-package 'org-plus-contrib)

(defun tak/org-mode-setup ()
  (guide-key/add-local-highlight-command-regexp "org-")
  (define-key org-mode-map (kbd "C-M-<return>") 'org-insert-todo-heading)
  )

(add-hook 'org-mode-hook 'tak/org-mode-setup)

(after-load 'org
  (dolist (element '(
                     ;;org-bullets
                     ;;org-cliplink
                     ;;org-elisp-help
                     org-fstree
                     
                     ;;org-gcal
                     
                     ;;orgit
                     orglink
                     
                     ;; disabled -- don't use anything.el/helm.el yet
                     ;;org-linkany
                     
                     org-repo-todo
                                        ;org-trello
                     ))
    (if (maybe-require-package element)
        (add-to-list 'org-modules element t)))
  
  (org-load-modules-maybe t))



;; so git-wip-mode doesn't depend on running magit-status first
(after-load 'init-git
  (require 'magit))


;; shell-mode init

;; use color term for shell
(defadvice tak/shell-setup-environment (around shell activate)
  "Setup environment for shell.

Sets TERM=xterm-256color"
  (let ((term (getenv "TERM")))
    (setenv "TERM" "xterm-256color")
    ad-do-it)
  (setenv "TERM" term))

;; bash-completion for shell-mode
;; from https://github.com/szermatt/emacs-bash-completion
(require-package 'bash-completion)

(autoload 'bash-completion-dynamic-complete 
  "bash-completion"
  "BASH completion hook")

(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)

(add-hook 'shell-command-complete-functions
          'bash-completion-dynamic-complete)

;; translate ANSI color control sequences into text properties
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)



(global-set-key (kbd "C-S-o") 'sanityinc/open-line-with-reindent)



;; discovery modes

(require-package 'discover)
(require-package 'discover-my-major)

(global-set-key (kbd "C-h M-m") 'discover-my-major)

(require 'discover)
(global-discover-mode 1)



;; exclude dirs from auto-revert-notify that cause slowdowns
(setq-default auto-revert-notify-exclude-dir-regexp
              (concat auto-revert-notify-exclude-dir-regexp "\\|/blib/"))



;; smart-mode-line

;; disable for now -- minor modes appear way off to the right and get cut off
;; for some reason
;; (require 'init-smart-mode-line)



;; turn on save place so that when opening a file, the cursor will be at the
;; last position.
(setq-default save-place t)
(setq save-place-file
      (concat user-emacs-directory ".saved-places"))
(require 'saveplace)



(require-package 'projectile)
(require-package 'org-projectile)



(global-set-key (kbd "M-g o") 'occur)
(global-set-key (kbd "M-g C-o") 'occur)



(require-package 'free-keys)
(require 'free-keys)

(global-set-key (kbd "C-h C-k") 'free-keys)

;; add super if on mac
(when *is-a-mac*
  (add-to-list 'free-keys-modifiers "s" t))


(global-set-key (kbd "C-h M-c") nil)
(global-set-key (kbd "C-h M-c g") 'customize-group)
(global-set-key (kbd "C-h M-c c") 'customize-variable)
(global-set-key (kbd "C-h M-c v") 'customize-variable)



(require-package 'ace-jump-mode)
(global-set-key (kbd "C-;") 'ace-jump-mode)
(global-set-key (kbd "C-:") 'ace-jump-word-mode)


;; from http://stackoverflow.com/questions/3393834/how-to-move-forward-and-backward-in-emacs-mark-ring/3399064#3399064
(defun unpop-to-mark-command ()
  "Unpop off mark ring into the buffer's actual mark.
Does not set point.  Does nothing if mark ring is empty."
  (interactive)
  (let ((num-times (if (equal last-command 'pop-to-mark-command) 2
                     (if (equal last-command 'unpop-to-mark-command) 1
                       (error "Previous command was not a (un)pop-to-mark-command")))))
    (dotimes (x num-times)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (+ 0 (car (last mark-ring))) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (mark t)))
      (deactivate-mark))))

;; from http://stackoverflow.com/questions/3393834/how-to-move-forward-and-backward-in-emacs-mark-ring/5117076#5117076
(defmacro my-unpop-to-mark-advice ()
  "Enable reversing direction with un/pop-to-mark.

To unpop, simply supply a negative prefix argument. e.g. `C-- C-SPC'.

After an initial `C-- C-SPC' you can continue un-popping with just
`C-SPC'. To reverse the direction again and call pop-to-mark, simply
supply a positive argument once more with C-u C-SPC."
  `(defadvice ,(key-binding (kbd "C-SPC")) (around my-unpop-to-mark activate)
     "Unpop-to-mark with negative arg"
     (let* ((arg (ad-get-arg 0))
            (num (prefix-numeric-value arg)))
       (cond
        ;; Enabled repeated un-pops with C-SPC
        ((eq last-command 'unpop-to-mark-command)
         (if (and arg (> num 0) (<= num 4))
             ad-do-it ;; C-u C-SPC reverses back to normal direction
           ;; Otherwise continue to un-pop
           (setq this-command 'unpop-to-mark-command)
           (unpop-to-mark-command)))
        ;; Negative argument un-pops: C-- C-SPC
        ((< num 0)
         (setq this-command 'unpop-to-mark-command)
         (unpop-to-mark-command))
        (t
         ad-do-it)))))
(my-unpop-to-mark-advice)


(provide 'init-local)
