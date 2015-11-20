(setq-default
 auto-revert-verbose t
 revert-without-query (quote (".*"))
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.d/saves"))
 make-backup-files t
 vc-make-backup-files nil
 delete-old-versions t
 fill-column 79

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

 scroll-restore-handle-cursor nil

 redisplay-dont-pause t
 scroll-margin 1
 scroll-step 5
 scroll-conservatively 10000
 scroll-preserve-screen-position 1
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01

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
 org-agenda-files '("~/org")

 ;; disable display reordering by default for performance reasons
 bidi-display-reordering nil

 ;; use this instead for now, is recommended way of doing this -- test and see
 ;; if it helps perf in large files with long lines
 bidi-paragraph-direction 'left-to-right

 ;; csv-mode
 csv-separators '(",")

 initial-scratch-message ";; *scratch*\n\n"

 ;; auto-completion
 ac-use-fuzzy t

 ;; blink-cursor mode
 blink-cursor-blinks 5
 blink-cursor-delay 1
 blink-cursor-interval 0.8

 debug-on-event 'sigusr1
 delete-by-moving-to-trash t
 frame-resize-pixelwise t
 nav-flash-use-pulse (quote gui-only)

 visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
 )

;; set default frame font globally
(add-to-list 'default-frame-alist '(font . "Office Code Pro-12"))
(set-face-attribute 'default t :font "Office Code Pro-12")

(defalias 'basename 'file-name-directory)

(if *is-a-mac*
    ;; visual bug on el capitan
    (setq visible-bell nil))

(require 's)

(blink-cursor-mode t)

(defvar user-home-directory (expand-file-name "~")
  "The absolute path to the user's home directory `~'.")

(mouse-wheel-mode 1)

(require-package 'hl-line+)
(require 'hl-line+)
(setq-default hl-line-face 'highlight)
(dolist (mode '(custom-mode
                magit-status-mode
                magit-diff-mode
                magit-log-mode))
  (add-to-list 'hl-line-inhibit-highlighting-for-modes mode))
(global-hl-line-mode)

(require-package 'bookmark+)
(setq-default bmkp-last-as-first-bookmark-file
              (expand-file-name ".bookmarks.el" user-emacs-directory))
(after-load 'bookmark
  (require 'bookmark+))

(require-package 'menu-bar+)
(after-load 'menu-bar
  (require 'menu-bar+))

(defun tak/auto-revert-mode-off ()
  (interactive)
  (auto-revert-mode -1))



(require 'mmm-auto)
(mmm-add-classes
 `((shell-script-python
    :submode python-mode
    :front ,(concat "^[  ]*#?[   ]*%{" (mmm-regexp-opt '("begin_python")) "}%
")
    :back ,(concat "^[   ]*#?[   ]*%{" (mmm-regexp-opt '("end_python")) "}%")
    :front-offset 0
    :back-offset 0
    :save-matches 0
    )))

(add-to-list
 'mmm-mode-ext-classes-alist
 '(sh-mode "\\.sh\\'" shell-script-python))

(defun tak/shell-script-mmm-on ()
  (message "shell-script-mmm-on")
  (setq-local mmm-submode-decoration-level 2)
  (mmm-ify-by-class 'shell-script-python))

(add-hook 'shell-script-mode-hook #'tak/shell-script-mmm-on)



;; Default key bindings:

;; C-x C-<SPC>    go back in `global-mark-ring', respects prefix arg
;; C-x C-<left>   go back in `global-mark-ring'
;; C-x C-<right>  go forward in `global-mark-ring'

;; C-x <SPC>      go back in (buffer-local) `mark-ring', respects prefix arg
;; C-x <left>     go back in (buffer-local) `mark-ring'
;; C-x <right>    go forward in (buffer-local) `mark-ring'

(require-package 'back-button)
(require 'back-button)
(back-button-mode 1)
(diminish 'back-button-mode)


;; iedit
(require-package 'iedit)
(define-key mode-specific-map (kbd ";") #'iedit-mode)



(require-package 'imenu)
(require-package 'imenu+)
(require-package 'imenu-anywhere)

(defun try-to-add-imenu ()
  (condition-case nil (imenu-add-to-menubar "imenu") (error nil)))

(add-hook 'font-lock-mode-hook 'try-to-add-imenu)

;;(global-set-key (kbd "C-'") 'imenu-anywhere)



(global-font-lock-mode 1)

(show-paren-mode 1)

(column-number-mode 1)
(line-number-mode 1)

(defsubst yank-secondary ()
  "Insert the secondary selection at point.
Moves point to the end of the inserted text. Does not change mark."
  (interactive) (insert (x-get-selection 'SECONDARY)))

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
              )))



;; arma script syntax is very similar to c
(add-to-list 'auto-mode-alist '("\\.sq[fm]$" . c-mode))

(set-scroll-bar-mode t)



;; magit setup

(require-package 'git-timemachine)
(require-package 'git-wip-timemachine)

(require-package 'git-gutter)
(require-package 'git-gutter+)
(require-package 'git-gutter-fringe)

(after-load 'git-gutter+

  (global-git-gutter+-mode t)
  (diminish 'git-gutter+-mode)

  ;; workaround visual bug with git-gutter and org-indent-mode
  (add-to-list 'git-gutter:disabled-modes 'org-mode)

  (global-set-key (kbd "C-x v =") 'git-gutter+-popup-hunk)
  (define-key git-gutter+-mode-map (kbd "C-c u =") 'git-gutter+-popup-hunk)

  ;; Jump to next/previous hunk
  (define-key git-gutter+-mode-map (kbd "C-c u p") 'git-gutter+-previous-hunk)
  (define-key git-gutter+-mode-map (kbd "C-c u n") 'git-gutter+-next-hunk)

  ;; Stage current hunk
  (define-key git-gutter+-mode-map (kbd "C-c u s") 'git-gutter+-stage-hunks)

  ;; Revert current hunk
  (define-key git-gutter+-mode-map (kbd "C-c u r") 'git-gutter+-revert-hunk)

  )

(after-load 'magit
  (require 'git-gutter)
  (require 'git-gutter+)

  (magit-wip-after-save-mode 1)
  (magit-wip-before-change-mode -1)
  (magit-wip-after-apply-mode -1)

  (diminish 'magit-wip-after-save-local-mode)
  (global-magit-file-buffer-mode)

  (setq-default magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  )



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



(defun tak/paradox-list-packages-new-frame (&optional no-fetch)
  "Makes a new frame, switches to it, and calls
`paradox-list-packages' with optional prefix arg NO-FETCH."
  (interactive "P")
  (let ((new-frame (make-frame)))
    (select-frame-set-input-focus new-frame)
    (paradox-list-packages no-fetch)))

(defun tak/toggle-visual-line-mode ()
  (interactive)
  (visual-line-mode (if (not visual-line-mode)
                        t
                      -1))
  )

(defun tak/toggle-linum-mode ()
  (interactive)
  (linum-mode (if (not linum-mode)
                  t
                -1))
  )

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
(define-key launcher-map "p" #'tak/paradox-list-packages-new-frame)
(define-key launcher-map "s" #'shell)
(define-key launcher-map "S" #'eshell)
(define-key launcher-map "t" #'proced) ; top
(define-key launcher-map "a" #'ansi-term)
(define-key launcher-map "e" #'ielm)   ; elisp REPL

;; toggle keymap
(define-key endless/toggle-map "c" #'column-number-mode)
(define-key endless/toggle-map "d" #'toggle-debug-on-error)
(define-key endless/toggle-map "e" #'toggle-debug-on-error)
(define-key endless/toggle-map "l" #'tak/toggle-linum-mode)
(define-key endless/toggle-map "f" #'auto-fill-mode)
(define-key endless/toggle-map "t" #'tak/toggle-visual-line-mode)
                                        ; instead of truncate-line-mode
(define-key endless/toggle-map "q" #'toggle-debug-on-quit)
(define-key endless/toggle-map "g" #'toggle-debug-on-quit)
(define-key endless/toggle-map "S" #'dired-toggle-sudo)

(define-key endless/toggle-map "C" #'comment-or-uncomment-region)
(define-key endless/toggle-map "#" #'comment-or-uncomment-region)



;;; Generalized version of `read-only-mode'.
(define-key endless/toggle-map "r" #'dired-toggle-read-only)
(autoload 'dired-toggle-read-only "dired" nil t)
(define-key endless/toggle-map "w" #'whitespace-mode)

(defvar tak/magit-diff-highlight-trailing magit-diff-highlight-trailing
  "Enable or disable whitespace highlighting in `magit-mode`.
Used by `tak/toggle-magit-diff-highlight-trailing`.")

(defun tak/toggle-magit-diff-highlight-trailing ()
  "Toggles highlighting of whitespace in `magit-mode` buffers.
Toggles the value of `tak/magit-diff-highlight-trailing`, assigning its
new value to `magit-diff-highlight-trailing`. Does not currently
differentiate `t` from `status` in this variable, and will toggle
between `nil` and `t` no matter the original value of
`magit-diff-highlight-trailing`."
  (interactive)
  (let ((highlight (setq tak/magit-diff-highlight-trailing
                         (not tak/magit-diff-highlight-trailing))))
    (setq magit-diff-highlight-trailing highlight)
    (message "Whitespace highlighting %s"
             (if magit-diff-highlight-trailing
                 '"enabled"
               '"disabled"))))

;; vc/magit-mode bindings
(define-key tak/vc-toggle-map "h" #'magit-diff-toggle-refine-hunk)
(define-key tak/vc-toggle-map "w" #'tak/toggle-magit-diff-highlight-trailing)

(after-load 'aggressive-indent
  (add-to-list 'aggressive-indent-excluded-modes 'cperl-mode t))

(require 'init-private nil t)

(defun put-disabled-commands-in-custom-file (orig-function &rest args)
  "Put disabled command declarations in `custom-file'."
  (let ((user-init-file custom-file))
    (apply orig-function args)))
(advice-add #'en/disable-command :around #'put-disabled-commands-in-custom-file)

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

(dolist (parameter '((width . 100)
                     (height . 55)))
  (add-to-list 'default-frame-alist parameter)
  )

(defun tak/set-default-frame-parameters (&optional arg)
  "Sets some parameters for the current frame.
If prefix ARG is non-nil, sets parameters appropriate for a frame with
2 fullsize vertical windows."
  (interactive "P")
  (let* ((params-mac     '((width . 110)
                           (height . 55)
                           ;;(left . 0)
                           ;;(top . 0)
                           ))
         (params-default '((width . 90)
                           (height . 50)
                           ;;(left . -10)
                           ;;(top . 35)
                           ))
         (params-2window '((width . 220)
                           (height . 55)))
         (params (cond (arg        params-2window)
                       (*is-a-mac* params-mac)
                       (t          params-default))))
    (dolist (parameter params)
      (set-frame-parameter nil (car parameter) (cdr parameter))
      )))

(global-set-key (kbd "C-x C-9") 'tak/set-default-frame-parameters)


;;; Scrolling

;; swap scroll-left and scroll-right default binds
(global-set-key (kbd "C-x >") 'scroll-left)
(global-set-key (kbd "C-x <") 'scroll-right)

;; mouse buttons are different on osx
(when *is-a-mac*
  (global-set-key (kbd "<wheel-right>") 'scroll-left)
  (global-set-key (kbd "<wheel-left>") 'scroll-right))



(require-package 'goto-last-change)
(global-set-key (kbd "C-x C-\\") 'goto-last-change)



;; http://oremacs.com/2015/01/20/introducing-hydra/
;;(require-package 'hydra)



;; org-mode setup

(require-package 'org-plus-contrib)



(require-package 'el-mock)
(require-package 'org-trello)

;; org-trello

;; 2) Once - Install the consumer-key and read/write access-token for org-trello
;; to work in your name with your boards (C-c o i) or
;; M-x org-trello-install-key-and-token
;; (See http://org-trello.github.io/trello-setup.html#credentials for more
;; details)

;; You may want:
;; - to connect your org buffer to an existing board (C-c o I).  Beware that
;; this will only install properties needed to speak with trello board (and
;; nothing else).
;; M-x org-trello-install-board-metadata

;; - to update an existing org-buffer connected to a trello board (C-c o u).
;; M-x org-trello-update-board-metadata

;; - to create an empty board directly from a org-mode buffer (C-c o b)
;; M-x org-trello-create-board-and-install-metadata

;; 3) Now check your setup is ok (C-c o d)
;; M-x org-trello-check-setup

;; 6) For some more help (C-c o h)
;; M-x org-trello-help-describing-setup

;; 7) The first time you attached your buffer to an existing trello board, you
;; may want to bootstrap your org-buffer (C-u C-c o s)
;; C-u M-x org-trello-sync-buffer

;; 8) Sync a card from Org to Trello (C-c o c / C-c o C)
;; M-x org-trello-sync-card

;; 9) Sync a card from Trello to Org (C-u C-c o c / C-u C-c o C)
;; C-u M-x org-trello-sync-card

;; 10) Sync complete org buffer to trello (C-c o s)
;; M-x org-trello-sync-buffer

;; 11) As already mentioned, you can sync all the org buffer from trello
;; (C-u C-c o s) or C-u M-x org-trello-sync-buffer

;; 12) You can delete an entity, card/checklist/item at point (C-c o k)
;; M-x org-trello-kill-entity

;; 13) You can delete all the cards (C-c o K / C-u C-c o k)
;; M-x org-trello-kill-cards / C-u M-x org-trello-kill-entity

;; 14) You can directly jump to the trello card in the browser (C-c o j)
;; M-x org-trello-jump-to-trello-card

;; 15) You can directly jump to the trello board in the browser
;; (C-c o J / C-u C-c o j)
;; M-x org-trello-jump-to-trello-board / C-u M-x org-trello-jump-to-trello-card


(add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))
(defun tak/maybe-enable-org-trello-mode ()
  (let ((filename (buffer-file-name (current-buffer))))
    (when (and filename
               (string= "trello" (file-name-extension filename)))
      (org-trello-mode))))

(after-load 'org
  (require 'org-trello)
  (add-hook 'org-mode-hook #'tak/maybe-enable-org-trello-mode))



;; org-time-budgets
(require-package 'org-time-budgets)
(after-load 'org
  (require 'org-time-budgets))
(setq-default
 org-time-budgets '((:title "Vendasta" :tags "+vendasta" :budget "30:00" :block 'workweek)
                    (:title "Jamtime" :tags "+jamtime" :budget "3:00" :block 'workweek)
                    (:title "Exercise" :tags "+exercise" :budget "3:00" :block 'week)
                    ))

(add-to-list 'org-agenda-custom-commands
             '(("a" "Agenda"
                ((agenda "" ((org-agenda-sorting-strategy '(habit-down time-up priority-down category-keep user-defined-up))))
                 (org-time-budgets-for-agenda)))))



;; org modules init

(after-load 'org
  (dolist (element '(
                     org-bullets
                     org-fstree
                     orgit
                     orglink
                     org-linkany
                     org-trello
                     org-time-budgets
                     ))
    (if (maybe-require-package element)
        (add-to-list 'org-modules element t)))

  (org-load-modules-maybe t)
  (add-hook 'org-mode-hook #'org-bullets-mode)
  (define-key org-mode-map (kbd "C-M-<return>") 'org-insert-todo-heading)
  )



;; shell-mode init

(defun tak/shell-setup-environment (orig-function &rest args)
  "Setup environment for shell.
Sets TERM=xterm-256color"
  (let ((term (getenv "TERM")))
    (setenv "TERM" "xterm-256color")
    (apply orig-function args)
    (setenv "TERM" term)))
(advice-add #'shell :around #'tak/shell-setup-environment)

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

(setq bash-completion-prog (executable-find "bash"))



;;; comint setup

(setq-default comint-prompt-read-only t)
(defun tak/comint-setup ()
  (define-key comint-mode-map [remap kill-region] 'comint-kill-region)
  (define-key comint-mode-map [remap kill-whole-line] 'comint-kill-whole-line)
  (define-key comint-mode-map [remap beginning-of-line] #'comint-bol-or-process-mark)
  (define-key comint-mode-map [remap move-beginning-of-line] #'comint-bol-or-process-mark)
  )

(add-hook 'comint-mode-hook 'tak/comint-setup)

;; translate ANSI color control sequences into text properties
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)



(global-set-key (kbd "C-S-o") 'sanityinc/open-line-with-reindent)



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

(after-load 'projectile
  (add-to-list 'projectile-project-root-files "pytest.ini"))

(projectile-global-mode)

(require-package 'speedbar)
(require-package 'sr-speedbar)
(require-package 'project-persist-drawer)
(require-package 'ppd-sr-speedbar)
(project-persist-drawer-mode t)



(require-package 'free-keys)
(require 'free-keys)

(global-set-key (kbd "C-h C-k") 'free-keys)

;; add super if on mac
(when *is-a-mac*
  (add-to-list 'free-keys-modifiers "s" t)
  (add-to-list 'free-keys-modifiers "H" t))


;; unbound
(require-package 'unbound)
(require 'unbound)
(global-set-key (kbd "C-h C-u") #'describe-unbound-keys)


;; custom
(require-package 'cus-edit+)
(require 'cus-edit+)
(toggle-customize-outside-change-updates -1)
(global-set-key (kbd "C-h M-c")     nil)
(global-set-key (kbd "C-h M-c C")   #'customize-customized)
(global-set-key (kbd "C-h M-c C-c") #'customize-changed)
(global-set-key (kbd "C-h M-c b")   #'customize-browse)
(global-set-key (kbd "C-h M-c c")   #'customize-variable)
(global-set-key (kbd "C-h M-c f")   #'customize-face)
(global-set-key (kbd "C-h M-c g")   #'customize-group)
(global-set-key (kbd "C-h M-c o")   #'customize-option)
(global-set-key (kbd "C-h M-c r")   #'customize-rogue)
(global-set-key (kbd "C-h M-c s")   #'customize-saved)
(global-set-key (kbd "C-h M-c t")   #'customize-themes)
(global-set-key (kbd "C-h M-c u")   #'customize-unsaved)
(global-set-key (kbd "C-h M-c v")   #'customize-variable)



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

(defun tak/quote-grep-read-files (orig-function &rest args)
  "Advice to put quotes around the value returned by `grep-read-files'."
  ;; split-string
  (let* ((result (apply orig-function args))
         (patterns (split-string result)))
    (s-join " " (--map (format "'%s'" it) patterns))))

(advice-add 'grep-read-files :around #'tak/quote-grep-read-files)



(require-package 'eshell-did-you-mean)
(require 'eshell-did-you-mean)
(add-to-list 'eshell-preoutput-filter-functions #'eshell-did-you-mean-output-filter)



(setq find-function-C-source-directory
      (lambda ()
        (expand-file-name "../emacs/src/" data-directory)))



(defvar backup-dir (expand-file-name "~/.emacs.d/backups/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/saves/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix (expand-file-name "~/.emacs.d/auto-save-list/.saves-"))
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))



(require-package 'god-mode)
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)

;; (setq god-exempt-major-modes nil)
;; (setq god-exempt-predicates nil)

(defun tak/god-mode-update-modeline ()
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-local-mode (progn
                            (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
                            (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
          (t (progn
               (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
               (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832")))))))
(add-hook 'god-mode-enabled-hook 'tak/god-mode-update-modeline)
(add-hook 'god-mode-disabled-hook 'tak/god-mode-update-modeline)

;;; from https://github.com/chrisdone/god-mode/issues/77
;;
;; This mortal mode is designed to allow temporary departures from god mode
;; The idea is that within god-mode, you can hit shift-i, type in a few characters
;; and then hit enter to return to god-mode. To avoid clobbering the previous bindings,
;; we wrap up this behavior in a minor-mode.
(define-minor-mode mortal-mode
  "Allow temporary departures from god-mode."
  :lighter " mortal"
  :keymap '(([escape] . (lambda ()
                          "Exit mortal-mode and resume god mode." (interactive)
                          (god-local-mode-resume)
                          (mortal-mode 0))))
  (when mortal-mode
    (god-local-mode-pause)))

(define-key god-local-mode-map (kbd "I") 'mortal-mode)

(defun tak/god-update-cursor ()
  (setq cursor-type
        (if (or god-local-mode buffer-read-only)
            'box
          'bar)))

(add-hook 'god-mode-enabled-hook 'tak/god-update-cursor)
(add-hook 'god-mode-disabled-hook 'tak/god-update-cursor)



;; Cycle between snake case, camel case, etc.
;; https://github.com/akicho8/string-inflection
;;
(require-package 'string-inflection)
(require 'string-inflection)

(defun tak/string-inflection-save-excursion (orig-function &rest args)
  "Advise string-inflection functions to `save-excursion'."
  (save-excursion
    (apply orig-function args)))

(dolist (func #'(string-inflection-ruby-style-cycle
                 string-inflection-java-style-cycle
                 string-inflection-all-cycle
                 string-inflection-toggle
                 string-inflection-camelcase
                 string-inflection-lower-camelcase
                 string-inflection-underscore
                 string-inflection-upcase
                 string-inflection-lisp))
  (advice-add func :around #'tak/string-inflection-save-excursion))

(defun tak/string-inflection-cycle-python ()
  "camelCase => snake_case => UpperCamelCase"
  (interactive)
  (save-excursion
    (insert (tak/string-inflection-python-function (string-inflection-get-current-word t)))))

(defun tak/string-inflection-python-function (str)
  "camelCase => snake_case"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-camelcase-function str))
   ((string-inflection-camelcase-p str)
    (string-inflection-lower-camelcase-function str))
   ((string-inflection-lower-camelcase-p str)
    (string-inflection-underscore-function str))
   (t
    (string-inflection-underscore-function str))))

(fset 'string-inflection-cycle 'string-inflection-all-cycle)
(global-set-key (kbd "C-c I") #'string-inflection-cycle)
(after-load 'python
  (define-key python-mode-map (kbd "C-c I") #'tak/string-inflection-cycle-python))
(define-key mode-specific-map (kbd "i c") #'string-inflection-lower-camelcase)
(define-key mode-specific-map (kbd "i C") #'string-inflection-camelcase)
(define-key mode-specific-map (kbd "i s") #'string-inflection-underscore)
(define-key mode-specific-map (kbd "i u") #'string-inflection-upcase)



;; diffview init
(require-package 'diffview)
;; TODO find sensible bindings for diffview-current, -region, -message



;; advise browse-url-url-at-point to remove trailing quote if it exists

(defun tak/remove-trailing-quote (orig-function &rest args)
  "Advise browse-url-url-at-point to remove any trailing quote char."
  (let* ((result (apply orig-function args))
         (new-result (replace-regexp-in-string "['\"]$" "" result)))
    new-result))
(advice-add #'browse-url-url-at-point :around #'tak/remove-trailing-quote)



(defun tak/quit-other-window ()
  "Switches to other window and runs `quit-window'."
  (interactive)
  (switch-window)
  (quit-window))
(define-key mode-specific-map (kbd "Q") #'tak/quit-other-window)
(define-key mode-specific-map (kbd "C-q") #'tak/quit-other-window)
(define-key mode-specific-map (kbd "q") #'quit-window)



;; zone 'screensaver'
(require 'zone)
(setq zone-programs [
                     ;;zone-pgm-jitter
                     zone-pgm-putz-with-case
                     ;;zone-pgm-dissolve
                     ;;zone-pgm-explode
                     ;;zone-pgm-whack-chars
                     ;;zone-pgm-rotate
                     ;;zone-pgm-rotate-LR-lockstep
                     ;;zone-pgm-rotate-RL-lockstep
                     ;;zone-pgm-rotate-LR-variable
                     ;;zone-pgm-rotate-RL-variable
                     zone-pgm-drip
                     zone-pgm-drip-fretfully
                     zone-pgm-five-oclock-swan-dive
                     zone-pgm-martini-swan-dive
                     zone-pgm-rat-race
                     zone-pgm-paragraph-spaz
                     ;; zone-pgm-stress
                     ;; zone-pgm-stress-destress
                     zone-pgm-random-life
                     ])
;;(zone-when-idle 120)



(require-package 'achievements)
(achievements-mode)
(diminish 'achievements-mode)



;; eshell
(setq
 eshell-prompt-regexp "^\\[[0-9]{4}(-[0-9]{2}){2} [0-9]{2}(:[0-9]{2}){2}\\]\n\\[[^\\]]+\\]\n> *[#$]
*"
 shell-prompt-pattern eshell-prompt-regexp)



;; shell-pop

;; TODO fix split window cmd--splits bottom window then opens shell in right split window
(require-package 'shell-pop)

(setq-default
 shell-pop-universal-key "<f12>"
 shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))
 shell-pop-window-height 60
 shell-pop-window-position "bottom"
 )
(require 'shell-pop)


(setq keyfreq-autosave-mode t
      keyfreq-file (expand-file-name ".keyfreq.el" user-emacs-directory)
      keyfreq-mode t)



;; ;; background process runner
;; (require-package 'bpr)
;; (require 'bpr)

;; ;; Set global config for bpr.
;; ;; Variables below would be applied to all processes.
;; (setq bpr-colorize-output t)
;; (setq bpr-close-after-success t)

;; ;; define function for running desired process
;; (defun run-tests ()
;;   "Spawns 'grunt test' process"
;;   (interactive)
;;   ;; Set dynamic config for process.
;;   ;; Variables below would be applied only to particular process
;;   (let* ((bpr-scroll-direction -1))
;;     (bpr-spawn "grunt test --color")))

;; ;; set key-binding
;; (define-key global-map "\C-ct" 'run-tests)



;; conway's game of life
(require-package 'xbm-life)
(setq-default xbm-life-default-delay 0.5
              xbm-life-default-grid nil
              xbm-life-default-grid-size 80
              xbm-life-delay-minimum 0.1
              xbm-life-delay-step 0.1
              xbm-life-grid-minimum 64
              )



;; neotree
(require-package 'neotree)
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(defun tak/projectile-switch-project ()
  (neotree-projectile-action)
  (magit-status))

(setq projectile-switch-project-action #'tak/projectile-switch-project
      neo-theme 'nerd)



;; alt-tab-like buffer-switching
(require-package 'key-chord)
(require-package 'buffer-flip)

(setq-default buffer-flip-keys "u8*")
(key-chord-mode +1)
(buffer-flip-mode +1)



;; logfile packages
(require-package 'syslog-mode)
(require-package 'itail)
(require 'itail)


;; semantic bovinator
(require-package 'stickyfunc-enhance)
(require 'semantic)
(require 'stickyfunc-enhance)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

(defvar tak/semantic-unwanted-filetypes '("html?")
  "A list of regular expressions matching the extensions of file types
  that should not be processed by semantic")

(defun tak/inhibit-unwanted-semantic-filetypes ()
  "Inhibits semantic-mode for inappropriate filetypes.
Unwanted file types are found by matching the buffer's file extension
with the patterns in `tak/semantic-unwanted-filetypes'."
  (require 'dash)
  (when (buffer-file-name)
    (let* ((file (buffer-file-name))
           (ext (file-name-extension file))
           (case-fold-search t))
      (when ext
        (-any? (lambda (x)
                 (string-match x ext))
               tak/semantic-unwanted-filetypes)))))

(add-to-list 'semantic-inhibit-functions #'tak/inhibit-unwanted-semantic-filetypes)

(global-semantic-stickyfunc-mode +1)
(global-semantic-idle-scheduler-mode +1)
(global-semanticdb-minor-mode +1)
;;(global-semantic-idle-summary-mode +1) -- covered by eldoc mode ?
(semantic-mode +1)




(provide 'init-local)
