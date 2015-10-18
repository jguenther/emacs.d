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

 ;; blink-cursor mode
 blink-cursor-blinks 5
 blink-cursor-delay 1
 blink-cursor-interval 0.8
 )

(blink-cursor-mode t)

(defvar user-home-directory (expand-file-name "~")
  "The absolute path to the user's home directory `~'.")

;;(dolist (path '("/home/jguenther/.emacs-lisp"
;;                "/usr/local/share/emacs/site-lisp"
;;                "/home/jguenther/share/emacs/site-lisp"
;;                "/usr/share/emacs/site-lisp"))
;;  (add-to-list 'load-path path))

(mouse-wheel-mode 1)

(require-package 'hl-line+)
(require 'hl-line+)
;;(toggle-hl-line-when-idle 1)
;;(hl-line-when-idle-interval 2)
(setq-default hl-line-face 'highlight
              hl-line-inhibit-highlighting-for-modes '(custom-mode
                                                       magit-status-mode
                                                       magit-diff-mode
                                                       magit-log-mode))
(global-hl-line-mode)

(require-package 'bookmark+)
(after-load 'bookmark
  (require 'bookmark+))

;; (require-package 'doremi)
;; (require 'doremi)
;; (require-package 'doremi-frm)
;; (require 'doremi-frm)
;; (require-package 'doremi-cmd)
;; (require 'doremi-cmd)
;; (autoload 'define-doremi "doremi-mac" "Define a Do Re Mi command." nil 'macro)

(require-package 'menu-bar+)
(after-load 'menu-bar
  (require 'menu-bar+))

(defun tak/auto-revert-mode-off ()
  (interactive)
  (auto-revert-mode -1))

(when *is-a-mac*
  ;; rebind home/end for macos
  (global-set-key (kbd "<home>") 'move-beginning-of-line)
  (global-set-key (kbd "<end>") 'move-end-of-line)
  
  ;; unbind CMD-w kill-frame
  (global-unset-key (kbd "s-w"))

  (global-set-key (kbd "s-/") 'hippie-expand)

  ;; paste utf-8
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

  ;; disable global-auto-revert-mode due to corruption bug
  ;;   fixed in 25?
  ;;(add-hook 'find-file-hook 'tak/auto-revert-mode-off)
  )



(require 'mmm-auto)
(mmm-add-classes
 `((shell-script-python
    :submode python-mode
    :front ,(concat "^[	 ]*#?[	 ]*%{" (mmm-regexp-opt '("begin_python")) "}%
")
    :back ,(concat "^[	 ]*#?[	 ]*%{" (mmm-regexp-opt '("end_python")) "}%")
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



;; (require-package 'imenu)
;; (require-package 'imenu+)
;; (require-package 'imenu-anywhere)

(defun try-to-add-imenu ()
  (condition-case nil (imenu-add-to-menubar "imenu") (error nil)))

;; (add-hook 'font-lock-mode-hook 'try-to-add-imenu)

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
  
  ;; (global-git-gutter-mode t)
  (global-git-gutter+-mode t)
  (diminish 'git-gutter+-mode)
  
  ;; workaround visual bug with git-gutter and org-indent-mode
  (add-to-list 'git-gutter:disabled-modes 'org-mode)
  
  ;;(global-set-key (kbd "C-c u t") 'git-gutter+:toggle)
  (global-set-key (kbd "C-x v =") 'git-gutter+-popup-hunk)
  (define-key mode-specific-map (kbd "u =") 'git-gutter+-popup-hunk)

  ;; Jump to next/previous hunk
  (define-key mode-specific-map (kbd "u p") 'git-gutter+-previous-hunk)
  (define-key mode-specific-map (kbd "u n") 'git-gutter+-next-hunk)

  ;; Stage current hunk
  (define-key mode-specific-map (kbd "u s") 'git-gutter+-stage-hunks)

  ;; Revert current hunk
  (define-key mode-specific-map (kbd "u r") 'git-gutter+-revert-hunk)

  )

(after-load 'magit
  (require 'git-gutter)
  (require 'git-gutter+)
  ;; (require 'git-gutter-fringe)

  (magit-wip-after-save-mode 1)
  (magit-wip-before-change-mode 1)
  
  (diminish 'magit-wip-before-change-mode)
  (diminish 'magit-wip-after-save-local-mode)

  (global-magit-file-buffer-mode)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(define-key endless/toggle-map "f" #'auto-fill-mode)
(define-key endless/toggle-map "t" #'tak/toggle-visual-line-mode)
                                        ; instead of truncate-line-mode
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

;; (dolist (parameter '((width . 90)
;;                      (height . 55)))
;;   (add-to-list 'default-frame-alist parameter)
;;   (add-to-list 'initial-frame-alist parameter)
;;   )

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

(defun tak/org-mode-setup ()
  (define-key org-mode-map (kbd "C-M-<return>") 'org-insert-todo-heading)
  (org-bullets-mode 1)
  )


(after-load 'org
  (dolist (element '(
                     org-bullets
                     ;;org-cliplink
                     ;;org-elisp-help
                     org-fstree
                     
                     ;;org-gcal
                     
                     orgit
                     orglink
                     
                     ;; disabled -- don't use anything.el/helm.el yet
                     ;;org-linkany
                     
                     org-repo-todo
                                        ;org-trello
                     ))
    (if (maybe-require-package element)
        (add-to-list 'org-modules element t)))
  
  (org-load-modules-maybe t)
  (add-hook 'org-mode-hook 'tak/org-mode-setup)
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



;;; comint setup

(setq-default comint-prompt-read-only t)
(defun tak/comint-setup ()
  (define-key comint-mode-map [remap kill-region] 'comint-kill-region)
  (define-key comint-mode-map [remap kill-whole-line] 'comint-kill-whole-line))

(add-hook 'comint-mode-hook 'tak/comint-setup)
(add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)

;; translate ANSI color control sequences into text properties
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)



(global-set-key (kbd "C-S-o") 'sanityinc/open-line-with-reindent)



;; discovery modes

(require-package 'discover)

(quelpa '(discover-my-major :fetcher file :path "~/code/discover-my-major/"))

(require-package 'discover-my-major)
(require 'discover-my-major)

(global-set-key (kbd "C-h M-m") 'discover-my-major)
(global-set-key (kbd "C-h C-M-m") 'discover-my-mode)
(global-set-key (kbd "C-h M-M") 'discover-my-mode)

(require 'discover)
(global-discover-mode 1)

(defun tak/unhighlight-symbol-at-point ()
  (interactive)
  (highlight-symbol-remove-symbol (highlight-symbol-get-symbol)))

(discover-add-context-menu
 :context-menu '(isearch
                 (description "Isearch, occur and highlighting")
                 (lisp-switches
                  ("-i" "Case should fold search" case-fold-search t nil))
                 (lisp-arguments
                  ("=l" "context lines to show (occur)"
                   "list-matching-lines-default-context-lines"
                   (lambda (dummy) (interactive) (read-number "Number of context lines to show: "))))
                 (actions
                  ("Isearch"
                   ("_" "isearch forward symbol" isearch-forward-symbol)
                   ("w" "isearch forward word" isearch-forward-word))
                  ("Occur"
                   ("o" "occur" occur))
                  ("More"
                                        ; local change
                   ("M-w" "search for words using eww" eww-search-words)
                   ("h" "highlighters ..." makey-key-mode-popup-isearch-highlight))))
 :bind "M-s")

(discover-add-context-menu
 :context-menu '(isearch-highlight
                 (actions
                  ("Highlight"
                                        ; local changes
                   ("." "highlight symbol at point" highlight-symbol-at-point)
                   ("," "unhighlight symbol at point" tak/unhighlight-symbol-at-point)
                   
                   ("l" "highlight lines matching regexp" highlight-lines-matching-regexp)
                   ("p" "highlight phrase" highlight-phrase)
                   ("r" "highlight regexp" highlight-regexp)
                   ("u" "unhighlight regexp" unhighlight-regexp))

                  ("Store"
                   ("f" "hi lock find patterns" hi-lock-find-patterns)
                   ("w" "hi lock write interactive patterns" hi-lock-write-interactive-patterns)))))



;; popup-keys

(quelpa '(popup-keys :fetcher file :path "~/code/popup-keys/"))

(require 'popup-keys)
(require 'popup-keys-examples)

(global-set-key (kbd "C-x M-/") 'popup-keys:run-findtool)

(eval-after-load "vc-hooks"
  '(progn
     ;; move original keymap
     (define-key ctl-x-map "V" 'vc-prefix-map)
     ;; run popup on original key
     (define-key ctl-x-map "v" 'popup-keys:run-vc)
     ))
(global-set-key (kbd "C-x v") 'popup-keys:run-vc)

(define-key mode-specific-map (kbd "p") 'popup-keys:run-projectile)

(global-set-key (kbd "C-x C-k")   'popup-keys:run-kmacro)
(global-set-key (kbd "C-x C-S-k") 'kmacro-keymap)

(global-set-key (kbd "C-x r") 'popup-keys:run-registers)
(global-set-key (kbd "C-x R") ctl-x-r-map)
;; undo-tree annoyingly binds to the C-x r prefix and overrides the above.
(eval-after-load "undo-tree"
  '(define-key undo-tree-map (kbd "C-x r") nil))

(add-hook 'dired-mode-hook
          (lambda ()
            (require 'dired-x)
            (define-key dired-mode-map (kbd "?") 'popup-keys:run-dired)
            (define-key dired-mode-map (kbd "%") 'popup-keys:run-dired-regexp)
            (define-key dired-mode-map (kbd "*") 'popup-keys:run-dired-mark)))

(eval-after-load "ibuffer"
  '(progn
     (define-key ibuffer-mode-map (kbd "?") 'popup-keys:run-ibuffer)
     (define-key ibuffer-mode-map (kbd "*") 'popup-keys:run-ibuffer-mark)
     (define-key ibuffer-mode-map (kbd "/") 'popup-keys:run-ibuffer-filter)
     ))

(eval-after-load "undo-tree"
  '(define-key undo-tree-visualizer-mode-map (kbd "?") 'popup-keys:run-undo-tree))

(after-load 'org
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key org-mode-map (kbd "M-S-s") 'popup-keys:run-org-speed))))



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



;; eshell
(setq
 eshell-prompt-regexp "^\\[[0-9]{4}(-[0-9]{2}){2} [0-9]{2}(:[0-9]{2}){2}\\]\n\\[[^\\]]+\\]\n> *[#$]
*"
 shell-prompt-pattern eshell-prompt-regexp)



;; shell-pop
;; TODO fix split window cmd--splits bottom window then opens shell in right split window
(require-package 'shell-pop)
(setq shell-pop-universal-key "C-t")
(require 'shell-pop)




(provide 'init-local)
