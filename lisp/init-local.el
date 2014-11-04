(setq
   backup-by-copying t               ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)                ; use versioned backups

(setq visible-bell t)
(setq load-path (cons "/home/jguenther/.emacs-lisp" load-path))
(setq load-path (cons "/usr/local/share/emacs/site-lisp" load-path))
(setq load-path (cons "/home/jguenther/share/emacs/site-lisp" load-path))
(setq load-path (cons "/usr/share/emacs/site-lisp" load-path))

(require 'ess-site)

;
; override keybindings with new minor mode
;
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

; custom keybindings
(define-key my-keys-minor-mode-map (kbd "C-S-i") 'imenu)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

; make sure my-keys-minor-mode doesn't mess with minibuffer
(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(global-set-key "\M-g" 'goto-line)
(mouse-wheel-mode 1)

(require 'bookmark+)

(require 'doremi)
(require 'doremi-frm)
(require 'doremi-cmd)
(autoload 'define-doremi "doremi-mac"
  "Define a Do Re Mi command." nil 'macro)

(require 'help+)
(require 'help-fns+)
(require 'thumb-frm)
(require 'menu-bar+)

;(require 'frame-cmds)
;(require 'compile-)
;(require 'compile+)
;(require 'lib-requires)

(defun try-to-add-imenu ()
 (condition-case nil (imenu-add-to-menubar "imenu") (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)

; imenu bindings
(global-set-key [S-mouse-3] 'imenu)
(global-set-key (kbd "C-'") 'imenu-anywhere)

(global-font-lock-mode 1)

;(require 'grep-buffers)

(show-paren-mode 1)

(column-number-mode 1)
(line-number-mode 1)

(defsubst yank-secondary ()
  "Insert the secondary selection at point.
  Moves point to the end of the inserted text.  Does not change mark."
  (interactive) (insert (x-get-selection 'SECONDARY)))

;; (setq hcz-set-cursor-color-color "")
;; (setq hcz-set-cursor-color-buffer "")
;; (defun hcz-set-cursor-color-according-to-mode ()
;;   "change cursor color according to some minor modes."
;;   ;; set-cursor-color is somewhat costly, so we only call it when needed:
;;   (let ((color
;; 	 (if buffer-read-only "black"
;; 	   (if overwrite-mode "red"
;; 	     "blue"))))
;;     (unless (and
;; 	     (string= color hcz-set-cursor-color-color)
;; 	     (string= (buffer-name) hcz-set-cursor-color-buffer))
;;       (set-cursor-color (setq hcz-set-cursor-color-color color))
;;       (setq hcz-set-cursor-color-buffer (buffer-name)))))
;; (add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)

(require 'cursor-chg)
(toggle-cursor-type-when-idle 1) ; Turn on cursor change when Emacs is idle
(change-cursor-mode 1)           ; Turn on change for overwrite, read-only, and input mode

(setq fill-column 79)

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
      (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

(defalias 'perl-mode 'cperl-mode)
(require 'cperl-mode)

(add-to-list 'auto-mode-alist
             '("\\.\\([pP][Llm]\\|al\\|t\\|xs\\|tl\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(add-hook 'cperl-mode-hook 'imenu-add-menubar-index)

(add-hook  'cperl-mode-hook
           (lambda ()
             (when (require 'auto-complete nil t)
               (auto-complete-mode t)
               (make-variable-buffer-local 'ac-sources)
               ;; (setq ac-sources
               ;;       '(ac-source-perl-completion))
               )))

(scroll-bar-mode t)

(require 'git-gutter-fringe+)
(global-git-gutter+-mode t)
(git-gutter+-enable-fringe-display-mode)

(require 'win-switch)
(win-switch-setup-keys-esdf "\C-xO")

(defun tak/after-load-info ()
  "Load `info+' and removes its mousewheel bindings."
  (require 'info+)
  (define-key Info-mode-map (kbd "<mouse-4>") nil)
  (define-key Info-mode-map (kbd "<mouse-5>") nil)
  (message "Rebound mousewheel for info."))

(eval-after-load "info" '(tak/after-load-info))

(global-set-key "\C-\M-_" 'undo-tree-redo)

;(autoload 'octave-mode "octave" "Octave Mode" t)
;; (add-to-list
;;  'auto-mode-alist
;;  '("\\.m$" . octave-mode))

(require 'matlab-load)

(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
 (add-to-list
  'auto-mode-alist
  '("\\.m$" . matlab-mode))
 (setq matlab-indent-function t)
 (setq matlab-shell-command "matlab")

(add-to-list 'auto-mode-alist
             '("\\(inputrc\\|bashrc\\)\\'" . sh-mode))

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
(define-key launcher-map "p" #'paradox-list-packages)
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
(define-key endless/toggle-map "q" #'toggle-debug-on-quit)
;(define-key endless/toggle-map "t" #'endless/toggle-theme)
;;; Generalized version of `read-only-mode'.
(define-key endless/toggle-map "r" #'dired-toggle-read-only)
(autoload 'dired-toggle-read-only "dired" nil t)
(define-key endless/toggle-map "w" #'whitespace-mode)

(provide 'init-local)
