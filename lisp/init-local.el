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
(global-set-key (quote [f5]) (quote compile))
(global-set-key (quote [f7]) (quote recompile))
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

(require 'tabbar)
(tabbar-mode t)

;; change some bindings for tabbar mousewheel
(add-hook 'tabbar-mwheel-mode-hook
          '(lambda ()
             (define-key tabbar-mwheel-mode-map
               [header-line (kbd "<mouse-4>")] 'tabbar-mwheel-backward-tab)
             (define-key tabbar-mwheel-mode-map
               [header-line (kbd "<mouse-5>")] 'tabbar-mwheel-forward-tab)
             (define-key tabbar-mwheel-mode-map
               [header-line (kbd "<C-mouse-4>")] 'tabbar-mwheel-backward-tab)
             (define-key tabbar-mwheel-mode-map
               [header-line (kbd "<C-mouse-5>")] 'tabbar-mwheel-forward-tab)))

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
   This function is a custom function for tabbar-mode's tabbar-buffer-groups.
   This function group all buffers into 3 groups:
   Those Dired, those user buffer, and those emacs buffer.
   Emacs buffer are those starting with “*”."
  (list
   (cond
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs Buffers"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    (t
     "User Buffers"
     )
    ))) 

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

(global-set-key [M-S-left] 'tabbar-backward)
(global-set-key [M-S-right] 'tabbar-forward)

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

(provide 'init-local)
