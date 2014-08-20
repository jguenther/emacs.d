(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups


(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)


(setq visible-bell t)
(setq load-path (cons "/home/jguenther/.emacs-lisp" load-path))
(setq load-path (cons "/usr/local/share/emacs/site-lisp" load-path))
(setq load-path (cons "/home/jguenther/share/emacs/site-lisp" load-path))
;(setq load-path (cons "/usr/share/emacs/site-lisp/git" load-path))
(setq load-path (cons "/usr/share/emacs/site-lisp" load-path))

(require 'ess-site)

;
; override keybindings with new minor mode
;
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

; custom keybindings
;(define-key my-keys-minor-mode-map (kbd "C-i") 'imenu)

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

(defalias 'perl-mode 'cperl-mode)


(add-to-list 'auto-mode-alist
             '("\\.\\([pP][Llm]\\|al\\|t\\|xs\\|tl\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(require 'imenu+)
(require 'menu-bar+)
;(require 'frame-cmds)
;(require 'compile-)
;(require 'compile+)
;(require 'lib-requires)
;(require 'tool-bar+)
;
;(defun try-to-add-imenu ()
;  (condition-case nil (imenu-add-to-menubar "imenu") (error nil)))
; (add-hook 'font-lock-mode-hook 'try-to-add-imenu)

; imenu bindings
(global-set-key [S-mouse-3] 'imenu)
(global-set-key (kbd "C-c i") 'imenu)


(require 'tabbar)
(tabbar-mode 1)
(global-font-lock-mode 1)
(require 'tool-bar+)

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
(setq hcz-set-cursor-color-color "")
(setq hcz-set-cursor-color-buffer "")
(defun hcz-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color
	 (if buffer-read-only "black"
	   (if overwrite-mode "red"
	     "blue"))))
    (unless (and
	     (string= color hcz-set-cursor-color-color)
	     (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)
(setq cperl-invalid-face nil) 
(setq fill-column 79)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(require 'desktop)
(setq history-length 250)
(desktop-save-mode 1)
(setq desktop-buffers-not-to-save
      (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb" 
              "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
      (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

;(require 'ebs)
;(ebs-initialize)
;(global-set-key [(control tab)] 'ebs-switch-buffer)

; disable electric-pair-mode in modes derived from text-mode
(add-hook 'text-mode-hook
          (lambda () (set (make-local-variable 'electric-pair-mode) nil)))


(provide 'init-local)
