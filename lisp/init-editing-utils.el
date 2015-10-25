(require-package 'unfill)

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))
(electric-indent-mode 1)

;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)

(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(transient-mark-mode t)


;;; Newline behaviour

(global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)



(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))


(require-package 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)


(require-package 'highlight-symbol)
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))
(add-hook 'org-mode-hook 'highlight-symbol-nav-mode)

(defun sanityinc/maybe-suppress (orig-function &rest args)
  "Suppress symbol highlighting while isearching."
  (unless (or isearch-mode
              (and (boundp 'multiple-cursors-mode) multiple-cursors-mode))
    (apply orig-function args)))

(after-load 'highlight-symbol
  (diminish 'highlight-symbol-mode)
  (advice-add #'highlight-symbol-temp-highlight :around #'sanityinc/maybe-suppress))

;;----------------------------------------------------------------------------
;; Zap *up* to char is a handy pair for zap-to-char
;;----------------------------------------------------------------------------
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)




(setq-default kill-ring-max 1000)

(require-package 'browse-kill-ring)
(require-package 'browse-kill-ring+)
(require-package 'second-sel)

(setq browse-kill-ring-separator "\f")
(global-set-key (kbd "M-Y") 'browse-kill-ring)
(after-load 'browse-kill-ring
  (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))
(after-load 'page-break-lines
  (push 'browse-kill-ring-mode page-break-lines-modes))
(after-load 'browse-kill-ring
  (require 'second-sel)
  (require 'browse-kill-ring+))

;; second-sel recommended binds
(global-set-key (kbd "C-M-y") 'secondary-dwim)
(define-key esc-map "y" 'yank-pop-commands)
(define-key isearch-mode-map (kbd "C-M-y") 'isearch-yank-secondary)
(global-set-key (kbd "C-x C-M-SPC") 'set-secondary-start)
(global-set-key (kbd "C-x C-M-<return>") 'secondary-save-then-kill)

;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(show-paren-mode 1)

;;----------------------------------------------------------------------------
;; Expand region
;;----------------------------------------------------------------------------
(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;----------------------------------------------------------------------------
;; Rectangle selections, and overwrite text when the selection is active
;;----------------------------------------------------------------------------
(setq cua-rectangle-mark-key (kbd "C-S-<return>"))
(cua-selection-mode t)                  ; for rectangles, CUA is nice


;;----------------------------------------------------------------------------
;; Handy key bindings
;;----------------------------------------------------------------------------
;; To be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(defun join-line-with-following-line ()
  "Joins this line to following line."
  (interactive)
  (join-line 1))

;; Vimmy alternatives to M-^ and C-u M-^
(define-key mode-specific-map (kbd "j") 'join-line)
(define-key mode-specific-map (kbd "J") 'join-line-with-following-line)

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(when (maybe-require-package 'avy)
  (autoload 'avy-goto-word-or-subword-1 "avy")
  (global-set-key (kbd "C-M-;") 'avy-goto-word-or-subword-1))

(when (maybe-require-package 'ace-isearch)
  (global-ace-isearch-mode +1)
  (diminish 'ace-isearch-mode))

(require-package 'multiple-cursors)
;; multiple-cursors
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(define-key mode-specific-map (kbd "C-<") 'mc/mark-all-like-this)
(define-key mode-specific-map (kbd "C A") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
(define-key mode-specific-map (kbd "C r") 'set-rectangular-region-anchor)
(define-key mode-specific-map (kbd "C c") 'mc/edit-lines)
(define-key mode-specific-map (kbd "C e") 'mc/edit-ends-of-lines)
(define-key mode-specific-map (kbd "C a") 'mc/edit-beginnings-of-lines)


;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])



(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)


;;----------------------------------------------------------------------------
;; Page break lines
;;----------------------------------------------------------------------------
(require-package 'page-break-lines)
(global-page-break-lines-mode)
(diminish 'page-break-lines-mode)

;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
;;----------------------------------------------------------------------------
(require-package 'move-dup)
(global-set-key [M-up] 'md/move-lines-up)
(global-set-key [M-down] 'md/move-lines-down)
(global-set-key [M-S-up] 'md/move-lines-up)
(global-set-key [M-S-down] 'md/move-lines-down)

(global-set-key (kbd "C-S-N") 'md/duplicate-down)
(global-set-key (kbd "C-S-<down>") 'md/duplicate-down)
(global-set-key (kbd "C-S-P") 'md/duplicate-up)
(global-set-key (kbd "C-S-<up>") 'md/duplicate-up)

;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up


;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(require-package 'whole-line-or-region)
(whole-line-or-region-mode t)
(diminish 'whole-line-or-region-mode)
(make-variable-buffer-local 'whole-line-or-region-mode)

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load 'cua-rect
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name 0)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))

(suspend-mode-during-cua-rect-selection 'whole-line-or-region-mode)




(defun sanityinc/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
	 (do-left-margin (and (bolp) (> (current-left-margin) 0)))
	 (loc (point-marker))
	 ;; Don't expand an abbrev before point.
	 (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
	     (if do-left-margin (indent-to (current-left-margin)))
	     (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'sanityinc/open-line-with-reindent)


;;----------------------------------------------------------------------------
;; Random line sorting
;;----------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))




(require-package 'highlight-escape-sequences)
(hes-mode)


;; instead of guide-key

(require-package 'which-key)
(require 'which-key)
(which-key-mode)
(diminish 'which-key-mode)
(which-key-setup-side-window-right-bottom)

(setq-default
 which-key-show-remaining-keys t
 which-key-sort-order (quote which-key-description-order)
 which-key-idle-delay 1.5
 )


;; from http://emacswiki.org/emacs/DeletingWhitespace
(defun whack-whitespace (&optional arg)
  "Delete all white space from point to the next word.

   With prefix ARG, delete all whitespace before and after point to
   the next word. The only danger in this is that you don't have to
   actually be at the end of a word to make it work. It skips over to
   the next whitespace and then whacks it all to the next word."
  (interactive "P")
  (let ((regexp "[ \r\t\n]+"))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)
    (when arg
      (re-search-backward regexp nil t)
      (replace-match "" nil nil))))

(global-set-key (kbd "C-S-D") #'whack-whitespace)
(global-set-key (kbd "C-x C-S-O") #'delete-blank-lines)



;; highlight the cursor whenever window scrolls
(require-package 'beacon)
(beacon-mode 1)
(diminish 'beacon-mode)

(setq beacon-dont-blink-minor-modes
      '(magit-blame-mode))

(defun tak/current-buffer-beacon-inhibited-p ()
  "Predicate for checking for beacon unwanted minor modes.
Returns non-nil if `current-buffer' has any of
  `beacon-dont-blink-minor-modes' enabled."
  (-any?
   (lambda (it)
     (let* ((symbol (if (symbolp it)
                        it
                      (intern it))))
       (and (boundp symbol)
            (symbol-value symbol))))
   beacon-dont-blink-minor-modes))

(add-hook 'beacon-dont-blink-predicates 'tak/current-buffer-beacon-inhibited-p)

(dolist (mode '(magit-status-mode
                magit-blame-mode
                magit-popup-mode
                Custom-mode))
  (add-to-list 'beacon-dont-blink-major-modes mode))

(setq-default beacon-blink-delay 0.5
              beacon-blink-when-point-moves 10
              beacon-push-mark 20
              beacon-size 50
              column-number-mode t
              )



;; zop-to-char
(require-package 'zop-to-char)
(global-set-key [remap zap-to-char] 'zop-to-char)




(provide 'init-editing-utils)
