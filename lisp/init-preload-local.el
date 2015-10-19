;;; setup load-path before packages start to load

;; prepend site-lisp dirs
(message "prepending site-lisp dirs to load-path")
(tak/prepend-subdirs-to-load-path
 (expand-file-name "site-lisp/" user-emacs-directory))


;;; Launcher keymap

;;
;; http://endlessparentheses.com/launcher-keymap-for-standalone-features.html
;;
(define-prefix-command 'launcher-map)
;; C-x l is `count-lines-page' by default. If you
;; use that, you can try s-l or <C-return>.
(define-key ctl-x-map (kbd "l") 'launcher-map)
;;(global-set-key (kbd "s-l") 'launcher-map)


;;; Toggle keymap

;;
;; http://endlessparentheses.com/the-toggle-map-and-wizardry.html
;;
(define-prefix-command 'endless/toggle-map)
;; The manual recommends C-c for user keys, but C-x t is
;; always free, whereas C-c t is used by some modes.
(define-key ctl-x-map (kbd "t") 'endless/toggle-map)

;; `vc-mode'-related toggle keymap
(define-prefix-command 'tak/vc-toggle-map)
(define-key endless/toggle-map (kbd "v") #'tak/vc-toggle-map)

;;; need to preload so anzu binds are properly remapped

;; swap bindings for isearch-*-regexp with isearch-*
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)


(provide 'init-preload-local)
