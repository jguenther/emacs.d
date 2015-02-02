;;; Launcher keymap

;;
;; http://endlessparentheses.com/launcher-keymap-for-standalone-features.html
;;
(define-prefix-command 'launcher-map)
;; C-x l is `count-lines-page' by default. If you
;; use that, you can try s-l or <C-return>.
(define-key ctl-x-map "l" 'launcher-map)
;;(global-set-key (kbd "s-l") 'launcher-map)


;;; Toggle keymap

;;
;; http://endlessparentheses.com/the-toggle-map-and-wizardry.html
;;
(define-prefix-command 'endless/toggle-map)
;; The manual recommends C-c for user keys, but C-x t is
;; always free, whereas C-c t is used by some modes.
(define-key ctl-x-map "t" 'endless/toggle-map)


(provide 'init-preload-local)
