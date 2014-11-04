(require 'tabbar)
(tabbar-mode t)

;; change some bindings for tabbar mousewheel
(add-hook 'tabbar-mwheel-mode-hook
          (lambda ()
            (let ((up   (tabbar--mwheel-key tabbar--mwheel-up-event))
                  (down (tabbar--mwheel-key tabbar--mwheel-down-event)))
              (define-key tabbar-mwheel-mode-map `[header-line ,down]
                'tabbar-mwheel-backward-tab)
              (define-key tabbar-mwheel-mode-map `[header-line ,up]
                'tabbar-mwheel-forward-tab)
              (define-key tabbar-mwheel-mode-map `[header-line (control ,down)]
                'tabbar-mwheel-backward-group)
              (define-key tabbar-mwheel-mode-map `[header-line (control ,up)]
                'tabbar-mwheel-forward-group)
              (define-key tabbar-mwheel-mode-map `[header-line (shift ,down)]
                'tabbar-mwheel-backward)
              (define-key tabbar-mwheel-mode-map `[header-line (shift ,up)]
                'tabbar-mwheel-forward)
              )))

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
                                        ; TODO: split buffers by major mode
    ((eq major-mode 'cperl-mode)
     "Perl"
     )
    ((eq major-mode 'emacs-lisp-mode)
     "Elisp"
     )
    (t
     "Other"
     )
    )))

(setq tabbar-buffer-home-button '(("[+]") "[-]")
      tabbar-cycle-scope 'groups
      tabbar-home-button '(("[o]") "[x]")
      tabbar-mwheel-mode t
      tabbar-ruler-swap-faces t
      tabbar-scroll-left-button '((" <") " =")
      tabbar-scroll-right-button '((" >") " =")
      tabbar-separator '("|"))

;;
;; copied from custom.el -- untested
(setq tabbar-button '((t (:inherit tabbar-default :box (:line-width 2 :color "white" :style released-button))))
      tabbar-button-highlight '((t (:inherit tabbar-highlight)))
      tabbar-default '((t (:inherit variable-pitch :background "gray40" :foreground "light gray" :box (:line-width 1 :color "white" :style released-button))))
      tabbar-highlight '((t (:foreground "gray5" :box (:line-width 2 :color "grey75" :style released-button) :underline t)))
      tabbar-modified '((t (:inherit tabbar-default :foreground "green")))
      tabbar-selected '((t (:inherit tabbar-default :background "gray13" :foreground "white" :box (:line-width 1 :color "white" :style pressed-button))))
      tabbar-separator '((t (:inherit tabbar-default :weight semi-bold :height 1.2)))
      tabbar-unselected '((t (:inherit tabbar-button :box (:line-width 1 :color "white" :style released-button)))))

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

(global-set-key [M-S-left] 'tabbar-backward-tab)
(global-set-key [M-S-right] 'tabbar-forward-tab)

(provide 'init-tabbar)
