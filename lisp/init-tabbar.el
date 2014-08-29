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

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

(global-set-key [M-S-left] 'tabbar-backward-tab)
(global-set-key [M-S-right] 'tabbar-forward-tab)

(provide 'init-tabbar)
