(when *is-a-mac*
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  (setq default-input-method "MacOSX")
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "s-;") 'ns-next-frame)
  ;; (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  ;; (global-set-key (kbd "M-˙") 'ns-do-hide-others)
  ;; (after-load 'nxml-mode
  ;;   (define-key nxml-mode-map (kbd "M-h") nil))
  (global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h

  (global-set-key (kbd "s-<up>") 'beginning-of-buffer)
  (global-set-key (kbd "s-<down>") 'end-of-buffer)

  (global-set-key (kbd "<home>") 'move-beginning-of-line)
  (global-set-key (kbd "<end>") 'move-end-of-line)

  ;; unbind CMD-w kill-frame
  (global-unset-key (kbd "s-w"))

  (global-set-key (kbd "s-/") 'hippie-expand)

  ;; paste utf-8
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  )


(provide 'init-osx-keys)
