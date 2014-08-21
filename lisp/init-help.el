(require 'help-mode)

(add-hook 'ido-setup-hook 'ido-my-keys)

(defun ido-my-keys ()
  "Set up the keymap for `ido'."

  (define-key help-mode-map [(meta left)] 'help-go-back)
  (define-key help-mode-map [(meta right)] 'help-go-forward)
  )


(provide 'init-help)
