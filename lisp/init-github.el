(require 'init-git)

(maybe-require-package 'yagist)
(require-package 'github-browse-file)
(magit-define-popup-action 'magit-file-popup
  ?o "Open on GitHub" 'github-browse-file)

(require-package 'bug-reference-github)
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

(maybe-require-package 'github-clone)


(provide 'init-github)
