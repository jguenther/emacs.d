(require-package 'matlab-mode)
(require-package 'ess)

(setq auto-mode-alist
      (append '(("\\.R\\'" . R-mode)
                ("\\.r\\'" . R-mode)
                ("\\.Rd\\'" . Rd-mode)
		("\\.S\\'" . S-mode)
                ("\\.m\\'" . matlab-mode))
              auto-mode-alist))

(autoload 'matlab-mode "matlab-load" "MatLab editing mode" t)
(add-hook 'matlab-mode-hook
          (lambda ()
            (setq matlab-indent-function t)
            (setq matlab-shell-command "matlab")
            ))

(add-hook 'ess-mode-hook
          (lambda ()
            (setq ess-indent-level 2)
            (ess-toggle-underscore nil)
            (setq ess-ask-for-ess-directory nil)
                                        ;otherwise you are prompted each time
                                        ;you start an interactive R session
            (setq ess-eval-visibly-p nil)
                                        ;otherwise C-c C-r (eval region) takes
                                        ;forever
            ))

(autoload 'R "ess-site" "ESS" t)
(autoload 'R-mode "ess-site" "ESS" t)
(autoload 'r-mode "ess-site" "ESS" t)
(autoload 'Rd-mode "ess-site" "ESS" t)
(autoload 'ess-eldoc "ess-site" "ESS" t)
(autoload 'ess-mode "ess-site" "ESS" t)

(provide 'init-stats)
