(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)
(add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup)

(require-package 'elpy)

(after-load 'elpy
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))))

(elpy-enable)

(require-package 'flycheck-pyflakes)
(add-hook 'python-mode-hook 'flycheck-mode)

(require-package 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq-default jedi:complete-on-dot t)                 ; optional
;; run `(jedi:install-server)' manually after installation and after each
;; update to jedi

;;(require-package 'pydoc)

;;; disable pylint -- covered by flycheck
;;(require-package 'pylint)
;;(autoload 'pylint "pylint")
;;(add-hook 'python-mode-hook 'pylint-add-menu-items)
;;(add-hook 'python-mode-hook 'pylint-add-key-bindings)

(require-package 'pytest)
(add-hook 'python-mode-hook (lambda () (require 'pytest)))

;; (require-package 'python-x)
;; (eval-after-load 'python
;;   (lambda ()
;;     (require 'python-x)
;;     ;; Suggested keybindings (ESS-like)
;;     (define-key python-mode-map (kbd "C-c C-j") 'python-shell-send-line)
;;     (define-key python-mode-map (kbd "C-c C-n") 'python-shell-send-line-and-step)
;;     (define-key python-mode-map (kbd "C-c C-f") 'python-shell-send-defun)
;;     (define-key python-mode-map (kbd "C-c C-b") 'python-shell-send-buffer)
;;     (define-key python-mode-map (kbd "C-c C-c") 'python-shell-send-dwim)
;;     (define-key python-mode-map (kbd "C-c p") 'python-shell-print-region-or-symbol)))

(require-package 'python-docstring)
;;(require-package 'python-info)
(require-package 'pydoc-info)

(after-load 'python-mode
  (require 'pydoc-info))

;;(require-package 'company-jedi)

;;(require-package helm-flycheck)
;;(require-package helm-pydoc)

;;pdb setup, note the python version
(setq pdb-path '/usr/lib/python2.7/pdb.py
      gud-pdb-command-name (symbol-name pdb-path))

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
	 		    (file-name-nondirectory buffer-file-name)))))

(provide 'init-python-mode)
