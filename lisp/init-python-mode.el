(setq-default auto-mode-alist (append '(("SConstruct\\'" . python-mode)
                                        ("SConscript\\'" . python-mode))
                                      auto-mode-alist)
              elpy-rpc-backend "jedi"
              indent-guide-recursive t
              jedi:setup-keys t
              jedi:complete-on-dot t)

(require-package 'pip-requirements)
(add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup)

(require-package 'elpy)

(require-package 'yasnippet)

(after-load 'elpy
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
  
  ;; use jedi completion instead
  (setq elpy-modules (delq 'elpy-module-company elpy-modules))
  (unless (require 'yasnippet nil t)
    (setq elpy-modules (delq 'elpy-module-yasnippet elpy-modules)))

                                        ; flymake
  (cl-dolist (key '("n" "p" "C-n" "C-p"
                                        ; refactor (deprecated)
                    "C-r"
                                        ; ??
                    "R"
                    ))
    (define-key elpy-mode-map (kbd key) nil))
  
  ;; Skeletons
  (define-key python-mode-map "\C-csc" 'python-skeleton-class)
  (define-key python-mode-map "\C-csd" 'python-skeleton-def)
  (define-key python-mode-map "\C-csf" 'python-skeleton-for)
  (define-key python-mode-map "\C-csi" 'python-skeleton-if)
  (define-key python-mode-map "\C-cst" 'python-skeleton-try)
  (define-key python-mode-map "\C-csw" 'python-skeleton-while))

(require-package 'indent-guide)
(require 'indent-guide)

;;(require-package 'flycheck-pyflakes)

(require-package 'jedi)


;; run `(jedi:install-server)' manually after installation and after each
;; update to jedi

(require-package 'pydoc)

;;(require-package 'pytest)

(require-package 'python-x)
(eval-after-load 'python
  (lambda ()
    (require 'python-x)    
    (define-key python-mode-map (kbd "C-c ! C-j") 'python-shell-send-line)
    (define-key python-mode-map (kbd "C-c ! C-n") 'python-shell-send-line-and-step)
    (define-key python-mode-map (kbd "C-c ! C-f") 'python-shell-send-defun)
    (define-key python-mode-map (kbd "C-c ! C-b") 'python-shell-send-buffer)
    (define-key python-mode-map (kbd "C-c ! C-c") 'python-shell-send-dwim)
    (define-key python-mode-map (kbd "C-c ! p") 'python-shell-print-region-or-symbol)))

;;; pungi (jedi and virtualenv compat)
;;(require-package 'pungi)
;;(add-hook 'python-mode-hook (lambda () (pungi:setup-jedi)))

(require-package 'python-docstring)
;;(require-package 'python-info)

(require-package 'pydoc-info)
(after-load 'python-mode
  (require 'pydoc-info))

;;(require-package 'company-jedi)

;;pdb setup, note the python version
(setq pdb-path '/usr/lib/python2.7/pdb.py
      gud-pdb-command-name (symbol-name pdb-path))

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
	 		    (file-name-nondirectory buffer-file-name)))))

;; (defvar server-buffer-clients)
;; (when (and (fboundp 'server-start) (string-equal (getenv "TERM") 'xterm))
;;   (server-start)
;;   (defun fp-kill-server-with-buffer-routine ()
;;     (and server-buffer-clients (server-done)))
;;   (add-hook 'kill-buffer-hook 'fp-kill-server-with-buffer-routine))

;; ipython setup
;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args ""
;;       python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;       python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;       python-shell-completion-setup-code
;;       "from IPython.core.completerlib import module_completion"
;;       python-shell-completion-module-string-code
;;       "';'.join(module_completion('''%s'''))\n"
;;       python-shell-completion-string-code
;;       "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")


;; realgud: use pdb, trepan2, or pydbgr

(require-package 'jedi-direx)
(after-load 'python
  (define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer)
  ;;(require 'realgud)
  ;;(require 'pytest)
  )
(add-hook 'jedi-mode-hook 'jedi-direx:setup)

(add-hook 'comint-output-filter-functions 'python-pdbtrack-comint-output-filter-function)

(defun annotate-pdb-breakpoints ()
  (interactive)
  (highlight-lines-matching-regexp "import i?pdb")
  (highlight-lines-matching-regexp "i?pdb.set_trace()"))


(defun python-add-breakpoint ()
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (previous-line)
  (beginning-of-line)
  ;;  (insert "import ipdb; ipdb.set_trace()")
  (insert "import pdb; pdb.set_trace()")
  (indent-according-to-mode)
  )

(define-key python-mode-map (kbd "C-c <SPC>") 'python-add-breakpoint)
;;(define-key python-mode-map (kbd "C-c C-<SPC>") 'python-add-breakpoint)

(add-hook 'python-mode-hook 'indent-guide-mode)
(add-hook 'magit-blame-mode-hook (lambda () (when indent-guide-mode
                                    (indent-guide-mode))))

(add-hook 'python-mode-hook 'annotate-pdb-breakpoints)

;; enable this mode in tak/python-setup instead
(add-hook 'python-mode-hook (lambda () (flycheck-mode -1)))

(add-hook 'python-mode-hook 'superword-mode)

;;; emacs-dbgr / realgud
;;(require-package 'test-simple)
;;(require-package 'load-relative)
;;(require-package 'loc-changes)
;;(require-package 'realgud)


;; Use the regular major mode hook to add a buffer-local hack-local-variables-hook
;;
;; This is necessary since python sys.path is set in dirlocals which is not
;; visible until after python-mode-hook has run
;;
(defun tak/python-setup ()
  ;;(message "in tak/python-setup")
  (add-hook
   'hack-local-variables-hook
   (lambda ()
     ;;(message "in tak/python-setup lambda")
     (add-hook 'python-mode-hook 'jedi:setup)
     ;;(add-hook 'python-mode-hook 'jedi:start-dedicated-server)
     (add-hook 'python-mode-hook 'flycheck-mode)
     
     ;; (setq elpy-rpc-pythonpath (mapconcat 'concat '(elpy-rpc-pythonpath
     ;;                                                python-shell-extra-pythonpaths)))
     
     ) nil t))

;; ;; alternate method
;; (add-hook 'hack-local-variables-hook 'run-local-vars-mode-hook)
;; (defun run-local-vars-mode-hook ()
;;   "Run a hook for the major-mode after the local variables have been processed."
;;   (run-hooks (intern (concat (symbol-name major-mode) "-local-vars-hook"))))
;; (add-hook 'python-mode-local-vars-hook 'cr/python-mode-shell-setup)

(add-hook 'python-mode-hook 'tak/python-setup)

;; TODO make this append to PYTHONPATH instead of replacing it
(defadvice elpy-test (around manipulate-environment activate)
  "Prepends the contents of `python-shell-extra-pythonpaths' to the PYTHONPATH
environment variable."
  (message "in elpy-rpc-open advice")
  (let ((pythonpath (getenv "PYTHONPATH"))
        (term       (getenv "TERM")))
    ;;(unwind-protect)
    (progn
      (setenv "PYTHONPATH"
              (s-join ":" python-shell-extra-pythonpaths))
      (setenv "TERM" "linux")
      ad-do-it)
    (setenv "PYTHONPATH" pythonpath)
    (setenv "TERM" term)
    ))


;; (let ((pythonpath "PYTHONPATH")
;;       (pythonpaths '("a" "b")))
;;   (message
;;    (s-join ":"
;;            (list pythonpaths (if
;;                                  (not
;;                                   (= (length pythonpath)))
;;                                  pythonpath
;;                                nil))
;;            )))

(require 'pytest-emacs)


;; from https://gist.github.com/TauPan/17305751a883005872dc
;; (use-package elpy
;;              :config
;;              (progn (elpy-enable)
;;                     (defun elpy-nose-test-spec (module test)
;;                       (cond (test
;;                              (format "%s:%s" module test))
;;                             (module module)
;;                             (t "")))
;;                     (defun elpy-test-nose-pdb-runner (top file module test)
;;                       "Test the project using the nose test runner with the --pdb arg.

;; This requires the nose package to be installed."
;;                       (interactive (elpy-test-at-point))
;;                       (let ((default-directory top))
;;                         (pdb (format "nosetests --pdb %s"
;;                                      (elpy-nose-test-spec module test)))))
;;                     (put 'elpy-test-nose-pdb-runner 'elpy-test-runner-p t)
;;                     (defvar elpy-test-pdb-runner
;;                       #'elpy-test-nose-pdb-runner
;;                       "Test runner to run with pdb")
;;                     (defun elpy-test-django-nose-pdb-runner (top file module test)
;;                       "Test the project using the django-nose test runner with the --pdb arg.

;; This requires the django-nose package to be installed and
;; properly configured for the django project."
;;                       (interactive (elpy-test-at-point))
;;                       (let ((default-directory top))
;;                         (pdb (format "django-admin.py test --noinput %s --pdb"
;;                                      (elpy-nose-test-spec module test)))))
;;                     (defun elpy-test-pdb (&optional test-whole-project)
;;                       "Run test the current project with the elpy-test-pdb-runner

;;             prefix args have the same semantics as for `elpy-test'"
;;                       (interactive "P")
;;                       (let ((elpy-test-runner elpy-test-pdb-runner))
;;                         (elpy-test test-whole-project)))
;;                     (eval-after-load 'elpy
;;                       '(progn
;;                          (define-key elpy-mode-map
;;                            (kbd "C-c t") 'elpy-test-pdb)))))

;;; from https://github.com/russell/dotfiles/blob/master/emacs.d/init-programming.d/python.el
;; (defvar python-source-setup-code
;;   "def source(filename):
;;     import os
;;     import subprocess
;;     command = ['bash', '-c', 'source %s && env' % filename]
;;     proc = subprocess.Popen(command, stdout = subprocess.PIPE)
;;     for line in proc.stdout:
;;         (key, _, value) = line.partition(\"=\")
;;         os.environ[key] = value[:-1]  # strip newline
;;     proc.communicate()
;; ")

;; (add-to-list 'python-shell-setup-codes 'python-source-setup-code)


;;; for virtualenv setup with jedi (out of date?):
;;; http://stackoverflow.com/questions/21246218/how-can-i-make-emacs-jedi-use-project-specific-virtualenvs

(elpy-enable)


(provide 'init-python-mode)
