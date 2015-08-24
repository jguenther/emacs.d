(setq-default auto-mode-alist (append '(("SConstruct\\'" . python-mode)
                                        ("SConscript\\'" . python-mode))
                                      auto-mode-alist)
              python-indent-offset 4
              elpy-rpc-backend "jedi"
              indent-guide-recursive nil
              jedi:setup-keys t
              jedi:complete-on-dot t
              jedi-direx:hide-imports t)

;; local fork of elpy
(add-to-list 'load-path
             (expand-file-name "~/code/elpy/"))


(require-package 'pip-requirements)
(add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup)

(require-package 'elpy)
(require-package 'yasnippet)

(require-package 'indent-guide)
(require-package 'python-x)



;;;
;;; elpy setup

(after-load 'elpy
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
  
  ;; use jedi completion instead
  (setq elpy-modules (delq 'elpy-module-company elpy-modules))

  (elpy-use-ipython)

  (unless (require 'yasnippet nil t)
    (setq elpy-modules (delq 'elpy-module-yasnippet elpy-modules)))

  ;; remove unnecessary binds
  (cl-dolist (key '(
                                        ; flymake
                    "C-c n" "C-c p" "C-c C-n" "C-c C-p"
                                        ; refactor (deprecated)
                    "C-c C-r"
                                        ; ??
                    "C-c R"
                                        ; elpy-check
                    "C-c C-v"
                    ))
    (define-key elpy-mode-map (kbd key) nil)))

(after-load 'python
  ;; Skeletons
  (define-key python-mode-map (kbd "C-c s c") 'python-skeleton-class)
  (define-key python-mode-map (kbd "C-c s d") 'python-skeleton-def)
  (define-key python-mode-map (kbd "C-c s f") 'python-skeleton-for)
  (define-key python-mode-map (kbd "C-c s i") 'python-skeleton-if)
  (define-key python-mode-map (kbd "C-c s t") 'python-skeleton-try)
  (define-key python-mode-map (kbd "C-c s w") 'python-skeleton-while)

  (add-hook 'python-mode-hook 'superword-mode)
  (add-hook 'python-mode-hook 'indent-guide-mode)
  (add-hook 'magit-blame-mode-hook (lambda () (when indent-guide-mode
                                           (indent-guide-mode))))

  ;; enable this mode in tak/python-setup instead
  ;;(add-hook 'python-mode-hook (lambda () (flycheck-mode -1)))

  (add-hook 'comint-output-filter-functions 'python-pdbtrack-comint-output-filter-function)

  ;; python-x binds
  (define-key python-mode-map (kbd "C-c ! C-j") 'python-shell-send-line)
  (define-key python-mode-map (kbd "C-c ! C-n") 'python-shell-send-line-and-step)
  (define-key python-mode-map (kbd "C-c ! C-f") 'python-shell-send-defun)
  (define-key python-mode-map (kbd "C-c ! C-b") 'python-shell-send-buffer)
  (define-key python-mode-map (kbd "C-c ! C-c") 'python-shell-send-dwim)
  (define-key python-mode-map (kbd "C-c ! p") 'python-shell-print-region-or-symbol)
  )



;;; jedi config

;; run `(jedi:install-server)' manually after installation and after each
;; update to jedi

(require-package 'jedi)
(require-package 'jedi-direx)

(after-load 'jedi
  (define-key python-mode-map (kbd "C-c J t") 'jedi:toggle-log-traceback)
  (define-key python-mode-map (kbd "C-c J d") 'jedi:toggle-debug-server)
  (define-key python-mode-map (kbd "C-c J e") 'jedi:pop-to-epc-buffer)

  (define-key python-mode-map (kbd "C-c C-.") 'jedi:goto-definition-pop-marker)
  (define-key python-mode-map (kbd "C-.")     'jedi:goto-definition-push-marker)
  (define-key python-mode-map (kbd "C-c C-.")   'jedi:goto-definition-push-marker)

  (add-hook 'jedi-mode-hook 'jedi-direx:setup)
  (define-key python-mode-map (kbd "C-c x") 'jedi-direx:pop-to-buffer)
  )

;; pungi (jedi and virtualenv compat)
;;(require-package 'pungi)
;;(add-hook 'python-mode-hook (lambda () (pungi:setup-jedi)))

;;(require-package 'company-jedi)



;;;
;;; documentation setup

(require-package 'pydoc)
(require-package 'python-docstring)
;;(require-package 'python-info)
(require-package 'pydoc-info)

;; (after-load 'python-mode
;;   (require 'pydoc-info))



;;;
;;; ipython setup

;; out of date?
;;
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



;;; testing setup

;;; emacs-dbgr / realgud

;; realgud: use pdb, trepan2, or pydbgr

;; TODO make realgud-trepan2 work with elpy-test
(require-package 'realgud)
;;(require-package 'test-simple)
;;(require-package 'load-relative)
;;(require-package 'loc-changes)

(require-package 'pytest)
(after-load 'python
  (require 'pytest)
  (define-key python-mode-map (kbd "C-c t a") 'pytest-all)
  (define-key python-mode-map (kbd "C-c t m") 'pytest-module)
  (define-key python-mode-map (kbd "C-c t .") 'pytest-one)
  (define-key python-mode-map (kbd "C-c t d") 'pytest-directory)
  (define-key python-mode-map (kbd "C-c p a") 'pytest-pdb-all)
  (define-key python-mode-map (kbd "C-c p m") 'pytest-pdb-module)
  (define-key python-mode-map (kbd "C-c p .") 'pytest-pdb-one)

  (require 'realgud))



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
      (setenv "TERM" "xterm-256color")
      ad-do-it)
    (setenv "PYTHONPATH" pythonpath)
    (setenv "TERM" term)
    ))

;;
;; pdb setup, note the python version
(setq pdb-path '/usr/local/bin/pdb
      gud-pdb-command-name (symbol-name pdb-path))

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
	 		    (file-name-nondirectory buffer-file-name)))))

(defun annotate-pdb-breakpoints ()
  (interactive)
  (highlight-lines-matching-regexp "\\(import i?pdb\\|i?pdb.set_trace()\\)"))

(defun python-add-breakpoint ()
  (interactive)
  (let* ((i (if (string-equal python-shell-interpreter "ipython") "i" ""))
         (breakpoint-string (format "import %spdb; %spdb.set_trace()" i i)))
    (beginning-of-line)
    (newline-and-indent)
    (previous-line)
    (beginning-of-line)
    (insert breakpoint-string)
    (indent-according-to-mode))
  )

(after-load 'python
  (define-key python-mode-map (kbd "C-c <SPC>") 'python-add-breakpoint)
  (define-key python-mode-map (kbd "C-c C-<SPC>") 'python-add-breakpoint)
  (add-hook 'python-mode-hook 'annotate-pdb-breakpoints)
  )



;;; pytest pdb
;; c.f. https://bitbucket.org/hpk42/py-trunk/commits/1d7b0838917f

(defun elpy-copy-test-at-point ()
  "Copy test at point to kill-ring"
  (interactive)
  (let ((test (elpy-test-at-point)))
    (if test
        (kill-new (test))
      (message "ERROR: elpy-test-at-point returned nil")))
  )

(defun elpy-test-pytest-pdb-runner (top file module test)
  "Test the project using the py.test test runner with --pdb -s (capture disabled).

This requires the pytest package to be installed."
  (interactive (elpy-test-at-point))
  (let ((runner-command
         (list (executable-find
                (car elpy-test-pytest-runner-command)))))
    (cond
     (test
      (let ((test-list (split-string test "\\.")))
        (apply #'elpy-test-run-pdb
               top
               (append runner-command
                       (list (mapconcat #'identity (cons file test-list) "::")
                             "--pdb" ;"-s"
                             )))))
     (module
      (apply #'elpy-test-run-pdb top (append runner-command
                                             (list file "--pdb" ;"-s"
                                                   ))))
     (t
      (apply #'elpy-test-run-pdb top (append runner-command
                                             (list "--pdb" ;"-s"
                                                   )))))))

(put 'elpy-test-pytest-pdb-runner 'elpy-test-runner-p t)

(defun elpy-test-run-pdb (working-directory command &rest args)
  "Run COMMAND with ARGS in WORKING-DIRECTORY as a test command using pdb."
  (let* ((default-directory working-directory)
         (gud-chdir-before-run nil)
         (gud-pdb-command-name "python")
         (cmdline (combine-and-quote-strings (cons gud-pdb-command-name (cons command args))))
         )
    (message "running pdb: `%s'" cmdline)
    (pdb cmdline)))

(set 'elpy-test-runner 'elpy-test-pytest-pdb-runner)




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



;;; deferred setup, to deal with pythonpath issues

;;
;; Use the regular major mode hook to add a buffer-local hack-local-variables-hook
;;
;; This is necessary since python sys.path is set in dirlocals which is not
;; visible until after python-mode-hook has run
;;
(defun tak/python-setup ()
  (add-hook
   'hack-local-variables-hook
   (lambda ()
     (add-hook 'python-mode-hook 'jedi:setup)
     
     (add-to-list 'flycheck-disabled-checkers 'python-flake8)
     (if flycheck-mode
         (flycheck-mode -1))
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

(after-load 'python  
  (add-hook 'python-mode-hook 'tak/python-setup)
  )

(elpy-enable)

(provide 'init-python-mode)
