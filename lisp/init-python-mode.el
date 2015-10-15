(setq-default auto-mode-alist (append '(("SConstruct\\'" . python-mode)
                                        ("SConscript\\'" . python-mode))
                                      auto-mode-alist)
              python-indent-offset 4
              elpy-rpc-backend "jedi"
              indent-guide-recursive nil
              jedi:setup-keys t
              jedi:use-shortcuts t
              jedi:complete-on-dot nil
              jedi-direx:hide-imports t

              ;; run flycheck-mode after hack-local-vars-hook
              flycheck-global-modes '(not python-mode))

(require-package 'pip-requirements)
(add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup)

(require-package 'yasnippet)

(require-package 'indent-guide)
(require-package 'python-x)
(require-package 'flycheck)



;;;
;;; elpy setup

(quelpa '(elpy :fetcher file :path "~/code/elpy/"))
;;(require-package 'elpy)

(after-load 'elpy
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
  
  ;; use jedi completion instead
  (setq elpy-modules (delq 'elpy-module-company elpy-modules))

  (elpy-use-ipython)

  (unless (require 'yasnippet nil t)
    (setq elpy-modules (delq 'elpy-module-yasnippet elpy-modules)))

  (require 'jedi)
  
  ;; note: this isn't necessary. the jedi epc server only needs to be installed
  ;; in the venv, jedi itself can go in site packages dir
  
  ;; (tak/setup-elpy-rpc)
  )

;; add jedi installdir in venv to elpy-rpc-pythonpath 
(defun tak/setup-elpy-rpc ()
  (let* ((env (if jedi:environment-root
                  jedi:environment-root
                python-environment-default-root-name))
         (env-root (if (file-name-absolute-p env)
                       env
                     (expand-file-name env python-environment-directory)))
         (env-lib-dir (expand-file-name "lib/python2.7/" env-root))
         (env-lib-site-packages-dir (expand-file-name "site-packages" env-lib-dir))
         (new-rpc-pythonpath (tak/append-paths-to-string elpy-rpc-pythonpath env-lib-dir)))
    (setq elpy-rpc-pythonpath new-rpc-pythonpath)
    (message "Set elpy-rpc-pythonpath to %s" elpy-rpc-pythonpath)))

(defun tak/indent-guide-mode-off ()
  (if indent-guide-mode
      (indent-guide-mode)))

(after-load 'python
  (add-hook 'python-mode-hook 'superword-mode)
  (add-hook 'python-mode-hook 'indent-guide-mode)
  (add-hook 'magit-blame-mode-hook 'tak/indent-guide-mode-off)
  )



;;; jedi config

;; run `(jedi:install-server)' manually after installation and after each
;; update to jedi

(require-package 'jedi)
(require-package 'jedi-direx)

(after-load 'jedi
  (setq jedi:install-python-jedi-dev-command
        '("pip" "install" "--upgrade" "git+https://github.com/davidhalter/jedi.git@master#egg=jedi"))
  (add-hook 'jedi-mode-hook 'jedi-direx:setup)
  )

(defun tak/disable-indent-guide-when-completing (orig-function &rest args)
  "Advice to disable `indent-guide-mode' while `jedi:complete' is
running."
  (let* ((guide-active indent-guide-mode)
         (guide-result-before (if guide-active
                                  (indent-guide-mode)))
         (result (apply orig-function args))
         (guide-result-after (if guide-active
                                 (indent-guide-mode))))))

(advice-add 'jedi:complete :around #'tak/disable-indent-guide-when-completing)

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

;; (after-load 'python
;;   (require 'pydoc-info))



;;;
;;; ipython setup

;; ipython setup
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i"
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      )



;;; testing setup

;;; emacs-dbgr / realgud

;; realgud: use pdb, trepan2, or pydbgr

;; TODO make realgud-trepan2 work with elpy-test
(require-package 'realgud)

(after-load 'python
  (require 'realgud)
  (let* ((prefix (if (string-equal python-shell-interpreter "ipython") "i" ""))
         (pdb-executable (format "/usr/local/bin/%spdb" prefix)))
    (setq pdb-path (intern pdb-executable)
          gud-pdb-command-name pdb-executable
          tak/elpy-pytest-pdb-runner-args (list (format "--%spdb" prefix) "-s" "--color=yes"))))

(defun annotate-pdb-breakpoints ()
  (interactive)
  (highlight-lines-matching-regexp "\\(import i?pdb\\|i?pdb.set_trace()\\)"))

(defun python-add-breakpoint ()
  (interactive)
  (let* ((prefix (if (string-equal python-shell-interpreter "ipython")
                     "i"
                   ""))
         (breakpoint-string (format "import %spdb; %spdb.set_trace()" prefix prefix)))
    (beginning-of-line)
    (newline-and-indent)
    (previous-line)
    (beginning-of-line)
    (insert breakpoint-string)
    (indent-according-to-mode)))

(defun tak/setup-python-shell ()
  (let ((setup-string (shell-command-to-string
                       (expand-file-name
                        (format "~/code/scripts/run_pytest.sh -C %s -i"
                                (projectile-project-root))))))
    (setq tak/python-shell-setup-string setup-string)
    (add-to-list 'python-shell-setup-codes 'tak/python-shell-setup-string)
    ))

(after-load 'python
  (add-hook 'python-mode-hook #'tak/setup-python-shell)
  (add-hook 'python-mode-hook 'annotate-pdb-breakpoints)
  (add-hook 'python-mode-hook 'ipretty-mode)
  )

(defun tak/realgud-setup ()
  (define-key realgud:shortkey-mode-map (kbd "C-c C-<SPC>") 'python-add-breakpoint)
  )

(after-load 'realgud
  (add-hook 'realgud-short-key-mode-hook 'tak/realgud-setup)
  )



;;; py.test pdb
;; c.f. https://bitbucket.org/hpk42/py-trunk/commits/1d7b0838917f

(defun elpy-copy-test-at-point ()
  "Copy test at point to kill-ring"
  (interactive)
  (let ((test (elpy-test-at-point)))
    (if test
        (kill-new test)
      (message "ERROR: elpy-test-at-point returned nil")))
  )

(defvar tak/elpy-pytest-pdb-runner-args (list "--pdb" "-s" "--color=yes")) ;  "-x"
(defvar tak/pdb-wrapper-script (expand-file-name "~/code/scripts/run_pytest.sh"))

(defun elpy-test-pytest-pdb-runner (top file module test)
  "Test the project using the py.test test runner with --pdb -s (capture disabled).

This requires the pytest package to be installed."
  (interactive (elpy-test-at-point))
  (let ((runner-command (list tak/pdb-wrapper-script))
        (runner-args (cons "--" tak/elpy-pytest-pdb-runner-args)))
    (cond
     (test
      (let ((test-list (split-string test "\\.")))
        (apply #'elpy-test-run-pdb
               top
               (append runner-command
                       (cons file
                             (cons "-t"
                                   (cons (mapconcat #'identity test-list "::")
                                         runner-args)))))))
     (module
      (apply #'elpy-test-run-pdb top (append runner-command
                                             (cons file runner-args))))
     (t
      (apply #'elpy-test-run-pdb top (append runner-command
                                             runner-args))))))

(defun elpy-test-run-pdb (working-directory command &rest args)
  "Run COMMAND with ARGS in WORKING-DIRECTORY as a test command using pdb."

  (setq comint-process-echoes t
        comint-use-prompt-regexp t
        gud-chdir-before-run nil
        gud-pdb-command-name tak/pdb-wrapper-script)
  
  (let* ((default-directory working-directory)
         (cmdline (combine-and-quote-strings (cons command args)))
         (process-environment (tak/compute-local-python-environment))
         )
    ;; (add-hook 'comint-output-filter-functions 'python-pdbtrack-comint-output-filter-function)
    ;; (message "running pdb: `%s'" cmdline)
    ;; realgud:cmdbuf-associate
    (pdb cmdline)))

(after-load 'elpy
  (put 'elpy-test-pytest-pdb-runner 'elpy-test-runner-p t)
  (set 'elpy-test-runner 'elpy-test-pytest-pdb-runner)
  )

(defun tak/munge-pdb-buffer-name (orig-function &rest args)
  "Advise `realgud-exec-shell' to remove `py.test'
class/method/function specifiers from the resulting buffer name."
  (let* ((buffer (apply orig-function args))
         (old-buffer-name (buffer-name buffer))
         (old-buffer-name-components (s-split "::" old-buffer-name))
         (old-buffer-car (car old-buffer-name-components))
         (new-buffer-name (format "%s shell*" old-buffer-car)))
    (with-current-buffer buffer
      (rename-buffer new-buffer-name t))))
;;(advice-add 'realgud-exec-shell :around #'tak/munge-pdb-buffer-name)



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



(defcustom tak/flycheck-enabled
  t
  "Whether to enable flycheck for the current buffer."
  :group 'tak
  :type 'boolean
  :safe 'booleanp
  )

(defun tak/append-paths-to-string (string paths)
  "Appends PATHS to STRING and returns the new value."
  (let* ((old-paths (if string
                        (s-split path-separator string t)
                      (list)))
         (paths (if (listp paths) 
                    paths
                  (list paths)))
         (new-paths (progn
                      (dolist (path paths)
                        (add-to-list 'old-paths path t))
                      old-paths))
         (new-path-string (string-join new-paths path-separator)))
    new-path-string))

(defun tak/append-paths-to-env-var (variable paths)
  "Appends PATHS to VARIABLE and returns the new value."
  (let* ((old-value (getenv variable))
         (new-path-string (tak/append-paths-to-string old-value paths)))
    new-path-string))

(defun tak/project-pythonpaths ()
  (list))

(defun tak/compute-local-python-environment ()
  "Computes a new `process-environment' that appends absolute paths in
`python-shell-extra-pythonpaths' to the PYTHONPATH environment
variable."
  ;; (message "%s: in tak/compute-local-python-environment" (buffer-name))
  (let* ((extra-pythonpaths (--map (file-truename (concat it "/"))
                                   python-shell-extra-pythonpaths))
         (new-pythonpath (tak/append-paths-to-env-var "PYTHONPATH" extra-pythonpaths)))
    ;; (let* ((project-pythonpaths (tak/project-pythonpaths))
    ;;        (all-extra-pythonpaths (append python-shell-extra-pythonpaths project-pythonpaths))
    ;;        (extra-pythonpaths-truenames (--map (file-truename (concat it "/"))
    ;;                                            all-extra-pythonpaths))
    ;;        (new-pythonpath (tak/append-paths-to-env-var "PYTHONPATH" extra-pythonpaths-truenames)))
    (if (> (length new-pythonpath) 0)
        (cons (concat "PYTHONPATH=" new-pythonpath)
              process-environment)
      process-environment)))

(defun tak/hack-python-locals ()
  (when (eq major-mode 'python-mode)
    (set (make-local-variable 'process-environment)
         (tak/compute-local-python-environment))
    (add-hook 'python-mode-hook 'jedi:setup nil t)
    ;;(add-hook 'python-mode-hook 'flycheck-mode nil t)
    )
  )

(defun tak/python-setup ()
  "Setup python-mode in buffers where this mode is active.

Adds and modifies keybinds and uses hack-local-variables-hook to setup
sys.path."

  (when (eq major-mode 'python-mode)
    ;; (message "%s: in tak/python-setup" (buffer-name))

    (when tak/flycheck-enabled
      (require 'flycheck)
      (flycheck-select-checker 'python-pylint)
      )

    ;; setup python-mode keybinds

    ;; local binds
    (define-key python-mode-map (kbd "C-c C-<SPC>") 'python-add-breakpoint)
    (define-key python-mode-map (kbd "C-c M-p") #'run-python)
    (define-key python-mode-map [C-tab] #'jedi:complete)
    
    ;; elpy--remove unnecessary binds
    (after-load 'elpy
      (cl-dolist (key '(
                                        ; elpy-flymake-next-error
                        "C-c C-n"
                                        ; elpy-flymake-previous-error
                        "C-c C-p"
                                        ; elpy-check
                        "C-c C-v"
                                        ; run-python
                        "C-c C-p"
                        ))
        (define-key elpy-mode-map (kbd key) nil)))
    
    ;; Skeletons
    (define-key python-mode-map (kbd "C-c s c") 'python-skeleton-class)
    (define-key python-mode-map (kbd "C-c s d") 'python-skeleton-def)
    (define-key python-mode-map (kbd "C-c s f") 'python-skeleton-for)
    (define-key python-mode-map (kbd "C-c s i") 'python-skeleton-if)
    (define-key python-mode-map (kbd "C-c s t") 'python-skeleton-try)
    (define-key python-mode-map (kbd "C-c s w") 'python-skeleton-while)
    
    ;; python-x
    (define-key python-mode-map (kbd "C-c e C-j") 'python-shell-send-line)
    (define-key python-mode-map (kbd "C-c e l") 'python-shell-send-line)
    (define-key python-mode-map (kbd "C-c e C-n") 'python-shell-send-line-and-step)
    (define-key python-mode-map (kbd "C-c e C-f") 'python-shell-send-defun)
    (define-key python-mode-map (kbd "C-c e f") 'python-shell-send-defun)
    (define-key python-mode-map (kbd "C-c e C-b") 'python-shell-send-buffer)
    (define-key python-mode-map (kbd "C-c e b") 'python-shell-send-buffer)
    (define-key python-mode-map (kbd "C-c e C-c") 'python-shell-send-dwim)
    (define-key python-mode-map (kbd "C-c e p") 'python-shell-print-region-or-symbol)

    ;; jedi
    (define-key python-mode-map (kbd "C-c J t") 'jedi:toggle-log-traceback)
    (define-key python-mode-map (kbd "C-c J d") 'jedi:toggle-debug-server)
    (define-key python-mode-map (kbd "C-c J e") 'jedi:pop-to-epc-buffer)
    (define-key python-mode-map (kbd "C-c C-.") 'jedi:goto-definition-pop-marker)
    (define-key python-mode-map (kbd "C-.")     'jedi:goto-definition-push-marker)
    (define-key python-mode-map (kbd "C-c C-.") 'jedi:goto-definition-push-marker)
    (define-key python-mode-map (kbd "M-.")     'jedi:goto-definition)
    (define-key elpy-mode-map   (kbd "M-.")     nil)

    ;; jedi-direx
    (define-key python-mode-map (kbd "C-c x") 'jedi-direx:pop-to-buffer)

    ;; realgud
    (define-key python-mode-map (kbd "C-x C-q") 'realgud-short-key-mode)

    ;; override company-mode backend (even though it's disabled)
    (define-key elpy-mode-map (kbd "C-M-i") 'jedi:complete)
    
    ;;
    ;; Use the regular major mode hook to add a buffer-local hack-local-variables-hook
    ;;
    ;; This is necessary since python sys.path is set in dirlocals which is not
    ;; visible until after python-mode-hook has run
    ;;
    (add-hook 'hack-local-variables-hook 'tak/hack-python-locals nil t)
    ;; ;; alternate method
    ;; (add-hook 'hack-local-variables-hook 'run-local-vars-mode-hook)
    ;; (defun run-local-vars-mode-hook ()
    ;;   "Run a hook for the major-mode after the local variables have been processed."
    ;;   (run-hooks (intern (concat (symbol-name major-mode) "-local-vars-hook"))))
    ;; (add-hook 'python-mode-local-vars-hook 'cr/python-mode-shell-setup)

    ))

(after-load 'python
  (add-hook 'python-mode-hook 'tak/python-setup)
  )

;; (after-load 'realgud
;;   (add-hook 'realgud-short-key-mode-hook 'tak/python-setup))

(elpy-enable)



;;; pylint setup

(define-derived-mode pylintrc-mode
    samba-generic-mode "pylintrc"
  "A mode for editing pylintrc files.

\\{pylintrc-mode-map}")

(add-to-list 'auto-mode-alist
             '("/\\.?pylintrc[^/]*\\'" . pylintrc-mode))




(provide 'init-python-mode)
