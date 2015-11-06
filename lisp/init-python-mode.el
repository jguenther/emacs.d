(setq-default auto-mode-alist (append '(("SConstruct\\'" . python-mode)
                                        ("SConscript\\'" . python-mode))
                                      auto-mode-alist)
              python-indent-offset 4
              python-indent-guess-indent-offset nil
              python-indent-guess-indent-offset-verbose nil
              python-fill-docstring-style (quote django)
              
              elpy-rpc-backend "jedi"
              indent-guide-recursive nil
              jedi:setup-keys t
              jedi:use-shortcuts t
              jedi:complete-on-dot nil
              jedi-direx:hide-imports t

              ;; run flycheck-mode after hack-local-vars-hook
              flycheck-global-modes '(not python-mode))

(after-load 'python
  (setq python--prettify-symbols-alist '(("lambda" . 955))))

(require-package 'pip-requirements)
(add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup)

(require-package 'yasnippet)

(require-package 'indent-guide)
(require-package 'python-x)
(require-package 'flycheck)



;;;
;;; elpy setup

;; needed to prevent missing org-babel-header-args-safe-fn error
(require 'init-org)

(quelpa '(elpy :fetcher file :path "~/code/elpy/"))
;;(require-package 'elpy)

(after-load 'elpy
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
  
  ;; use jedi completion instead
  (setq elpy-modules (delq 'elpy-module-company elpy-modules))
  ;; if using company, require company-jedi instead of jedi

  (elpy-use-ipython)

  ;; copy elpy snippets dir if it doesn't exist (due to quelpa not copying it)
  (let* ((elpy-lib-dir (file-name-directory (locate-library "elpy")))
         (elpy-snippets-dir        (expand-file-name "snippets" elpy-lib-dir))
         (elpy-snippets-source-dir (expand-file-name "snippets" "~/code/elpy/")))
    (if (and (not (file-directory-p elpy-snippets-dir))
             (file-directory-p elpy-snippets-source-dir))
        (copy-directory elpy-snippets-source-dir elpy-snippets-dir)))
  
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
  (require 'indent-guide)
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
                                 (indent-guide-mode))))
    result))

(advice-add 'jedi:complete :around #'tak/disable-indent-guide-when-completing)

;; pungi (jedi and virtualenv compat)
;;(require-package 'pungi)
;;(add-hook 'python-mode-hook (lambda () (pungi:setup-jedi)))

;;(require-package 'company-jedi)



;;;
;;; documentation setup

(require-package 'pydoc)
;; broken package
;;(require-package 'python-docstring)
(require-package 'python-info)
(require-package 'pydoc-info)

(after-load 'python
  (require 'python-info)
  (require 'pydoc-info)
  ;;(require 'python-docstring)
  (require 'pydoc))



;;;
;;; ipython setup

;; ipython setup
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args ""
      python-shell-prompt-regexp "\\(In \\[[0-9]+\\]:\\|([Pp]db)\\|ipdb>\\|[(]+trepan2:?.+[)]+\\) "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      )



;;; realgud / pydbgr setup

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
  (highlight-lines-matching-regexp "import trepan.api; *trepan.api.debug()"))

(defun python-add-breakpoint ()
  (interactive)
  (let* ((breakpoint-string "import trepan.api; trepan.api.debug()"))
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
                                (if (projectile-project-p)
                                    (projectile-project-root)
                                  "."))))))
    (setq tak/python-shell-setup-string setup-string)
    (add-to-list 'python-shell-setup-codes 'tak/python-shell-setup-string)
    ))

(after-load 'python
  (add-hook 'python-mode-hook #'tak/setup-python-shell)
  (add-hook 'python-mode-hook #'annotate-pdb-breakpoints)
  (add-hook 'python-mode-hook #'ipretty-mode)
  )

(defun tak/realgud-setup ()
  (define-key realgud:shortkey-mode-map (kbd "C-c C-<SPC>") 'python-add-breakpoint)
  (setq-local paragraph-separate nil)
  )

(after-load 'realgud
  (add-hook 'realgud-short-key-mode-hook #'tak/realgud-setup)
  (add-hook 'realgud-track-mode-hook #'tak/realgud-setup)
  )



;; thing-at-pt functions for elpy

(defun elpy-name-thing-at-point ()
  "Print the name of the thing at point to the echo area."
  (interactive)
  (message (jedi:get-full-name-sync)))

(defun elpy-copy-test-at-point ()
  "Copy test at point to kill-ring"
  (interactive)
  (let* ((test-at-point (elpy-test-at-point))
         (topdir (car test-at-point))
         (test-file (cadr test-at-point))
         (test-module (caddr test-at-point))
         (test-name (cadddr test-at-point))
         (test (if test-name
                   (concat test-module "." test-name)
                 test-module)))
    (kill-new test)
    (message test)))

(defun elpy-copy-buffer-file-module (&optional filename)
  "Copy module path for `filename', or the current buffer's file."
  (interactive)
  (let* ((top (elpy-library-root))
         (file buffer-file-name)
         (module (elpy-test--module-name-for-file top file)))
    (kill-new module)
    (message module)))

(defun elpy-copy-thing-at-point ()
  "Return the package/module path to the thing at point."
  (interactive)
  (let* ((top (elpy-library-root))
         (file buffer-file-name)
         (module (elpy-test--module-name-for-file top file))
         (function-name (python-info-current-defun))
         (symbol (python-info-current-symbol))
         (result (or symbol
                     (and module function-name (concat module "." function-name)))))
    (kill-new result)
    (message result)
    ))

(after-load 'elpy
  (define-key elpy-mode-map (kbd "C-c M-w") #'elpy-copy-thing-at-point)
  (define-key elpy-mode-map (kbd "C-c M-t") #'elpy-copy-test-at-point)
  (define-key elpy-mode-map (kbd "C-c M-m") #'elpy-copy-buffer-file-module)
  (define-key elpy-mode-map (kbd "C-c M-.") #'elpy-copy-buffer-file-module)
  (define-key elpy-mode-map (kbd "C-c M-n") #'elpy-name-thing-at-point)
  )



;; elpy pytest runner functions

(defvar elpy-pytest-capturelog-enabled nil)

(defun elpy-pytest-toggle-capturelog ()
  (interactive)
  (setq elpy-pytest-capturelog-enabled
        (not elpy-pytest-capturelog-enabled)))

(defun tak/elpy-pytest-runner-args (&optional debugger-arg)
  (let* ((pdb-arg (format "--%spdb"
                          (if (string-equal python-shell-interpreter "ipython")
                              "i" "")))
         (debugger-arg (if debugger-arg
                           debugger-arg
                         pdb-arg))
         (args (list debugger-arg "-s" "--color=yes"))
         )
    (if (not elpy-pytest-capturelog-enabled)
        (add-to-list 'args "--nocapturelog" t))
    args))

(defvar tak/pytest-wrapper-script (expand-file-name "~/code/scripts/run_pytest.sh"))



;; pytest with trepan2 debugger

(defun elpy-test-pytest-trepan2-runner (top file module test)
  "Test the project using the py.test test runner with --trepan.
This requires the pytest package to be installed."
  (interactive (elpy-test-at-point))
  (let ((runner-command (list tak/pytest-wrapper-script))
        (runner-args (cons "--" (tak/elpy-pytest-runner-args "--trepan"))))
    (cond
     (test
      (let ((test-list (split-string test "\\.")))
        (apply #'elpy-test-run-trepan2
               top
               (append runner-command
                       (cons file
                             (cons "-t"
                                   (cons (mapconcat #'identity test-list "::")
                                         runner-args)))))))
     (module
      (apply #'elpy-test-run-trepan2 top (append runner-command
                                                 (cons file runner-args))))
     (t
      (apply #'elpy-test-run-trepan2 top (append runner-command
                                                 runner-args))))))

(defun elpy-test-run-trepan2 (working-directory command &rest args)
  "Run COMMAND with ARGS in WORKING-DIRECTORY as a test command using trepan2."
  (let* ((gud-chdir-before-run nil)
         (gud-trepan2-command-name tak/pytest-wrapper-script)
         (default-directory working-directory)
         (cmdline (combine-and-quote-strings (cons command args)))
         (process-environment (tak/compute-local-python-environment))
         )
    
    (setf (gethash "shell" realgud:trepan2-command-hash) python-shell-interpreter)
    (setf (gethash "eval"  realgud:trepan2-command-hash) "pp %s")
    (setf (gethash "trepan2" realgud-command-hash) realgud:trepan2-command-hash)
    (trepan2 cmdline)))

(after-load 'elpy
  (put 'elpy-test-pytest-trepan2-runner 'elpy-test-runner-p t)
  (set 'elpy-test-runner 'elpy-test-pytest-trepan2-runner)
  )




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

    ;; nav rebinds
    (define-key python-mode-map [remap forward-sexp] #'python-nav-forward-sexp-safe)
    (define-key python-mode-map [remap backward-sexp] #'python-nav-backward-sexp-safe)
    (define-key python-mode-map [remap up-list] #'python-nav-up-list)
    (define-key python-mode-map [remap backward-up-list] #'python-nav-backward-up-list)
    (define-key python-mode-map [remap end-of-block] #'python-nav-end-of-block)
    (define-key python-mode-map [remap beginning-of-block] #'beginning-of-block)
    (define-key python-mode-map [remap beginning-of-defun] #'python-nav-beginning-of-defun)
    (define-key python-mode-map [remap end-of-defun] #'python-nav-end-of-defun)
    (define-key python-mode-map [remap forward-defun] #'python-nav-forward-defun)
    (define-key python-mode-map [remap backward-defun] #'python-nav-backward-defun)
    (define-key python-mode-map [remap end-of-statement] #'python-nav-end-of-statement)
    (define-key python-mode-map [remap beginning-of-statement] #'python-nav-beginning-of-statement)
    (define-key python-mode-map [remap forward-statement] #'python-nav-forward-statement)
    (define-key python-mode-map [remap backward-statement] #'python-nav-backward-statement)

    
    (define-key python-mode-map [remap mark-defun] 'python-mark-defun)
    

    ;; local binds
    (define-key python-mode-map (kbd "C-c C-<SPC>") 'python-add-breakpoint)
    (define-key python-mode-map (kbd "C-c M-p") #'run-python)
    (define-key python-mode-map [C-tab] #'jedi:complete)
    
    ;; elpy--remove unnecessary binds
    (after-load 'elpy
      (cl-dolist (key '(
                        "C-c C-n"  ; elpy-flymake-next-error
                        
                        "C-c C-p"  ; elpy-flymake-previous-error
                        
                        "C-c C-v"  ; elpy-check
                        
                        "C-c C-p"  ; run-python
                        
                        "C-c C-s"  ; elpy-rgrep-symbol
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
    (define-key python-mode-map (kbd "M-,")     'jedi:goto-definition-pop-marker)

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

(after-load 'realgud
  (add-hook 'realgud-short-key-mode-hook #'realgud-cmdbuf-stay-in-source-toggle))

(elpy-enable)



;;; pylint setup

(define-derived-mode pylintrc-mode
    samba-generic-mode "pylintrc"
  "A mode for editing pylintrc files.

\\{pylintrc-mode-map}")

(add-to-list 'auto-mode-alist
             '("/\\.?pylintrc[^/]*\\'" . pylintrc-mode))



;;(require-package 'nose)
(quelpa '(nose :fetcher file :path "~/code/nosemacs/"))

(defun tak/nose-project-root-p (dirname)
  "Returns t if DIRNAME is a `projectile-project-root', and nil otherwise."
  (let* ((absdir (directory-file-name (expand-file-name dirname)))
         (default-directory absdir)
         (projectile-root (directory-file-name (projectile-project-root))))
    (string= absdir projectile-root)
    ))

(defun tak/setup-nose ()
  (let ((setup-string (shell-command-to-string
                       (expand-file-name
                        (format "~/code/scripts/run_pytest.sh -C %s -i"
                                (projectile-project-root))))))
    (setq tak/python-shell-setup-string setup-string)
    (add-to-list 'python-shell-setup-codes 'tak/python-shell-setup-string)
    (add-hook 'hack-local-variables-hook 'tak/hack-python-locals nil t)
    ))

(defun tak/compute-nose-extra-args ()
  (--map (format "--with-path=%s" (file-truename (concat it "/")))
         python-shell-extra-pythonpaths)
  )

(defun tak/run-nose-advice (orig-function &rest args)
  "Advise `run-nose' to set `default-directory' to the project root
and add extra args to `nose-extra-args'."
  (let* ((project-root (projectile-project-root))
         (default-directory project-root)
         (nose-extra-args (tak/compute-nose-extra-args)))
    (apply orig-function args)))

(advice-add 'run-nose :around #'tak/run-nose-advice)

(defun tak/set-nose-extra-args (orig-function &rest args)
  "Advise `run-nose' to set `default-directory' to the project root."
  (let* ((project-root (projectile-project-root))
         (default-directory project-root))
    (apply orig-function args)))
(advice-add 'run-nose :around #'tak/chdir-to-project-root)

(after-load 'python
  (require 'nose)
  (add-hook 'python-mode-hook #'nose-mode)
  (setq nose-project-root-test #'tak/nose-project-root-p)
  (define-key nose-mode-map (kbd "C-c a") #'nosetests-all)
  (define-key nose-mode-map (kbd "C-c m") #'nosetests-module)
  (define-key nose-mode-map (kbd "C-c t") #'nosetests-one)
  (define-key nose-mode-map (kbd "C-c ,") #'nosetests-again)
  (define-key nose-mode-map (kbd "C-c A") #'nosetests-pdb-all)
  (define-key nose-mode-map (kbd "C-c M") #'nosetests-pdb-module)
  (define-key nose-mode-map (kbd "C-c .") #'nosetests-pdb-one)
  (add-hook 'compilation-mode-hook #'tak/hack-python-locals)
  )




(provide 'init-python-mode)
