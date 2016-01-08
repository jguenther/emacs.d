;;; init-projects
(require-package 'projectile)
(require-package 'org-projectile)

(require-package 'perspective)
(require-package 'persp-projectile)
(require-package 'speedbar)
(require-package 'sr-speedbar)
(require-package 'project-persist-drawer)
(require-package 'ppd-sr-speedbar)

(setq-default projectile-enable-caching nil
              projectile-keymap-prefix (kbd "C-c p")
              projectile-completion-system 'helm
              projectile-find-dir-includes-top-level t
              projectile-switch-project-action #'magit-status
              projectile-use-git-grep t
              )

(after-load 'popup-keys-examples
  (global-set-key (kbd "C-c p") 'popup-keys:run-projectile))

(require 'projectile)

(dolist (suffix '(".elc"
                  ".pyc"))
  (add-to-list 'projectile-globally-ignored-file-suffixes suffix))

(defun tak/init-perspective ()
  (require 'perspective)
  (require 'persp-projectile)
  (global-set-key (kbd "s-p") #'projectile-persp-switch-project)
  (projectile-global-mode)
  (project-persist-drawer-mode t)
  )

(add-hook 'after-init-hook #'tak/init-perspective)

;; doesn't seem to work without deferring it
(add-hook 'desktop-after-read-hook #'tak/init-perspective)



;; remove make project type
(remhash 'make projectile-project-types)

;; py.test project type
(projectile-register-project-type 'python-pytest
                                  '("pytest.ini" "setup.py")
                                  nil
                                  (expand-file-name "~/code/scripts/run_pytest.sh")
                                  nil)
(add-to-list 'projectile-project-root-files "pytest.ini")

(defun tak/projectile-test-suffix-pytest (project-type)
  "Advise `projectile-test-suffix-function' for pytest project type."
  (cond
   ((member project-type '(python-pytest)) "_test")))

(add-function :before-until
              projectile-test-suffix-function
              #'tak/projectile-test-suffix-pytest)

;; vbuild project type
(projectile-register-project-type 'python-vbuild
                                  '("tasks.py")
                                  nil
                                  (list "invoke" "test" "-i")
                                  nil)

(add-to-list 'projectile-project-root-files "tasks.py")

(defun tak/projectile-test-suffix-vbuild (project-type)
  "Advise `projectile-test-suffix-function' for vbuild project type."
  (cond
   ((member project-type '(python-vbuild)) "_test")))

(add-function :before-until
              projectile-test-suffix-function
              #'tak/projectile-test-suffix-vbuild)


(provide 'init-projects)
