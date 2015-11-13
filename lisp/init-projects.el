;;; init-projects
(require-package 'projectile)
(require-package 'perspective)
(require-package 'persp-projectile)
(require-package 'nameframe)
(require-package 'nameframe-perspective)

(setq-default projectile-enable-caching t
              rojectile-keymap-prefix (kbd "C-c P")
              )

(after-load 'popup-keys-examples
  (global-set-key (kbd "C-c p") 'popup-keys:run-projectile))

(require 'projectile)
(defun tak/init-perspective ()
  (require 'perspective)
  (require 'persp-projectile)
  (require 'nameframe)
  (require 'nameframe-perspective)
  ;;(persp-mode)
  ;;(nameframe-perspective-mode t)

  (global-set-key (kbd "s-p") #'projectile-persp-switch-project)
  (projectile-global-mode)
  )

(add-hook 'after-init-hook #'tak/init-perspective)

;; doesn't seem to work without deferring it
(add-hook 'desktop-after-read-hook #'tak/init-perspective)

(require-package 'org-projectile)
(require-package 'speedbar)
(require-package 'sr-speedbar)
(require-package 'project-persist-drawer)
(require-package 'ppd-sr-speedbar)
(project-persist-drawer-mode t)



;; remove make project type
(remhash 'make projectile-project-types)

;; py.test project type
(projectile-register-project-type 'python-pytest
                                  '("pytest.ini" "setup.py")
                                  nil
                                  (expand-file-name "~/code/scripts/run_pytest.sh")
                                  nil)

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

(defun tak/projectile-test-suffix-vbuild (project-type)
  "Advise `projectile-test-suffix-function' for vbuild project type."
  (cond
   ((member project-type '(python-vbuild)) "_test")))

(add-function :before-until
              projectile-test-suffix-function
              #'tak/projectile-test-suffix-vbuild)


(provide 'init-projects)
