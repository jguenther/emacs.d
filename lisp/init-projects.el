;;; init-projects

(require-package 'projectile)
(require-package 'perspective)
(require-package 'persp-projectile)
(require-package 'nameframe)
(require-package 'nameframe-perspective)

(setq projectile-keymap-prefix (kbd "C-c p"))

(require 'projectile)
(require 'perspective)
(require 'persp-projectile)
(require 'nameframe)
(require 'nameframe-perspective)
(projectile-global-mode)

;; doesn't seem to work without deferring it
(add-hook 'after-init-hook #'persp-mode)
(add-hook 'desktop-after-read-hook #'persp-mode)
(define-key projectile-mode-map (kbd "s-p") 'projectile-persp-switch-project)
(nameframe-perspective-mode t)

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
