(require-package 'helm)

(require-package 'helm-descbinds)
(require-package 'helm-flycheck)
(require-package 'helm-git-grep)
(require-package 'helm-orgcard)
(require-package 'helm-projectile)
(require-package 'helm-pydoc)
(require-package 'wgrep-helm)
(require-package 'ag)
(require-package 'helm-ag)
(require-package 'helm-fuzzier)
(require-package 'helm-google)
(require-package 'helm-themes)
(require-package 'ace-jump-helm-line)
(require-package 'helm-grepint)

(quelpa '(helm-ipython :fetcher github :repo "thierryvolpiatto/helm-ipython"))

(after-load 'helm-config
  (require 'helm-ipython)
  (require 'ace-jump-helm-line)
  (after-load 'helm-config
    (define-key helm-map (kbd "C-'") 'ace-jump-helm-line)))

(when (sanityinc/dash-installed-p)
  (require-package 'helm-dash))

(require 'helm-config)
(helm-mode t)
(helm-adaptive-mode t)
(helm-push-mark-mode 1)
(helm-descbinds-mode)

;;; http://emacs.stackexchange.com/questions/2867/how-should-i-change-my-workflow-when-moving-from-ido-to-helm
(let ((data-dir (expand-file-name "data" user-emacs-directory)))
  (unless (file-exists-p data-dir)
    (make-directory data-dir)))

(setq-default helm-ff-transformer-show-only-basename nil
              helm-adaptive-history-file             "~/.emacs.d/data/helm-history"
              helm-yank-symbol-first                 t
              helm-move-to-line-cycle-in-source      t
              helm-buffers-fuzzy-matching            t
              helm-ff-auto-update-initial-value      t
              ido-use-virtual-buffers                t   ; Needed in helm-buffers-list
              helm-org-headings-fontify              t

              helm-autoresize-max-height             80  ; %
              helm-autoresize-min-height             20  ; %
              helm-buffers-to-resize-on-pa           '("*helm apropos*" "*helm ack-grep*"
                                                       "*helm grep*" "*helm occur*" "*helm ag*"
                                                       "*helm multi occur*" "*helm git-grep*"
                                                       "*helm imenu*" "*helm imenu all*"
                                                       "*helm gid*" "*helm semantic/imenu*")
              helm-autoresize-mode t
              helm-candidate-separator (make-string 20 ?\x2015)
              helm-follow-mode-persistent t
              helm-idle-delay 0.01
              helm-input-idle-delay 0.01
              helm-persistent-action-use-special-display t
              helm-quick-update t

              helm-dabbrev-cycle-threshold 2

              helm-ag-insert-at-point 'symbol
              helm-ag-use-grep-ignore-list t

              helm-google-tld "ca"
              )

(after-load 'helm-grep
  (setq helm-grep-ag-command
        (replace-regexp-in-string "--color" "--nocolor --ignore '.git'" helm-grep-ag-command))
  )

(after-load 'helm-ag
  (setq-default helm-ag-command-option nil)
  (define-key endless/toggle-map (kbd "z") #'tak/toggle-helm-ag-search-zip)
  )

;; TODO don't clobber other args in this variable
(defun tak/toggle-helm-ag-search-zip ()
  (interactive)
  (cond (helm-ag-command-option
         (setq helm-ag-command-option nil)
         (message "Disabled --search-zip in helm-ag"))
        (t
         (setq helm-ag-command-option "--search-zip")
         (message "Enabled --search-zip in helm-ag")))
  )

(dolist (ext '(".gvfs/"
               ".elc"
               ".pyc"))
  (add-to-list 'completion-ignored-extensions ext))

(require 'helm-fuzzier)
(helm-fuzzier-mode 1)

(require 'helm-grepint)
(helm-grepint-set-default-config)

(after-load 'flycheck
  (add-to-list 'flycheck-global-modes 'helm-command-mode t)
  (define-key flycheck-mode-map (kbd "C-c ! h") #'helm-flycheck)
  (define-key helm-command-map (kbd "!") #'helm-flycheck))

(setq projectile-keymap-prefix (kbd "C-c P"))
(after-load 'popup-keys-examples
  (global-set-key (kbd "C-c p") 'popup-keys:run-projectile))

;;; helm-projectile
;;;
(after-load 'projectile
  (require 'helm-projectile)
  (setq-default helm-projectile-sources-list (cons 'helm-source-projectile-files-list
                                                   (remove 'helm-source-projectile-files-list
                                                           helm-projectile-sources-list))))

(helm-projectile-on)

(define-key projectile-mode-map (kbd "C-c p /") #'helm-do-ag-project-root)

;;; https://github.com/thierryvolpiatto/emacs-tv-config/blob/master/init-helm-thierry.el
;;;

(defun helm/turn-on-header-line ()
  (interactive)
  (setq helm-echo-input-in-header-line t)
  (setq helm-split-window-in-side-p t)
  (helm-autoresize-mode -1)
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  (helm-refresh)
  )

(defun helm/turn-off-header-line ()
  (interactive)
  (setq helm-echo-input-in-header-line nil)
  (helm-autoresize-mode 1)
  (setq helm-split-window-in-side-p nil)
  (remove-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  (helm-refresh)
  )

(defun helm/toggle-header-line ()
  (interactive)
  (if helm-echo-input-in-header-line
      (helm/turn-off-header-line)
    (helm/turn-on-header-line)))

;;; Psession windows
;;
(defun helm-psession-windows ()
  (interactive)
  (helm :sources (helm-build-sync-source "Psession windows"
                   :candidates (lambda ()
                                 (sort (mapcar 'car psession--winconf-alist) #'string-lessp))
                   :action (helm-make-actions
                            "Restore" 'psession-restore-winconf
                            "Delete" 'psession-delete-winconf))
        :buffer "*helm psession*"))

(defun helm-ff-candidates-lisp-p (candidate)
  (cl-loop for cand in (helm-marked-candidates)
           always (string-match "\.el$" cand)))

(defmethod helm-setup-user-source ((source helm-source-ffiles))
  (helm-source-add-action-to-source-if
   "Byte compile file(s) async"
   'async-byte-compile-file
   source
   'helm-ff-candidates-lisp-p))

(defmethod helm-setup-user-source ((source helm-source-buffers))
  (set-slot-value source 'candidate-number-limit 200))

;;; Toggle grep program
;;
;;
(defun eselect-grep ()
  (interactive)
  (when (y-or-n-p (format "Current grep program is %s, switching? "
                          (helm-grep-command)))
    (if (helm-grep-use-ack-p)
        (setq helm-grep-default-command
              "grep --color=always -d skip %e -n%cH -e %p %f"
              helm-grep-default-recurse-command
              "grep --color=always -d recurse %e -n%cH -e %p %f")
      (setq helm-grep-default-command
            "ack-grep -Hn --smart-case --no-group %e %p %f"
            helm-grep-default-recurse-command
            "ack-grep -H --smart-case --no-group %e %p %f"))
    (message "Switched to %s" (helm-grep-command))))

(defun tak/disable-kill-buffer-query-functions (orig-function &rest args)
  (let ((kill-buffer-query-functions))
    (apply orig-function args)))

(advice-add #'helm-buffer-run-kill-persistent :around #'tak/disable-kill-buffer-query-functions)
(advice-add #'helm-buffer-run-kill-buffers :around #'tak/disable-kill-buffer-query-functions)

;;; Helm-command-map
;;
;;
(define-key helm-command-map (kbd "w")       #'helm-psession)
(define-key helm-command-map (kbd "z")       #'helm-complex-command-history)
(define-key helm-command-map (kbd "I")       #'helm-imenu-in-all-buffers)
(define-key helm-command-map (kbd "a")       #'helm-ag)
(define-key helm-command-map (kbd "A")       #'helm-do-ag)
(define-key helm-command-map (kbd "F")       #'helm-do-ag-this-file)
(define-key helm-command-map (kbd "M-g g")   #'helm-git-grep)
(define-key helm-command-map (kbd "M-g G")   #'helm-google)
(define-key helm-command-map (kbd "M-g S")   #'helm-google-suggest)
(define-key helm-command-map (kbd "M-g z")   #'helm-do-zgrep)
(define-key helm-command-map (kbd "M-g F")   #'helm-do-ag-this-file)
(define-key helm-command-map (kbd "M-g b")   #'helm-do-ag-buffers)
(define-key helm-command-map (kbd "M-g p")   #'helm-do-ag-project-root)
(define-key helm-command-map (kbd "M-g o")   #'helm-swoop)

(define-key helm-command-map (kbd "M-i")     #'helm-swoop)
(define-key helm-command-map (kbd "s-i")     #'helm-multi-swoop)
(define-key helm-command-map (kbd "M-I")     #'helm-swoop-back-to-last-point)
(define-key helm-command-map (kbd "C-c M-i") #'helm-multi-swoop)
(define-key helm-command-map (kbd "C-x M-i") #'helm-multi-swoop-all)

(define-key helm-command-map (kbd "M-g M-i") #'helm-multi-swoop)
(define-key helm-command-map (kbd "M-g M-I") #'helm-multi-swoop-all)

(define-key helm-command-map (kbd "B")       #'helm-descbinds)
(define-key helm-command-map (kbd "C-x C-b") #'helm-buffers-list)
(define-key helm-command-map (kbd "D")       #'helm-dash-at-point)

(define-key helm-map (kbd "M-o") #'helm-previous-source)
(define-key helm-map (kbd "M-h") #'helm/toggle-header-line)
(define-key helm-map (kbd "s-k") #'helm-buffer-run-kill-persistent)
(define-key helm-map (kbd "s-d") #'helm-buffer-run-kill-buffers)
(define-key helm-map (kbd "s-D") #'helm-buffer-run-kill-buffers)

(define-key shell-mode-map (kbd "C-M-p")             #'helm-comint-input-ring) ; shell history.

(after-load 'org
  (define-key org-mode-map (kbd "C-x c o h")         #'helm-org-headlines))

;;; Global-map
;;
;;
(global-set-key (kbd "M-x")                          #'undefined)
(global-set-key (kbd "M-x")                          #'helm-M-x)
(global-set-key (kbd "C-x C-m")                      #'helm-M-x)
(global-set-key (kbd "C-x RET")                      #'helm-M-x)
(global-set-key (kbd "M-y")                          #'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f")                      #'helm-find-files)
(global-set-key (kbd "C-c <SPC>")                    #'helm-all-mark-rings)
(global-set-key (kbd "C-x r b")                      #'helm-filtered-bookmarks)
(global-set-key (kbd "C-h r")                        #'helm-info-emacs)
(global-set-key (kbd "C-:")                          #'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-,")                          #'helm-calcul-expression)
(global-set-key (kbd "C-h i")                        #'helm-info-at-point)
(global-set-key (kbd "C-x C-d")                      #'helm-browse-project)
(global-set-key (kbd "<f1>")                         #'helm-resume)
(global-set-key (kbd "C-h C-f")                      #'find-function)
(global-set-key (kbd "<f2>")                         #'helm-execute-kmacro)
(global-set-key [remap dabbrev-expand]               #'helm-dabbrev)
(global-set-key (kbd "M-/")                          #'helm-dabbrev)
(global-set-key [remap find-tag]                     #'helm-etags-select)
(global-set-key [remap xref-find-definitions]        #'helm-etags-select)
(global-set-key (kbd "C-h a")                        #'helm-apropos)
(global-set-key (kbd "C-h i")                        #'helm-info-emacs)
(global-set-key (kbd "C-h b")                        #'helm-descbinds)
(global-set-key (kbd "C-x b")                        #'helm-mini)
(global-set-key (kbd "C-x C-b")                      #'helm-buffers-list)
(global-set-key (kbd "C-x C-f")                      #'helm-find-files)
(global-set-key (kbd "C-x C-r")                      #'helm-recentf)
(global-set-key (kbd "C-x r l")                      #'helm-filtered-bookmarks)
(add-hook
 'after-init-hook (lambda ()
                    (global-set-key (kbd "M-y")      #'helm-show-kill-ring)
                    (global-set-key (kbd "C-x V F")  #'helm-do-grep-ag)))
(global-set-key (kbd "C-x c C-!")                    #'helm-calcul-expression)
(global-set-key (kbd "C-x c :")                      #'helm-eval-expression-with-eldoc)

;;; mode-specific-map
;;
(define-key mode-specific-map (kbd "I")              #'helm-imenu-in-all-buffers)

;; helm-swoop and grep-related commands

(global-set-key (kbd "M-i")     #'helm-swoop)
(global-set-key (kbd "s-i")     #'helm-multi-swoop)
(global-set-key (kbd "M-I")     #'helm-swoop-back-to-last-point)
(global-set-key (kbd "M-g o")   #'helm-swoop)
(global-set-key (kbd "C-c M-i") #'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") #'helm-multi-swoop-all)

(global-set-key (kbd "M-g M-i") #'helm-multi-swoop)
(global-set-key (kbd "M-g M-I") #'helm-multi-swoop-all)

(global-set-key (kbd "M-g a")   #'helm-grepint-grep)
(global-set-key (kbd "M-g F")   #'helm-do-ag-this-file)
(global-set-key (kbd "M-g b")   #'helm-do-ag-buffers)
(global-set-key (kbd "M-g f")   #'helm-do-grep-ag)
(global-set-key (kbd "M-g g")   #'helm-git-grep)
(global-set-key (kbd "M-g p")   #'helm-do-ag-project-root)
(global-set-key (kbd "M-g z")   #'helm-do-zgrep)
(global-set-key (kbd "M-g G")   #'helm-google)
(global-set-key (kbd "M-g S")   #'helm-google-suggest)

(after-load 'helm-swoop
  (define-key helm-swoop-map (kbd "C-m")             #'helm-multi-swoop-current-mode-from-helm-swoop)
  (define-key helm-swoop-edit-map (kbd "C-k")        #'helm-swoop--edit-cancel)
  )



(dolist (source
         '(helm-source-buffers-list
           helm-source-recentf
           helm-source-bookmarks
           helm-source-file-cache
           helm-source-files-in-current-dir
           helm-source-locate
           helm-source-projectile-files-list
           helm-source-projectile-recentf-list
           helm-source-projectile-directories-and-dired-list
           helm-source-projectile-projects
           helm-source-moccur
           helm-source-locate
           helm-source-grep
           helm-source-occur
           helm-source-grep-ag
           ))
  (add-to-list 'helm-for-files-preferred-list source))

;; Use default-as-input in grep
(add-to-list 'helm-sources-using-default-as-input 'helm-source-grep)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-grep-ag)




(provide 'init-helm)
