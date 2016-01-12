;; TODO: link commits from vc-log to magit-show-commit
;; TODO: smerge-mode
(require-package 'magit)
(require-package 'git-blame)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'git-messenger) ;; Though see also vc-annotate's "n" & "p" bindings
(require-package 'git-timemachine)

(setq-default
 magit-process-popup-time 10
 magit-diff-refine-hunk t
 magit-completing-read-function 'helm--completing-read-default)

;; Hint: customize `magit-repo-dirs' so that you can use C-u M-F12 to
;; quickly open magit on any one of your projects.
(global-set-key [(meta f12)] 'magit-status)

(after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up)
  (define-key magit-status-mode-map (kbd "<backtab>") 'magit-section-cycle-global)

  (add-hook 'magit-popup-mode-hook 'sanityinc/no-trailing-whitespace))

(require-package 'fullframe)
;; (after-load 'magit
;;   (fullframe magit-status magit-mode-quit-window))

(when (maybe-require-package 'git-commit)
  (add-hook 'git-commit-mode-hook 'goto-address-mode))



(global-set-key (kbd "C-x v f") #'vc-git-grep)
(global-set-key (kbd "C-x v F") #'helm-do-ag)
(global-set-key (kbd "C-x v g") #'magit-blame)



(require-package 'git-messenger)
(global-set-key (kbd "C-x v p") #'git-messenger:popup-message)



(global-set-key (kbd "C-x v t") #'git-timemachine)
(global-set-key (kbd "C-x v T") #'git-wip-timemachine)
(global-set-key (kbd "C-x v w") #'git-wip-timemachine)



;; magit-filenotify
(unless (or (eq system-type 'windows-nt) *is-a-mac*)
                                        ; magit-filenotify is slow on windows
                                        ; with msysgit, and file notifications
                                        ; don't work at all on MacOS
  (require-package 'magit-filenotify)
  (add-hook 'magit-status-mode-hook 'magit-filenotify-mode)
  (after-load 'magit-filenotify
    (diminish 'magit-filenotify-mode)
    (dolist (re '(                      ; ignore ExtUtils::CBuilder-related
                                        ; temporary files
                  "\\`compilet-.+\\.\\(?:cc?\\|cpp\\|cxx\\|h\\|hpp\\|hxx\\|s?o\\).*"
                                        ; ignore emacs auto-save files
                  "\\`#\\.+#\\'"
                  ))
      (add-to-list 'magit-filenotify-ignored re))))



;; in case color.ui=always
(after-load 'magit
  (setq-default magit-git-standard-options
                (append magit-git-standard-options (list "-c" "color.ui=true"))))




;; from http://endlessparentheses.com/automatically-configure-magit-to-access-github-prs.html
(defun endless/add-PR-fetch ()
  "If refs/pull is not defined on a GH repo, define it."
  (let ((fetch-address
         "+refs/pull/*/head:refs/pull/origin/*")
        (magit-remote-config
         (magit-get-all "remote" "origin" "fetch")))
    (unless (or (null magit-remote-config)
                (member fetch-address magit-remote-config))
      (when (string-match
             "github\\.com" (magit-get "remote" "origin" "url"))
        (magit-git-string
         "config" "--add" "remote.origin.fetch"
         fetch-address)))
    ))

(after-load 'magit
  (add-hook 'magit-mode-hook #'endless/add-PR-fetch)
  )

;; gh
(after-load 'gh
  ;; don't change '-' to '.' (via gh-api-enterprise-username-filter)
  (setq gh-api-username-filter nil))



;; completing read over all files in repo
(require-package 'magit-find-file)
(define-key mode-specific-map (kbd "G") 'magit-find-file-completing-read)



;; TODO fix bug with interactive rebase:

;; (buffer-modified-p) is nil unless I type something in COMMIT_EDITMSG, even
;; if the text of that buffer was changed by this function in
;; git-commit-setup-hook
;;
;; use different hook?
;; set a var if prefix inserted, then set-buffer-modified-p in pre-finish-hook?
;;
;; possibly something in with-editor.el?
;; with-editor-pre-finish-hook?
;; [[file:~/.emacs.d/elpa/with-editor-20150921.1018/with-editor.el::(put%20'with-editor-pre-finish-hook%20'permanent-local%20t)]]

(defun tak/insert-git-commit-prefix ()
  "Prefixes the default git commit message with a Jira issue number,
  if it isn't already in the default commit message (e.g. due to commit
  --amend)

The issue number is parsed from the branch name."
  (let* ((branch (magit-get-current-branch))
         (interactive-rebase (file-directory-p (magit-git-dir "rebase-merge")))
         (rebase-dir  (if interactive-rebase "rebase-merge/"))
         (head-name (if rebase-dir
                        (-> (concat rebase-dir "head-name") magit-git-dir magit-file-line)
                      branch))
         (head-name (or (magit-rev-name head-name "refs/heads/*") head-name))
         (ticket-id (if (and head-name
                             (let ((case-fold-search nil))
                               (string-match "^\\([A-Z]+-[0-9]+\\)" head-name)))
                        (match-string 1 head-name)))
         (len (length ticket-id))
         (prefix (if ticket-id
                     (format "%s: " ticket-id)))
         (prefix-len (length prefix))
         (prefix (save-excursion
                   (goto-char (point-min))
                   (if (and prefix
                            (not (re-search-forward
                                  (concat "\\`" (regexp-quote prefix)) (+ prefix-len 1) t)))
                       prefix))))
    (when prefix
      (goto-char (point-min))
      (insert prefix)
      (end-of-line))))

(after-load 'git-commit
  (add-hook 'git-commit-setup-hook 'tak/insert-git-commit-prefix t))



(after-load 'magit
  (define-key magit-revision-mode-map (kbd "M-<left>") 'magit-go-backward)
  (define-key magit-revision-mode-map (kbd "M-<right>") 'magit-go-forward)
  (define-key magit-status-mode-map (kbd "C-x g") 'magit-refresh)
  (define-key ctl-x-map (kbd "g") #'magit-status)
  )



;; speedup
(after-load 'magit
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags))



;; so git-wip-mode doesn't depend on running magit-status first
(require 'magit)



;; customize magit popups
;;
;; c.f. https://github.com/magit/magit/wiki/Additional-proposed-infix-arguments-and-suffix-commands
;;
(magit-define-popup-action 'magit-log-popup
  ?w "Wip" 'magit-wip-log-current)

(defun tak/magit-diff-current (arg)
  "Call `magit-diff` on current buffer."
  (interactive "P")
  (let* ((file (buffer-file-name))
         (magit-diff-arguments (magit-popup-import-file-args
                                (default-value 'magit-diff-arguments)
                                (list file)))
         )
    (magit-invoke-popup 'magit-diff-popup nil arg)
    ))

(magit-define-popup-action 'magit-file-popup
  ?d "Diff" 'tak/magit-diff-current
  )

(magit-define-popup-switch 'magit-log-popup
  ?m "Omit merge commits" "--no-merges")






(provide 'init-git)
