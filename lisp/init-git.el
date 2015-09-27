;; TODO: link commits from vc-log to magit-show-commit
;; TODO: smerge-mode
(require-package 'magit)
(require-package 'git-blame)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'git-messenger) ;; Though see also vc-annotate's "n" & "p" bindings
(require-package 'git-timemachine)

(require-package 'magit)
(setq-default
 magit-process-popup-time 10
 magit-diff-refine-hunk t
 magit-completing-read-function 'magit-ido-completing-read)

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


(when *is-a-mac*
  (after-load 'magit
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))




(global-set-key (kbd "C-x v f") 'vc-git-grep)
(global-set-key (kbd "C-x v g") 'magit-blame)



(require-package 'git-messenger)
(global-set-key (kbd "C-x v p") #'git-messenger:popup-message)



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

(add-hook 'magit-mode-hook #'endless/add-PR-fetch)



;; silver-searcher
(require-package 'ag)
(global-set-key (kbd "C-x v F") 'ag)



;; completing read over all files in repo
(require-package 'magit-find-file)
(define-key mode-specific-map (kbd "G") 'magit-find-file-completing-read)



(defun tak/insert-git-commit-prefix ()
  "Prefixes the default git commit message with a Jira issue number.

The issue number is parsed from the branch name."
  (let* ((branch (magit-get-current-branch ))
         (ticket-id (if (and branch (string-match "^\\([A-Z]+-[0-9]+\\)-" branch))
                        (match-string 1 branch)))
         (prefix (if ticket-id
                     (format "%s: " ticket-id))))
    (goto-char (point-min))
    (when prefix
      (insert prefix)
      (end-of-line))))

(after-load 'git-commit
  (add-hook 'git-commit-setup-hook 'tak/insert-git-commit-prefix t))


(after-load 'magit
  (define-key magit-revision-mode-map (kbd "M-<left>") 'magit-go-backward)
  (define-key magit-revision-mode-map (kbd "M-<right>") 'magit-go-forward)
  )


;; so git-wip-mode doesn't depend on running magit-status first
(require 'magit)



(provide 'init-git)
