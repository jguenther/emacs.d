;;; discovery modes
;; discover, popup-keys, etc.

(require-package 'discover)

(quelpa '(discover-my-major :fetcher file :path "~/code/discover-my-major/"))

(require-package 'discover-my-major)
(require 'discover-my-major)

(global-set-key (kbd "C-h M-m") 'discover-my-major)
(global-set-key (kbd "C-h C-M-m") 'discover-my-mode)
(global-set-key (kbd "C-h M-M") 'discover-my-mode)

(require 'discover)
(global-discover-mode 1)

(defun tak/unhighlight-symbol-at-point ()
  (interactive)
  (highlight-symbol-remove-symbol (highlight-symbol-get-symbol)))

(discover-add-context-menu
 :context-menu '(isearch
                 (description "Isearch, occur and highlighting")
                 (lisp-switches
                  ("-i" "Case should fold search" case-fold-search t nil))
                 (lisp-arguments
                  ("=l" "context lines to show (occur)"
                   "list-matching-lines-default-context-lines"
                   (lambda (dummy) (interactive) (read-number "Number of context lines: "))))
                 (actions
                  ("isearch"
                   ("M-s" "isearch forward symbol" isearch-forward-symbol)
                   ("_" "isearch forward symbol" isearch-forward-symbol)
                   ("w" "isearch forward word" isearch-forward-word))
                  ("Occur"
                   ("o" "occur" helm-occur)
                   ("o" "helm-swoop" helm-swoop)
                   ("C-/" "helm-swoop (all)" helm-multi-swoop-all)
                   ("C-o" "helm-swoop (org)" helm-multi-swoop-org)
                   ("/" "helm-swoop (multi)" helm-multi-swoop)
                   ("m" "helm-swoop (current-mode)" helm-multi-swoop-current-mode)
                   ("M-m" "helm-swoop (mode)" helm-multi-swoop-by-mode)
                   )

                  ("Grep"
                   ("s" "ag project" #'helm-projectile-ag)
                   ("F" "ag" #'helm-do-ag)
                   ("g" "grep-ag" #'helm-do-grep-ag)
                   ("G" "git-grep" #'helm-grep-do-git-grep))
                  ("More"
                                        ; local change
                   ("a" "search and replace in bookmark targets" makey-key-mode-popup-isearch-bookmarks)
                   ("M-w" "search for words using eww" eww-search-words)
                   ("h" "highlighters ..." makey-key-mode-popup-isearch-highlight))))
 :bind "M-s")

(discover-add-context-menu
 :context-menu '(isearch-highlight
                 (actions
                  ("Highlight"
                                        ; local changes
                   ("." "highlight symbol at point" highlight-symbol-at-point)
                   ("," "unhighlight symbol at point" tak/unhighlight-symbol-at-point)

                   ("l" "highlight lines matching regexp" highlight-lines-matching-regexp)
                   ("p" "highlight phrase" highlight-phrase)
                   ("r" "highlight regexp" highlight-regexp)
                   ("u" "unhighlight regexp" unhighlight-regexp))

                  ("Store"
                   ("f" "hi lock find patterns" hi-lock-find-patterns)
                   ("w" "hi lock write interactive patterns" hi-lock-write-interactive-patterns)))))

(discover-add-context-menu
 :context-menu '(isearch-bookmarks
                 (lisp-switches
                  ("-i" "Case should fold search" case-fold-search t nil)
                  ("-a" "Search in all bookmarks" current-prefix-arg 4 nil)
                  ;; ("-s" "Display only bookmarks that are icycles search hits"
                  ;;  bmkp-bmenu-show-only-icicles-search-hits-bookmarks )
                  )
                 (actions
                  ("Bookmarks"
                                        ; isearch
                   ("C-s" "regexp-isearch marked bookmarks"
                    bmkp-bmenu-isearch-marked-bookmarks-regexp)
                   ("C-M-s" "isearch marked bookmarks"
                    bmkp-bmenu-isearch-marked-bookmarks)
                   ("M-s" "regexp-search marked bookmarks"
                    bmkp-bmenu-search-marked-bookmarks-regexp)
                                        ; query-replace
                   ("M-%" "query-replace-regexp marked bookmarks"
                    bmkp-bmenu-query-replace-marked-bookmarks-regexp)
                   ("C-M-%" "query-replace-regexp marked bookmarks"
                    bmkp-bmenu-query-replace-marked-bookmarks-regexp))
                  )))



;; popup-keys

(quelpa '(popup-keys :fetcher file :path "~/code/popup-keys/"))

(require 'popup-keys)
(require 'popup-keys-examples)

(global-set-key (kbd "C-x M-/") 'popup-keys:run-findtool)

(eval-after-load "vc-hooks"
  '(progn
     ;; move original keymap
     (define-key ctl-x-map "V" 'vc-prefix-map)
     ;; run popup on original key
     (define-key ctl-x-map "v" 'popup-keys:run-vc)
     ))
(global-set-key (kbd "C-x v") 'popup-keys:run-vc)

(define-key mode-specific-map (kbd "p") 'popup-keys:run-projectile)

(global-set-key (kbd "C-x C-k")   'popup-keys:run-kmacro)
(global-set-key (kbd "C-x C-S-k") 'kmacro-keymap)

(global-set-key (kbd "C-x r") 'popup-keys:run-registers)
(global-set-key (kbd "C-x R") ctl-x-r-map)
;; undo-tree annoyingly binds to the C-x r prefix and overrides the above.
(eval-after-load "undo-tree"
  '(define-key undo-tree-map (kbd "C-x r") nil))

(add-hook 'dired-mode-hook
          (lambda ()
            (require 'dired-x)
            (define-key dired-mode-map (kbd "?") 'popup-keys:run-dired)
            (define-key dired-mode-map (kbd "%") 'popup-keys:run-dired-regexp)
            (define-key dired-mode-map (kbd "*") 'popup-keys:run-dired-mark)))

(eval-after-load "ibuffer"
  '(progn
     (define-key ibuffer-mode-map (kbd "?") 'popup-keys:run-ibuffer)
     (define-key ibuffer-mode-map (kbd "*") 'popup-keys:run-ibuffer-mark)
     (define-key ibuffer-mode-map (kbd "/") 'popup-keys:run-ibuffer-filter)
     ))

(eval-after-load "undo-tree"
  '(define-key undo-tree-visualizer-mode-map (kbd "?") 'popup-keys:run-undo-tree))

(after-load 'org
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key org-mode-map (kbd "M-S-s") 'popup-keys:run-org-speed))))

(provide 'init-discovery)
