;;; discovery modes
;; discover, popup-keys, etc.

(require-package 'discover-my-major)
(require 'discover-my-major)

(global-set-key (kbd "C-h M-m") 'discover-my-major)
(global-set-key (kbd "C-h C-M-m") 'discover-my-mode)
(global-set-key (kbd "C-h M-M") 'discover-my-mode)

;; popup-keys

(quelpa `(popup-keys
          :fetcher file
          :path ,(expand-file-name "popup-keys" "~/code")))

(require 'popup-keys)
(require 'popup-keys-examples)

(global-set-key (kbd "C-x M-/") 'popup-keys:run-findtool)

(eval-after-load "vc-hooks"
  '(progn
     ;; move original keymap
     (define-key ctl-x-map "V" 'vc-prefix-map)
     ;; run popup on original key
     (define-key ctl-x-map "v" 'popup-keys:run-vc)
     (popup-keys:add-thing 'popup-keys:run-vc
                           'action
                           "p" "helm projectile ag" 'helm-projectile-ag
                           )
     (popup-keys:add-thing 'popup-keys:run-vc
                           'action
                           "F" "helm grep-ag" 'helm-do-grep-ag
                           )
     ))
(global-set-key (kbd "C-x v") 'popup-keys:run-vc)

(global-set-key (kbd "C-c P") 'popup-keys:run-projectile)

(global-set-key (kbd "C-x C-k")   'popup-keys:run-kmacro)
(global-set-key (kbd "C-x C-S-k") 'kmacro-keymap)

(global-set-key (kbd "C-x r") 'popup-keys:run-registers)
(global-set-key (kbd "C-x r") 'popup-keys:run-registers)

(after-load 'undo-tree
  (define-key undo-tree-map (kbd "C-x r") nil))

;; undo-tree annoyingly binds to the C-x r prefix and overrides this
(global-set-key (kbd "C-x R") ctl-x-r-map)

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

(global-set-key (kbd "C-x D") 'popup-keys:run-debug-commands)

(defun tak/unhighlight-symbol-at-point ()
  (interactive)
  (highlight-symbol-remove-symbol (highlight-symbol-get-symbol)))

(popup-keys:new
 'popup-keys:run-isearch
 :buf-name "*isearch*"
 :more-help (popup-keys:info-node "(emacs) Incremental search")
 :actions `(
            ;; (lisp-switches
            ;;  ("-i" "Case should fold search" case-fold-search t nil))
            ;; (lisp-arguments
            ;;  ("=l" "context lines to show (occur)"
            ;;   "list-matching-lines-default-context-lines"
            ;;   (lambda (dummy) (interactive) (read-number "Number of context lines: "))))
            ("M-s" "isearch forward symbol" isearch-forward-symbol)
            ("_" "isearch forward symbol" isearch-forward-symbol)
            ("w" "isearch forward word" isearch-forward-word)
            ("o" "helm-swoop" helm-swoop)
            ("/" "helm-swoop (multi)" helm-multi-swoop)
            ("m" "helm-swoop (current-mode)" helm-multi-swoop-current-mode)
            ("C-/" "helm-swoop (all)" helm-multi-swoop-all)
            ("C-o" "helm-swoop (org)" helm-multi-swoop-org)
            ("M-m" "helm-swoop (mode)" helm-multi-swoop-by-mode)
            ("s" "ag project" #'helm-projectile-ag)
            ("F" "ag" #'helm-do-ag)
            ("g" "grep-ag" #'helm-do-grep-ag)
            ("G" "git-grep" #'helm-grep-do-git-grep)
            ("a" "isearch bookmarks" popup-keys:run-isearch-bookmarks)
            ("M-w" "search for words using eww" eww-search-words)
            ("h" "highlighters ..." popup-keys:run-isearch-highlight)
            ))
(global-set-key (kbd "M-s") #'popup-keys:run-isearch)

(popup-keys:new
 'popup-keys:run-isearch-highlight
 :buf-name "*isearch highlight*"
 :actions `(
            ("." "highlight symbol at point" highlight-symbol-at-point)
            ("," "unhighlight symbol at point" tak/unhighlight-symbol-at-point)

            ("l" "highlight lines matching regexp" highlight-lines-matching-regexp)
            ("p" "highlight phrase" highlight-phrase)
            ("r" "highlight regexp" highlight-regexp)
            ("u" "unhighlight regexp" unhighlight-regexp)
            ("f" "hi lock find patterns" hi-lock-find-patterns)
            ("w" "hi lock write interactive patterns" hi-lock-write-interactive-patterns)))

(popup-keys:new
 'popup-keys:run-isearch-bookmarks
 :buf-name "*isearch bookmarks*"

 ;; (lisp-switches
 ;;  ("-i" "Case should fold search" case-fold-search t nil)
 ;;  ("-a" "Search in all bookmarks" current-prefix-arg 4 nil)
 ;;  ;; ("-s" "Display only bookmarks that are icycles search hits"
 ;;  ;;  bmkp-bmenu-show-only-icicles-search-hits-bookmarks )
 ;;  )

 :actions `(
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
             bmkp-bmenu-query-replace-marked-bookmarks-regexp)
            ))




(provide 'init-discovery)
