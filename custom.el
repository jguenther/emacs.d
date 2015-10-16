;; -*-no-byte-compile: t; -*-
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-timeout 120)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/saves"))))
 '(blink-cursor-mode t)
 '(compilation-ask-about-save nil)
 '(cperl-close-paren-offset -2)
 '(cperl-comment-column 40)
 '(cperl-highlight-variables-indiscriminately t)
 '(cperl-indent-comment-at-column-0 t)
 '(cperl-indent-parens-as-block t)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(desktop-auto-save-timeout 600)
 '(desktop-lazy-idle-delay 2)
 '(desktop-not-loaded-hook (quote (desktop-save-mode-off)))
 '(desktop-restore-eager 20)
 '(desktop-restore-in-current-display t)
 '(desktop-restore-reuses-frames :keep)
 '(desktop-save (quote if-exists))
 '(electric-indent-mode t)
 '(electric-pair-mode nil)
 '(elpy-modules
   (quote
    (elpy-module-eldoc elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(emacs-lisp-docstring-fill-column 70)
 '(ess-default-style (quote OWN))
 '(ess-fancy-comments nil)
 '(ess-indent-with-fancy-comments nil)
 '(fill-column 79)
 '(flycheck-disabled-checkers
   (quote
    (emacs-lisp-checkdoc perl-perlcritic html-tidy python-flake8 python-pycompile)))
 '(flycheck-keymap-prefix "`")
 '(garbage-collection-messages t)
 '(gc-cons-threshold 100000000)
 '(git-commit-finish-query-functions nil)
 '(hl-line-inhibit-highlighting-for-modes (quote (custom-mode)))
 '(ido-auto-merge-delay-time 2)
 '(ido-create-new-buffer (quote always))
 '(ido-default-file-method (quote selected-window))
 '(ido-everywhere t)
 '(ido-ignore-buffers (quote ("\\` " "\\`*epc con ")))
 '(jedi:goto-definition-marker-ring-length 32)
 '(jedi:install-imenu t)
 '(jedi:install-python-jedi-dev-command
   (quote
    ("pip" "install" "--upgrade" "git+https://github.com/davidhalter/jedi.git@master#egg=jedi")))
 '(jedi:tooltip-method (quote (pos-tip)))
 '(jedi:use-shortcuts t)
 '(js-indent-level 4 t)
 '(js2-basic-offset 4 t)
 '(magit-push-always-verify t)
 '(magit-revert-buffers 2)
 '(magit-save-repository-buffers (quote dontask))
 '(make-backup-files t)
 '(mark-ring-max 32)
 '(matlab-indent-level 2)
 '(matlab-shell-command-switches (quote ("-nodesktop" "-nosplash")))
 '(menu-bar-mode t)
 '(message-confirm-send t)
 '(mouse-1-click-follows-link 250)
 '(mouse-scroll-delay 0.2)
 '(mouse-wheel-progressive-speed t)
 '(org-agenda-files
   (quote
    ("/Users/jguenther/org/vendasta.org" "/Users/jguenther/org/snippets.org" "/Users/jguenther/org/todo.org")))
 '(org-attach-git-annex-cutoff 1048576)
 '(org-attach-store-link-p (quote attached))
 '(org-default-notes-file "~/org/.notes")
 '(org-emphasis-alist
   (quote
    (("**" bold)
     ("*" italic)
     ("_" underline)
     ("=" org-verbatim verbatim)
     ("~" org-code verbatim))))
 '(org-enforce-todo-dependencies t)
 '(org-fontify-done-headline t)
 '(org-list-indent-offset 2)
 '(org-mouse-1-follows-link (quote double))
 '(org-pretty-entities t)
 '(org-pretty-entities-include-sub-superscripts nil)
 '(org-startup-indented t)
 '(org-use-sub-superscripts (quote {}))
 '(package-selected-packages
   (quote
    (ido-other-window yasnippet pyvenv highlight-indentation find-file-in-project swiper company shell-pop nose flappymacs pymacs emacs-for-python anaconda-mode anaconda popup-keys quelpa restclient iedit back-button visual-mark achievements magit-tramp diffview string-inflection git-gutter+ god-mode project-persist-drawer sr-speedbar flx-ido flx git-gutter magit-find-file eshell-did-you-mean org-bullets orgit yari yaml-mode yagist win-switch whole-line-or-region whitespace-cleanup-mode which-key wgrep-ag vc-darcs unfill undo-tree tidy textile-mode tagedit switch-window sql-indent smex smarty-mode skewer-less session second-sel scss-mode scratch sass-mode ruby-hash-syntax robe rinari regex-tool redshank realgud rainbow-mode rainbow-delimiters python-x python-docstring pydoc-info pydoc project-local-variables ppd-sr-speedbar pip-requirements php-mode paredit-everywhere paradox page-break-lines osx-location orglink org-repo-todo org-projectile org-pomodoro org-plus-contrib org-mac-link org-mac-iCal org-jira org-fstree org-cliplink ob-browser mwe-log-commands multiple-cursors move-dup mmm-mode mkdown menu-bar+ matlab-mode magit-svn magit-gh-pulls macrostep lua-mode lively less-css-mode ledger-mode json-mode js-comint jquery-doc jira-markup-mode jinja2-mode jedi-direx isearch+ ipretty info+ indent-guide ido-ubiquitous ido-describe-bindings ibuffer-vc ibuffer-projectile htmlize hl-sexp hl-line+ hippie-expand-slime highlight-symbol highlight-quoted highlight-escape-sequences help-mode+ help-fns+ help+ hayoo guide-key goto-last-change gnuplot gitignore-mode github-clone github-browse-file gitconfig-mode git-wip-timemachine git-timemachine git-messenger git-gutter-fringe git-blame ghci-completion fullframe free-keys flycheck-package flycheck-ledger flycheck-hdevtools flycheck-haskell flx-isearch fill-column-indicator expand-region exec-path-from-shell ess erlang elpy elisp-slime-nav dsvn discover-my-major discover dired-toggle-sudo dired-sort-menu dired-sort dired-imenu dired-filter dired+ diminish diff-hl default-text-scale dash-at-point darcsum cursor-chg csv-nav csv-mode css-eldoc crontab-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode cl-lib-highlight bug-reference-github browse-kill-ring+ bash-completion avy auto-compile anzu aggressive-indent ag ace-jump-mode ac-slime ac-js2 ac-inf-ruby ac-haskell-process)))
 '(projectile-global-mode t)
 '(python-fill-docstring-style (quote django))
 '(python-indent-guess-indent-offset-verbose nil)
 '(safe-local-variable-values
   (quote
    ((checkdoc-minor-mode . 1)
     (python-shell-extra-pythonpaths "/Users/jguenther/Projects/VA-vobject/" "/Users/jguenther/Projects/VA-vobject/src/" "/Users/jguenther/Projects/VA-vobject/src/lib/" "/Users/jguenther/Projects/VA-vobject/src/vobject" "/Users/jguenther/Projects/VA-vobject/src/velastic" "/Users/jguenther/Projects/VA-vobject/src/vdatastore" "/Users/jguenther/Projects/VA-vobject/src/util" "/Users/jguenther/Projects/VA-vobject/tests/" "/Users/jguenther/Projects/VA-vobject/tests/lib/" "/Users/jguenther/Projects/VA-vobject/tools/" "/usr/local/google_appengine/" "/usr/local/google_appengine/lib/webob-1.2.3/" "/usr/local/google_appengine/lib/webapp2-2.5.2/" "/usr/local/google_appengine/lib/jinja2-2.6/" "/usr/local/google_appengine/lib/lxml-2.3/")
     (python-shell-extra-pythonpaths "/Users/jguenther/Projects/VA-vobject/" "/Users/jguenther/Projects/VA-vobject/src/" "/Users/jguenther/Projects/VA-vobject/src/lib/" "/Users/jguenther/Projects/VA-vobject/src/vobject" "/Users/jguenther/Projects/VA-vobject/src/velastic" "/Users/jguenther/Projects/VA-vobject/src/vdatastore" "/Users/jguenther/Projects/VA-vobject/tests/" "/Users/jguenther/Projects/VA-vobject/tests/lib/" "/Users/jguenther/Projects/VA-vobject/tools/" "/usr/local/google_appengine/" "/usr/local/google_appengine/lib/webob-1.2.3/" "/usr/local/google_appengine/lib/webapp2-2.5.2/" "/usr/local/google_appengine/lib/jinja2-2.6/" "/usr/local/google_appengine/lib/lxml-2.3/")
     (python-shell-extra-pythonpaths "/Users/jguenther/Projects/VA-vobject/" "/Users/jguenther/Projects/VA-vobject/src/" "/Users/jguenther/Projects/VA-vobject/src/lib/" "/Users/jguenther/Projects/VA-vobject/test/" "/Users/jguenther/Projects/VA-vobject/test/lib/" "/Users/jguenther/Projects/VA-vobject/tools/" "/usr/local/google_appengine/" "/usr/local/google_appengine/lib/webob-1.2.3/" "/usr/local/google_appengine/lib/webapp2-2.5.2/" "/usr/local/google_appengine/lib/jinja2-2.6/" "/usr/local/google_appengine/lib/lxml-2.3/")
     (python-shell-extra-pythonpaths "/Users/jguenther/Projects/harvester-service" "/Users/jguenther/Projects/harvester-service/src" "/Users/jguenther/Projects/harvester-service/src/lib" "/Users/jguenther/Projects/harvester-service/test" "/Users/jguenther/Projects/harvester-service/test/lib" "/Users/jguenther/Projects/harvester-service/tools" "/usr/local/google_appengine/" "/usr/local/google_appengine/lib/webob-1.1.1" "/usr/local/google_appengine/lib/webapp2-2.5.2" "/usr/local/google_appengine/lib/jinja2-2.6")
     (python-shell-extra-pythonpaths "/Users/jguenther/Projects/CS/" "/Users/jguenther/Projects/CS/src/" "/Users/jguenther/Projects/CS/src/lib/" "/Users/jguenther/Projects/CS/test/" "/Users/jguenther/Projects/CS/test/lib/" "/Users/jguenther/Projects/CS/tools/" "/usr/local/google_appengine/" "/usr/local/google_appengine/lib/webob-1.2.3/" "/usr/local/google_appengine/lib/webapp2-2.5.2/" "/usr/local/google_appengine/lib/jinja2-2.6/" "/usr/local/google_appengine/lib/lxml-2.3/")
     (flycheck-pylintrc . "/Users/jguenther/Projects/CS/tools/vbuild/pylintrc")
     (python-shell-extra-pythonpaths "/Users/jguenther/Projects/VBC" "/Users/jguenther/Projects/VBC/src" "/Users/jguenther/Projects/VBC/src/lib" "/Users/jguenther/Projects/VBC/tools" "/Users/jguenther/Projects/VBC/tools/coverage-3.5.1" "/Users/jguenther/Projects/VBC/tools/minimock" "/usr/local/google_appengine" "/usr/local/google_appengine/lib/webob-1.1.1" "/usr/local/google_appengine/lib/webapp2-2.5.1" "/usr/local/google_appengine/lib/jinja2-2.6" "/usr/local/google_appengine/lib/simplejson" "/usr/local/google_appengine/lib/yaml/lib" "/usr/local/google_appengine/lib/fancy_urllib" "/usr/local/google_appengine/lib/protorpc-1.0")
     (python-shell-extra-pythonpaths "/Users/jguenther/Projects/CS/" "/Users/jguenther/Projects/CS/src/" "/Users/jguenther/Projects/CS/src/lib/" "/Users/jguenther/Projects/CS/test/lib/" "/Users/jguenther/Projects/CS/tools/" "/usr/local/google_appengine/" "/usr/local/google_appengine/lib/webob-1.2.3/" "/usr/local/google_appengine/lib/webapp2-2.5.2/" "/usr/local/google_appengine/lib/jinja2-2.6/" "/usr/local/google_appengine/lib/lxml-2.3/")
     (flycheck-python-pylint-executable . "/Users/jguenther/Projects/CS/tools/pylint/lint.py")
     (flycheck-python-pylint-executable . "/Users/jguenther/Projects/harvester-service/tools/pylint/lint.py")
     (flycheck-python-pylint-executable . "/Users/jguenther/Projects/VBC/tools/pylint/lint.py")
     (flycheck-pylintrc . "/Users/jguenther/Projects/VBC/tools/vbuild/pylintrc")
     (whitespace-style face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-mark tab-mark newline-mark)
     (flycheck-pylintrc "/Users/jguenther/Projects/harvester-service/.pylintrc")
     (flycheck-pylintrc
      (expand-file-name "~/Projects/VBC/test/.pylintrc_test"))
     (flycheck-pylintrc "/Users/jguenther/Projects/harvester-service/tools/vbuild/pylintrc")
     (flycheck-pylintrc "/Users/jguenther/Projects/harvester-service/test/.pylintrc_test")
     (flycheck-pylintrc "/Users/jguenther/Projects/CS/tools/vbuild/pylintrc")
     (flycheck-pylintrc "/Users/jguenther/Projects/VBC/tools/vbuild/pylintrc")
     (flycheck-pylintrc "/Users/jguenther/Projects/CS/test/.pylintrc_test")
     (flycheck-pylintrc "/Users/jguenther/Projects/VBC/test/.pylintrc_test")
     (no-byte-compile t)
     (ruby-compilation-executable . "ruby")
     (ruby-compilation-executable . "ruby1.8")
     (ruby-compilation-executable . "ruby1.9")
     (ruby-compilation-executable . "rbx")
     (ruby-compilation-executable . "jruby"))))
 '(scroll-bar-mode (quote right))
 '(scroll-preserve-screen-position nil)
 '(send-mail-function (quote sendmail-send-it))
 '(session-use-package t nil (session))
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(show-trailing-whitespace nil)
 '(speedbar-default-position (quote left))
 '(speedbar-frame-parameters
   (quote
    ((minibuffer)
     (width . 20)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0)
     (vertical-scroll-bars))))
 '(standard-indent 2)
 '(tramp-connection-timeout 30)
 '(vc-follow-symlinks t)
 '(visible-bell nil)
 '(whitespace-cleanup-mode-ignore-modes
   (quote
    (markdown-mode special-mode view-mode comint-mode cider-repl-mode haskell-interactive-mode org-mode)))
 '(yas-also-auto-indent-first-line t)
 '(yas-expand-only-for-last-commands (quote (self-insert-command))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-fine-diff-B ((t (:background "#22aa22" :foreground "black"))))
 '(realgud-backtrace-number ((t (:inherit highlight-symbol-face :weight bold))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "#22aa22" :foreground "black")))))
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
