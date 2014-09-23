;; -*-no-byte-compile: t; -*-
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#657b83" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#fdf6e3"))
 '(blink-cursor-mode nil)
 '(cperl-close-paren-offset -2)
 '(cperl-comment-column 40)
 '(cperl-highlight-variables-indiscriminately t)
 '(cperl-indent-comment-at-column-0 t)
 '(cperl-indent-parens-as-block t)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(desktop-auto-save-timeout 600)
 '(desktop-restore-in-current-display t)
 '(desktop-save (quote if-exists))
 '(desktop-save-mode t)
 '(electric-indent-mode t)
 '(electric-pair-mode nil)
 '(ess-fancy-comments nil)
 '(fci-rule-color "#eee8d5")
 '(fill-column 79)
 '(flycheck-checkers
   (quote
    (ledger haskell-hdevtools ada-gnat asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint d-dmd elixir emacs-lisp emacs-lisp-checkdoc erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck haml handlebars haskell-ghc haskell-hlint html-tidy javascript-jshint javascript-eslint javascript-gjslint json-jsonlint less lua make perl perl-perlcritic php php-phpmd php-phpcs puppet-parser puppet-lint python-flake8 racket rst rst-sphinx ruby-rubocop ruby-rubylint ruby ruby-jruby rust sass scala scala-scalastyle scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim tex-chktex tex-lacheck texinfo verilog-verilator xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby)))
 '(flycheck-disabled-checkers (quote (emacs-lisp-checkdoc perl-perlcritic)))
 '(flycheck-idle-change-delay 1.5)
 '(flycheck-perlcritic-verbosity 4)
 '(git-gutter-fr+-side (quote right-fringe))
 '(global-magit-wip-save-mode t)
 '(ido-create-new-buffer (quote always))
 '(ido-everywhere t)
 '(magit-repo-dirs (quote ("~/.emacs.d" "~/code/PRIPchip")))
 '(make-backup-files t)
 '(mouse-wheel-progressive-speed nil)
 '(session-use-package t nil (session))
 '(sh-basic-offset 2)
 '(show-trailing-whitespace nil)
 '(tabbar-buffer-home-button (quote (("[+]") "[-]")))
 '(tabbar-cycle-scope (quote groups))
 '(tabbar-home-button (quote (("[o]") "[x]")))
 '(tabbar-mwheel-mode t nil (tabbar))
 '(tabbar-ruler-swap-faces t)
 '(tabbar-scroll-left-button (quote ((" <") " =")))
 '(tabbar-scroll-right-button (quote ((" >") " =")))
 '(tabbar-separator (quote ("|")))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil)
 '(win-switch-idle-time 1.5))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-array-face ((t (:foreground "yellow" :weight bold))))
 '(cperl-hash-face ((t (:foreground "Red" :slant italic :weight bold))))
 '(tabbar-button ((t (:inherit tabbar-default :box (:line-width 2 :color "white" :style released-button)))))
 '(tabbar-button-highlight ((t (:inherit tabbar-highlight))))
 '(tabbar-default ((t (:inherit variable-pitch :background "gray40" :foreground "light gray" :box (:line-width 1 :color "white" :style released-button)))))
 '(tabbar-highlight ((t (:foreground "gray5" :box (:line-width 2 :color "grey75" :style released-button) :underline t))))
 '(tabbar-modified ((t (:inherit tabbar-default :foreground "green"))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "gray13" :foreground "white" :box (:line-width 1 :color "white" :style pressed-button)))))
 '(tabbar-separator ((t (:inherit tabbar-default :weight semi-bold :height 1.2))))
 '(tabbar-unselected ((t (:inherit tabbar-button :box (:line-width 1 :color "white" :style released-button))))))
