;; adapted from Devel::PerlySense docs on CPAN

(setq-default
 ps/key-prefix (kbd "C-o")

 ;; TODO: use flycheck instead (flymake is outdated)
 ps/load-flymake nil
 ps/external-dir (shell-command-to-string "perly_sense external_dir")

 ;; If you have a Devel::CoverX::Covered database handy and want to display the
 ;; sub coverage in the source, set this to t
 ps/enable-test-coverage-visualization nil

 ;; Run calls to perly_sense as a prepared shell command. Experimental
 ;; optimization, please try it out.
 ps/use-prepare-shell-command t
 )

;; unbind sanityinc/open-line-with-reindent -- can't currently use local
;; cperl-mode binds
(global-unset-key (kbd "C-o"))

;; add PerlySense external emacs dir to load-path
(if (string-match "Devel.PerlySense.external" ps/external-dir)
    (progn
      (let ((ps-external-emacs-dir (expand-file-name "emacs" ps/external-dir)))
        (message "Adding PerlySense elisp directory %s to load-path..."
                 ps-external-emacs-dir)
        (add-to-list 'load-path ps-external-emacs-dir t)))
  (message "Could not identify PerlySense install dir. Is
           Devel::PerlySense installed properly? Does `perly_sense
           external_dir' give you a proper directory? (%s)"
           ps/external-dir))

(add-hook 'cperl-mode-hook
                                        ; load PerlySense if it hasn't already
                                        ; been loaded
          (unless (boundp 'ps/perl-module-at-point)
            (load "perly-sense")
            (global-set-key (kbd "C-o C-g") nil)
            (global-set-key (kbd "C-o M-g") 'ps/smart-go-to-at-point)))

(provide 'init-perlysense)
