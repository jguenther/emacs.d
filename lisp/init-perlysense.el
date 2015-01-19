;; *** PerlySense Config ***
;; adapted from Devel::PerlySense docs on CPAN

;; ** PerlySense **
;; The PerlySense prefix key (unset only if needed, like for \C-o)
(global-unset-key "\C-o")
(setq ps/key-prefix "\C-o")

;; ** Flymake **
;; Load flymake if t
;; Flymake must be installed.
;; It is included in Emacs 22
;;     (or http://flymake.sourceforge.net/, put flymake.el in your load-path)
(setq ps/load-flymake t)
;; Note: more flymake config below, after loading PerlySense

;; *** PerlySense load (don't touch) ***
(setq ps/external-dir (shell-command-to-string "perly_sense external_dir"))
(if (string-match "Devel.PerlySense.external" ps/external-dir)
    (progn
      (message
       "PerlySense elisp files  at (%s) according to perly_sense, loading..."
       ps/external-dir)
      (setq load-path (cons
                       (expand-file-name
                        (format "%s/%s" ps/external-dir "emacs")
                        ) load-path))
      (load "perly-sense"))
  (message "Could not identify PerlySense install dir.
    Is Devel::PerlySense installed properly?
    Does 'perly_sense external_dir' give you a proper directory? (%s)" ps/external-dir))

;; ** Code Coverage Visualization **
;; If you have a Devel::CoverX::Covered database handy and want to
;; display the sub coverage in the source, set this to t
(setq ps/enable-test-coverage-visualization nil)

;; ** Misc Config **

;; Run calls to perly_sense as a prepared shell command. Experimental
;; optimization, please try it out.
(setq ps/use-prepare-shell-command t)

;; rebind C-o C-g to C-o M-g
(define-key ps/class-mode-map (kbd "C-o C-g") nil)
(define-key ps/class-mode-map (kbd "C-o M-g") 'ps/class-goto-at-point)

;; *** PerlySense End ***

(provide 'init-perlysense)
