
(defun sanityinc/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))


(defvar sanityinc/require-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defun build-require-times (orig-function feature &rest args)
  "Note in `sanityinc/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        (apply orig-function feature args)
      (when (and (not already-loaded) (memq feature features))
        (let ((time (sanityinc/time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'sanityinc/require-times
                       (cons feature time)
                       t))))))
(advice-add #'require :around #'build-require-times)

(defun tak/print-require-times ()
  "Prints the require times stored in `sanityinc/require-times'."
  (interactive)
  (let ((list sanityinc/require-times)
        sorted-times)
    (while list
      (let ((package-time (car list)))
        (setq list (cdr list))
        (let ((package (car package-time))
              (time (cdr package-time)))
          (add-to-ordered-list 'sorted-times (list package time) time))))
    (setq sorted-times (reverse sorted-times))
    (setq tak/sorted-times sorted-times)
    ;;(setq sanityinc/require-times sorted-times)
    (while sorted-times
      (let ((package-time (car sorted-times)))
        (setq sorted-times (cdr sorted-times))
        (let ((package (car package-time))
              (time (car (cdr package-time))))
          (message "feature `%s': %d ms." package time))))))

(add-to-list 'load-path "~/code/benchmark-init-el")
(require 'benchmark-init-loaddefs)
(benchmark-init/activate)



(defun tak/run-elp-results-after-defun (orig-function &rest args)
  "Advice to run `elp-results' after the function
returns. ORIG-FUNCTION must be instrumented by
`elp-instrument-function' prior to calling it."
  (require 'elp)
  (apply orig-function args)
  (elp-results))

(defun tak/profile-package (package &optional function)
  "Profiles all defuns in PACKAGE. If FUNCTION is set, advises
FUNCTION to run `elp-results' after FUNCTION returns."
  (require 'elp)
  ;;(elp-restore-all)
  (elp-instrument-package package)
  ;; (elp-reset-all)
  (if function
      (advice-add function :around #'tak/run-elp-results-after-defun)))

(defun tak/profile-defun (func)
  (require 'elp)
  ;;(elp-restore-all)
  (elp-instrument-function function)
  ;; (elp-reset-all)
  (advice-add function :around #'tak/run-elp-results-after-defun))

(defun tak/reset-elp ()
  (interactive)
  (elp-restore-all)
  (elp-reset-all))


(require 'profiler)

(defun tak/wanted-profilers ()
  (interactive)
  (if tak/want-cpu-profiler-p
      (if tak/want-memory-profiler-p
          'cpu+mem
        'cpu)
    (if tak/want-memory-profiler-p
        'mem)))

(defun tak/profiler-start ()
  (interactive)
  (profiler-start (tak/wanted-profilers)))

(defun tak/profiler-report ()
  (interactive)
  (if (profiler-running-p)
      (profiler-report)
    (error "Profiler is not currently running.")))

(defvar tak/want-cpu-profiler-p t)
(defvar tak/want-memory-profiler-p t)

(with-eval-after-load 'discover
  (discover-add-context-menu
   :context-menu '(profiler
                   (description "Profiler and debugging")
                   (lisp-switches
                    ("-c" "Profile CPU" tak/want-cpu-profiler-p t nil)
                    ("-m" "Profile memory" tak/want-memory-profiler-p t nil))
                   (actions
                    ("Common"
                     ("r" "profiler report" tak/profiler-report)
                     ("e" "reset profiler" profiler-reset)
                     ("p" "start profiler" tak/profiler-start)
                     ("s" "stop profiler" profiler-stop))
                    ))
   :bind "C-x M-d"))

;; (dolist (package '(
;;                    "magit"
;;                    "ido"
;;                    ))
;;   (tak/profile-package package))





(provide 'init-benchmarking)
