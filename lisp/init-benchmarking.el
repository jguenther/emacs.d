(defun sanityinc/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))


(defvar sanityinc/require-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defadvice require
  (around build-require-times (feature &optional filename noerror) activate)
  "Note in `sanityinc/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (add-to-list 'sanityinc/require-times
                     (cons feature
                           (sanityinc/time-subtract-millis (current-time)
                                                           require-start-time))
                     t)))))

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

(provide 'init-benchmarking)
