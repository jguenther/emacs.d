;; TODO: enhance ibuffer-fontification-alist
;;   See http://www.reddit.com/r/emacs/comments/21fjpn/fontifying_buffer_list_for_emacs_243/

(require-package 'fullframe)
(after-load 'ibuffer
  (fullframe ibuffer ibuffer-quit))

(require-package 'ibuffer-vc)
(require-package 'ibuffer-projectile)

(defvar tak/ibuffer-projectile-filter-active nil)

(defun ibuffer-set-up-preferred-filters ()
  (interactive)
  (setq tak/ibuffer-projectile-filter-active nil)
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process))
  )

(defun ibuffer-set-up-projectile-filters ()
  (interactive)
  (setq tak/ibuffer-projectile-filter-active t)
  (ibuffer-projectile-set-filter-groups)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic))
  )

(defun ibuffer-toggle-filters ()
  (interactive)
  (if tak/ibuffer-projectile-filter-active
      (ibuffer-set-up-preferred-filters)
    (ibuffer-set-up-projectile-filters))
  )

(after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "P") 'ibuffer-toggle-filters))

(add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)

(setq-default ibuffer-show-empty-filter-groups nil)


(after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size))))))


;; Explicitly require ibuffer-vc to get its column definitions, which
;; can't be autoloaded
(after-load 'ibuffer
  (require 'ibuffer-vc))

;; Modify the default ibuffer-formats (toggle with `)
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 25 25 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)
        ;; wider name column
        (mark modified read-only vc-status-mini " "
              (name 40 40 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))

(setq ibuffer-filter-group-name-face 'font-lock-doc-face)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-ibuffer)
