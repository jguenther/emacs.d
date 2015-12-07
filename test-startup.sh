#!/bin/sh -e
if [ -n "$TRAVIS" ]; then
    # Make it look like this is ~/.emacs.d (needed for Emacs 24.3, at least)
    export HOME=$PWD/..
    ln -s emacs.d ../.emacs.d
fi
echo "Attempting startup..."
${EMACS:=emacs} -nw --batch \
                --eval "(let* ((debug-on-error t)
                               (url-show-status nil)
                               (user-emacs-directory default-directory)
                               (user-init-file (expand-file-name \"init.el\"))
                               (load-path (delq default-directory load-path))
                               (psession-object-to-save-alist nil))
                           (desktop-save-mode -1)
                           (load-file user-init-file)
                           (run-hooks (quote after-init-hook))
                           (dolist (fn '(psession-save-last-winconf
                                         psession--dump-some-buffers-to-list
                                         psession--dump-object-to-file-save-alist))
                             (setq kill-emacs-hook (delete fn kill-emacs-hook))))"
echo "Startup successful"
