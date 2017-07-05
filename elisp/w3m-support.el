;;; Set up to use w3m mode as setup on a Fedora system.

;; Append the w3m directory to the load path.
(setq load-path
      (append (list (expand-file-name "/usr/share/emacs/site-lisp/w3m")) load-path))

(require 'w3m-load)

