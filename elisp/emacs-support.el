;;; emacs-support --- Miscellaneous setup for Emacs itself.

;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "~/gd/emacs/dictionary"))

;; Put customizations in a dot file.
(setq custom-file "~/gd/emacs/.emacs-custom.el")
(load custom-file 'noerror)

;; Initialize the ELPA (if supported).
(if (fboundp 'package-initialize) (package-initialize))

;; Turn on global line number mode and uses spaces instead of tabs by
;; default.
(global-linum-mode 1)
(setq-default indent-tabs-mode nil)

;; Discourage Emacs from using ellipsis too quickly and follow links
;; when visiting files automagically.
(setq auto-save-list-file-prefix "~/.emacs-session-saves/"
      backup-by-copying-when-mismatch t
      display-time-day-and-date t
      eval-expression-print-level 10   ; default 4
      eval-expression-print-length 50  ; default 12
      find-file-visit-truename t
      next-line-add-newlines nil
      ;;visible-bell t
      select-enable-clipboard t)


;; Provide a Trey Jackson function to silently dismiss a pending
;; minibuffer command when the mouse leaves the current buffer
;; (provided there is no nested commands going on).
(defun stop-using-minibuffer ()
  "Kill  the minibuffer."
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)


;;; Enable expression evaluation on the command line
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

(if (eq window-system 'x)
    (progn
      (setq baud-rate 1000000)  ; kill use of slow scroll
      (setq search-highlight t)))

;; Disable the scroll bar amd tool bar always.  Disable the menu bar
;; when not running on a Mac display.
(menu-bar-showhide-tool-bar-menu-customize-disable)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;;(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (and (fboundp 'menu-bar-mode) (not (eq window-system 'ns)))
    (menu-bar-mode -1)
  (menu-bar-mode t))

;; Uniquely name buffers.
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

;; Enable time and date display in the minibuffer.
(display-time)

(provide 'emacs-support)
;;; emacs-support.el ends here
