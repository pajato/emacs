;;; package --- Support for customized keybindings
;;;
;;; Commentary:
;;;
;;; See gd/org/mac-keyboard.org

;; Use the very nice iy-go-to-char feature as seen in Emacs Rocks!
;;;;(require 'iy-go-to-char)
;;;;(global-set-key (kbd "M-m") 'iy-go-to-char)

;; Provide support for tracking key frequency.
;;(require 'keyfreq)
;;(keyfreq-mode 1)
;;(keyfreq-autosave-mode 1)

(require 'org)

;;; Code:
(setq org-confirm-elisp-link-function nil)

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;; Support for ergonomic extensions, ala ErgoEmacs.
(defun 2-windows-vertical-to-horizontal ()
  "Switch to side by side windows."
  (interactive)
  (let ((buffers (mapcar 'window-buffer (window-list))))
    (when (= 2 (length buffers))
      (delete-other-windows)
      (set-window-buffer (split-window-horizontally) (cadr buffers)))))

;; Support for ergonomic extensions, ala ErgoEmacs.
(defun 2-windows-horizontal-to-vertical ()
  "Switch to top/bottom windows."
  (interactive)
  (let ((buffers (mapcar 'window-buffer (window-list))))
    (when (= 2 (length buffers))
      (delete-other-windows)
      (set-window-buffer (split-window-vertically) (cadr buffers)))))

;; Provide a function to goto a particular column on a particular
;; line.  Takes a string spec of the form "#[:#]' where the first # is
;; the line number and the second # is the column number.
(defun goto-position (position)
  "Goto POSITION at line:column."
  (interactive "sPosition: ")
  (let ((re "\\([0-9]*\\):?\\([0-9]*\\)")
        line column)
    (if (string-match re position)
        (setq line (match-string 1 position)
              column (match-string 2 position))
      (error "Bad line!"))
    (forward-line (string-to-number line))
    (move-to-column (string-to-number column))))

(defun backward-kill-line ()
  "Kill to the beginning of line at point."
  (interactive)
  (kill-line -1))

(defun kill-region-all ()
  "Erase the entire buffer."
  (interactive)
  (kill-region (point-min) (point-max)))

(defun copy-all ()
  "Copy the buffer content to the clipboard."
  (kill-ring-save (point-min) (point-max)))

;;; MacBook Laptop Keyboard accelerators

;; Define the <menu> key and set up the <menu> map.
(define-key key-translation-map (kbd "<f13>") (kbd "<menu>"))
(defvar menu-map nil "Keymap for menu key (global) operations.")
(define-prefix-command 'menu-map)
(global-set-key (kbd "<menu>") 'menu-map)

;; Special accelerations: those with no need for quick repetition.
(define-key menu-map ")" 'text-scale-set)
(define-key menu-map "," (kbd "C-c C-c"))
(define-key menu-map "-" '2-windows-vertical-to-horizontal)
(define-key menu-map "|" '2-windows-horizontal-to-vertical)
(define-key menu-map "0" 'delete-window)
(define-key menu-map "6" 'make-frame-command)
(define-key menu-map "7" 'delete-other-windows)
(define-key menu-map "8" 'split-window-below)
(define-key menu-map "9" 'split-window-right)
(define-key menu-map ";" 'helm-M-x)
(define-key menu-map "C" 'compile)
(define-key menu-map "P" 'projectile-switch-project)
(define-key menu-map "\\" 'delete-horizontal-space)
(define-key menu-map "a" 'tbd)
(define-key menu-map "b" 'helm-mini)
(define-key menu-map "c" 'ns-copy-including-secondary)
(define-key menu-map "d" 'delete-indentation)
(define-key menu-map "e" 'eval-last-sexp)
(define-key menu-map "f" 'helm-find-files)
(define-key menu-map "g" 'rgrep)
(define-key menu-map "h" 'tbd)
(define-key menu-map "i" 'tbd)
(define-key menu-map "j" 'tbd)
(define-key menu-map "k" 'tbd)
(define-key menu-map "l" 'tbd)
(define-key menu-map "m" 'tbd)
(define-key menu-map "n" 'tbd)
(define-key menu-map "o" 'occur)
(define-key menu-map "p" 'goto-position)
(define-key menu-map "r" 'replace-string)
(define-key menu-map "s" 'sh)
(define-key menu-map "t" 'tbd)
(define-key menu-map "u" 'package-list-packages)
(define-key menu-map "v" 'tbd)
(define-key menu-map "w" 'compare-windows)
(define-key menu-map (kbd "<SPC>") 'exchange-dot-and-mark)
(define-key menu-map (kbd "<escape>") 'save-buffers-kill-terminal)
(define-key menu-map (kbd "<menu> b") 'eval-buffer)
(define-key menu-map (kbd "<menu> c") 'compile)
(define-key menu-map (kbd "<menu> d") 'eval-defun)
(define-key menu-map (kbd "<menu> e") 'eval-expression)
(define-key menu-map (kbd "<menu> f") 'browse-url-of-buffer)
(define-key menu-map (kbd "<menu> g") 'prelude-google)
(define-key menu-map (kbd "<menu> i") 'insert-file)
(define-key menu-map (kbd "<menu> j") 'ace-jump)
(define-key menu-map (kbd "<menu> k") 'ace-window)
(define-key menu-map (kbd "<menu> o") 'other-frame)
(define-key menu-map (kbd "<menu> p") 'browse-url-at-point)
(define-key menu-map (kbd "<menu> r") 'eval-region)

;;; Ergonomic bindings using "Super" chords: those key sequences are
;;; commonly used with a benefit to having easy and quick
;;; repeatability.

(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "C-.") 'end-of-buffer)
(global-set-key (kbd "H-'") 'tbd)
(global-set-key (kbd "H-,") 'move-text-up)
(global-set-key (kbd "H-;") 'tbd)
(global-set-key (kbd "H-b") 'helm-mini)
(global-set-key (kbd "H-c") 'org-insert-link)
(global-set-key (kbd "H-f") 'rgrep)
(global-set-key (kbd "H-g") 'goto-position)
(global-set-key (kbd "H-h") 'ded/org-show-previous-heading-tidily)
(global-set-key (kbd "H-i") 'capitalize-word)
(global-set-key (kbd "H-j") 'beginning-of-buffer)
(global-set-key (kbd "H-k") 'end-of-buffer)
(global-set-key (kbd "H-l") 'ded/org-show-next-heading-tidily)
(global-set-key (kbd "H-m") 'move-text-down)
(global-set-key (kbd "H-o") 'downcase-word)
(global-set-key (kbd "H-p") 'run-presentation)
(global-set-key (kbd "H-r") 'replace-string)
(global-set-key (kbd "H-u") 'upcase-word)
(global-set-key (kbd "s-'") 'recenter-top-bottom)
(global-set-key (kbd "s-,") 'scroll-other-window-down)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") 'text-scale-set)
(global-set-key (kbd "s-;") 'tbd)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s-[") 're-search-backward)
(global-set-key (kbd "s-]") 're-search-forward)
(global-set-key (kbd "s-a") 'kill-word)
(global-set-key (kbd "s-b") 'undo-tree-redo)
(global-set-key (kbd "s-d") 'delete-file-and-buffer)
(global-set-key (kbd "s-e") 'kill-line)
(global-set-key (kbd "s-f") 'backward-delete-char-untabify)
(global-set-key (kbd "s-g") 'magit-status)
(global-set-key (kbd "s-h") 'tbd)
(global-set-key (kbd "s-i") 'backward-word)
(define-key key-translation-map (kbd "s-j") (kbd "M-n"))
(define-key key-translation-map (kbd "s-k") (kbd "M-p"))
(global-set-key (kbd "s-l") 'tbd)
(global-set-key (kbd "s-m") 'scroll-other-window)
(global-set-key (kbd "s-n") 'scroll-down-command)
(global-set-key (kbd "s-o") 'tbd)
(global-set-key (kbd "s-p") 'reserved)  ;Reserved for Projectile
;;(global-set-key (kbd "s-p;") 'projectile-compile-project)
(define-key key-translation-map (kbd "s-p ;") (kbd "s-p c"))
(global-set-key (kbd "s-q") 'fill-paragraph)
(global-set-key (kbd "s-r") 'backward-kill-line)
(global-set-key (kbd "s-s") 'backwark-kill-word)
(global-set-key (kbd "s-t") 'tbd)
(global-set-key (kbd "s-u") 'forward-word)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-y") 'transpose-chars)
(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-{") 'beginning-of-buffer-other-window)
(global-set-key (kbd "s-}") 'end-of-buffer-other-window)

;; Map mode sensitive functions using key translations

;;(define-key key-translation-map (kbd "s-w") (kbd "C-x"))

;; Define keys to support the Google Pixel when runing Xubuntu via Crouton.

(define-key global-map [M-up] 'scroll-up)
(define-key global-map [M-down] 'scroll-down)
(define-key global-map [M-S-up] 'beginning-of-buffer)
(define-key global-map [M-S-down] 'end-of-buffer)

(provide 'terminal-support)
;;; terminal-support.el ends here
