(defvar frame-use-separate-minibuf
  (cond
   ((equal instance "main")
    t)
   ((equal instance "enet")
    nil)
   ((not window-system) 
    nil)
   (t nil))
  "Select the use of a separate minibuffer on a per-instance basis.")

(defvar frame-default-colors
  (cond
   (t '((foreground-color . "gold")
        (background-color . "black")
        (mouse-color      . "white")
        (cursor-color     . "green"))))
  "Alist of colors to use by default in newly created frames.")

(defvar frame-default-font
  (cond
   ((or (string= frame-platform "reipa09-reipa09"))
    '((font . "Lucida Sans Typewriter-10")))
   ((string= frame-platform "nuad-nuad")
    '((font . "Lucida Console-11")))
   ((or (string= frame-platform "mull-mull")
        (string= frame-platform "conf-conf")
        (string= frame-platform "hamm-hamm")
        (string= frame-platform "isok-isok")
        (string= frame-platform "reipa09-reipa09")
        (string= frame-platform "tarp-tarp")
        (string= frame-platform "vent-vent"))
    '((font . "-unknown-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")))
   ((string= frame-platform "daun-daun")
    '((font . "-unknown-DejaVu Sans Mono-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1")))
   ((or (string= frame-platform "parl-parl"))
    '((font . "Lucida Sans Typewriter-14")))
   (t '((font . "-adobe-courier-medium-r-normal--14-*-*-*-m-*-iso8859-1")))))


;; Some other cool fonts:
;; 8x13

(defvar frame-default-params
  (cond
   (t
    '((cursor-type                 . box)
      (auto-raise                  . nil)
      (auto-lower                  . nil)
      (icon-type              . nil)
      (minibuffer                  . t))))
  "Alist for normal frame defaults.")

(defvar frame-minibuf-params
  (cond
   (t
    '((name                . (system-name))
      (width               . 84)
      (height              . 2)
      (left                . 0)
      (top                 . 0)
      (auto-raise          . t)
      (vertical-scroll-bars   . nil)
      (menu-bar-lines         . 0))))
  "Alist for detached minibuffer options.")

(defvar frame-select-list-index 0 "Index into frame-list")

(defvar frame-select-local-list nil "Local copy of frame-list")

(defvar frame-select-minibuffer-map
  (copy-keymap minibuffer-local-must-match-map)
  "This is a copy of the minibuffer completion keymap with all the keys that
were bound to next-line now bound to frame-select-next and all the keys
that were bound to previous-line now bound to frame-select-prev.")

(defvar frame-select-output-kill-message t
  "Flag controlling kill message, t => message is to be output.")

(defvar frame-use-frames window-system
  "Boolean to provide a fast enable/disable switch for controlling
the general use of frames.")

(defun reset-frame-position ()
  "Place the current frame at position 0,0."
  (interactive)
  (set-frame-position (window-frame (selected-window)) 0 0))

(defun frame-select-kill-frame ()
  "Kill the frame currently appearing in the minibuffer, then move to
the next frame on the frame-list."
   (interactive)
   (let
      ((kframe (nth frame-select-list-index frame-select-local-list)))
     (if frame-select-output-kill-message
         (message "Killing frame %s." (frame-get-name kframe)))
     (delete-frame kframe))

   ;; Rebuild the buffer list, so that the killed buffer doesn't appear
   ;; in it.  Under certain circumstances, the buffer might not have
   ;; gone away, such as killing "*scratch*" when it is the last buffer.
   
   (setq frame-select-local-list (make-frame-list (frame-list)))
   
   ;; Fix buffer-select-list-index, in case it went off the end of
   ;; the list (in either direction, just to be absolutely safe).

   (if (< frame-select-list-index 0)
       (setq frame-select-list-index (1- (length frame-select-local-list)))
   )
   (if (>= frame-select-list-index (length frame-select-local-list))
       (setq frame-select-list-index 0)
   )
   (erase-buffer)
   (insert (frame-get-name
              (nth frame-select-list-index frame-select-local-list))))

(defun frame-select-next ()
"Move to the next frame on the frame-list."
   (interactive)
   (erase-buffer)
   (setq frame-select-list-index (1+ frame-select-list-index))
   (if (>= frame-select-list-index (length frame-select-local-list))
       (setq frame-select-list-index 0)
   )
   (insert (frame-get-name (nth frame-select-list-index frame-select-local-list)))
)

(defun frame-select-prev ()
"Move to the previous frame on the frame-list."
   (interactive)
   (erase-buffer)
   (setq frame-select-list-index (1- frame-select-list-index))
   (if (< frame-select-list-index 0)
       (setq frame-select-list-index (1- (length frame-select-local-list)))
   )
   (insert (frame-get-name
              (nth frame-select-list-index frame-select-local-list)))
)

(defun frame-initialize ()
  ;; Initialize the frame operations ...
  (global-set-key "\C-x5f" 'frame-select)

  (mapcar
   (function 
    (lambda (keyseq)
      (define-key frame-select-minibuffer-map keyseq 'frame-select-prev)))
   (where-is-internal 'previous-line nil nil))

  (mapcar
   (function 
    (lambda (keyseq)
      (define-key frame-select-minibuffer-map keyseq 'frame-select-next)))
   (where-is-internal 'next-line nil nil))

  (mapcar
   (function 
    (lambda (keyseq)
      (define-key frame-select-minibuffer-map keyseq 'frame-select-kill-frame)))
   (where-is-internal 'delete-frame nil nil))

  ;; Tailor the Emacs built-in frame variables.
  (setq minibuffer-frame-alist (append frame-minibuf-params))
  (setq initial-frame-alist    frame-default-params)
  (setq default-frame-alist
        (append frame-default-params
                frame-default-colors
                frame-default-font
                (list (cons 'name (format "emacs@%s" display)))))

  ;; Modify initial frame --- do the font first, then the rest.
  (modify-frame-parameters
   (selected-frame)
   (append frame-default-font
           frame-default-params
           frame-default-colors))
  
  ;; Set up modeline.
  (if (not (string= (getenv "PLATFORM") "i386-*-nt"))
      (progn
        (make-face-bold 'mode-line)
        (make-face-italic 'mode-line))))

(defun frame-set-name (frame-name)
  (interactive "sNew frame name: ")
  (modify-frame-parameters (selected-frame) (list (cons 'name frame-name))))

(defun frame-get-name ()
  (interactive)
  (cdr (assoc 'name (frame-parameters (selected-frame)))))

(defun frame-get-geometry ()
  (interactive)
  (list 
   (assoc 'height (frame-parameters (selected-frame)))
   (assoc 'width (frame-parameters (selected-frame)))
   (assoc 'left (frame-parameters (selected-frame)))
   (assoc 'top (frame-parameters (selected-frame)))))

(defun frame-select ()
  "Interactively select or kill frames using the minibuffer.
The default buffer is the second one in the frame-list. Other frames can
be selected either explicitly, or by using frame-select-next and
frame-select-prev.  Keys normally bound to next-line are bound to
frame-select-next, those normally bound to previous-line are bound to
frame-select-prev, and those normally bound to kill-buffer are bound to
frame-select-kill-buf."
   (interactive)
   (let* ((save-minibuffer-map minibuffer-local-must-match-map)
          (minibuffer-local-must-match-map frame-select-minibuffer-map)
          (frame-select-list-index 1)
          (frame-select-local-list (frame-list))
          (default-frame (car (cdr frame-select-local-list)))
          inpt the-frame visip)
     (setq inpt
           (unwind-protect
               (completing-read
                (concat "Switch to frame: (" (frame-get-name default-frame) ") ")
                (mapcar '(lambda (frame)
                           (list (frame-get-name frame)))
                        frame-select-local-list)
                nil t "")
             (setq minibuffer-local-must-match-map save-minibuffer-map)))
     (if (string= inpt "")
         (setq the-frame default-frame))
     (while frame-select-local-list
       (if (string= inpt (frame-get-name (car frame-select-local-list)))
           (setq the-frame (car frame-select-local-list)
                 frame-select-local-list nil)
         (setq frame-select-local-list (cdr frame-select-local-list))))
     (make-frame-visible the-frame)
     (raise-frame the-frame)
     (select-frame the-frame)
     (set-mouse-position the-frame (- (frame-width the-frame) 1) 0)
     (unfocus-frame)))

(provide 'frame-functions)
