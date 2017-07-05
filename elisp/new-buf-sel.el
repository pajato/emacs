;; =====================================================================
;;     usenet: thorsley@ssd.csd.harris.com  USMail: Tom Horsley
;; compuserve: 76505,364                         511 Kingbird Circle
;;      genie: T.HORSLEY                         Delray Beach, FL  33444
;; ======================== Aging: Just say no! ========================
;;
;; This file provides an interactive buffer-list capability.
;; When the function select-buffer is invoked, the minibuffer
;; prompts you for another buffer to select.  The default is the second
;; buffer on the buffer-list.  Also, all the keys that are normally
;; bound to next-line and previous-line are bound to functions that
;; navigate through the buffer list.  Any keys bound to kill-buffer
;; are rebound to a function that will kill the buffer currently
;; named in the minibuffer, then move to the next buffer on the list.
;; This is a faster means of selecting another buffer than buffer-menu
;; is, but with most of the power.
;; 
;; Paul Reilly 4/3/92
;;   Modified behavior to be more transparent with the original version,
;;   i.e. the user can type newline to get the default selection, the keys
;;   associated with up or down lines, or a buffer name.  This is a slight
;;   improvement in that you can go directly to a buffer by specifying its
;;   name without having to get rid of the default input.
;;
;; Bob Weiner, Motorola, Inc., 4/5/89
;;   Added 'select-buffer-other-window' command bound to {C-x4b}
;;   usually.
;; 
;; Bob Weiner, Motorola, Inc., 10/3/91
;;   Eliminated inefficient recursion for computing buffer list.
;;   This eliminated error of passing max-lisp-eval-depth when working
;;     with many buffers.
;;   Added completion to 'select-buffer' so it works like standard
;;     'switch-to-buffer' function.
;;
;; We have gotten to where we use this technique for several of the
;; packages we have written where something prompts for input, each
;; command keeps its own history list so you can quickly cycle through
;; the previous input to just that command.
;; 
;; It is very handy to rebind the keys where next line is, so you can
;; continue to use any cursor keys.
;;
;;(autoload 'select-buffer "buff-sel" nil t)
;;(global-set-key "\C-xb" 'select-buffer)
;;(global-set-key "\C-x4b" 'select-buffer-other-window)

(defun make-buffer-list (buffer-list)
  "Returns names from BUFFER-LIST excluding those beginning with a space."
  (delq nil (mapcar '(lambda (b)
		       (if (= (aref (buffer-name b) 0) ? ) nil b))
		    buffer-list)))


(defun select-buffer (&optional other-window)
  "Interactively select a buffer using the minibuffer.
Optional argument OTHER-WINDOW non-nil means display buffer in another window.
The default buffer is the second one in the buffer-list. Other buffers can
selected either explicitly, or by using buffer-select-next and
buffer-select-prev.  Keys normally bound to next-line are bound to
buffer-select-next, those normally bound to previous-line are bound to
buffer-select-prev, and those normally bound to kill-buffer are bound to
buffer-select-kill-buf."
   (interactive)
   (let* ((buffer-select-local-list (make-buffer-list (buffer-list)))
          (buffer-name-collection
            (mapcar
              '(lambda (buf)
		 (list (buffer-name buf)))
	       buffer-select-local-list))
          (buffer-name-history
            (mapcar
              '(lambda (buf)
		 (buffer-name buf))
	       buffer-select-local-list))
	  (default-buffer (car buffer-name-history))
	  inpt)
     (setq inpt
	   (unwind-protect
	       (completing-read
		(format "Switch to buffer (default %s): " default-buffer)
		buffer-name-collection
		nil t nil (cons 'buffer-name-history 1) default-buffer t)))
     (if other-window
	 (switch-to-buffer-other-window inpt)
       (switch-to-buffer inpt))))

(defvar actual-input)

(defun select-buffer-other-window ()
  "See documentation for 'select-buffer'."
  (interactive)
  (select-buffer t))

(defun buffer-select-next ()
"Move to the next buffer on the buffer-list."
   (interactive)
   (erase-buffer)
   (setq buffer-select-list-index (1+ buffer-select-list-index))
   (if (>= buffer-select-list-index (length buffer-select-local-list))
       (setq buffer-select-list-index 0)
   )
   (insert (buffer-name (nth buffer-select-list-index buffer-select-local-list)))
)

(defun buffer-select-prev ()
"Move to the previous buffer on the buffer-list."
   (interactive)
   (erase-buffer)
   (setq buffer-select-list-index (1- buffer-select-list-index))
   (if (< buffer-select-list-index 0)
       (setq buffer-select-list-index (1- (length buffer-select-local-list)))
   )
   (insert (buffer-name
              (nth buffer-select-list-index buffer-select-local-list)))
)

(defun buffer-select-kill-buf ()
"Kill the buffer currently appearing in the minibuffer, then move to
the next buffer on the buffer-list."
   (interactive)
   (let
      (
         (mbuf (current-buffer))        ;; Save the minibuffer because
                                        ;; kill-buffer selects a buffer
         (kbuf (nth buffer-select-list-index buffer-select-local-list))
      )
     (if buffer-select-output-kill-message
	 (message "Killing buffer %s." (buffer-name kbuf)))
     (kill-buffer kbuf)
     (set-buffer mbuf))

   ;; Rebuild the buffer list, so that the killed buffer doesn't appear
   ;; in it.  Under certain circumstances, the buffer might not have
   ;; gone away, such as killing "*scratch*" when it is the last buffer.
   
   (setq buffer-select-local-list (make-buffer-list (buffer-list)))
   
   ;; Fix buffer-select-list-index, in case it went off the end of
   ;; the list (in either direction, just to be absolutely safe).

   (if (< buffer-select-list-index 0)
       (setq buffer-select-list-index (1- (length buffer-select-local-list)))
   )
   (if (>= buffer-select-list-index (length buffer-select-local-list))
       (setq buffer-select-list-index 0)
   )
   (erase-buffer)
   (insert (buffer-name
              (nth buffer-select-list-index buffer-select-local-list)))
)
