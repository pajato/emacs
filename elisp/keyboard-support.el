(load "terminal-support")
;(require 'ergoemacs-support)

(defun setup-keymap-tooltips ()
  (interactive)
  (let ((re "\\[\\[\\([a-za-z0-9]+\\)]\\[")
        (match-list nil))
    (goto-char (point-min))
    (while (re-search-forward re (point-max) t)
      (add-to-list 'match-list (match-beginning 0)))
    match-list))

(defun describe-keymap (map prefix)
  (setq current-key-sequence prefix)
  (describe-keymap-bindings map prefix))

(defun describe-keymap-bindings (keymap prefix)
  (interactive)
  (cond
   ((and (symbolp keymap) (keymapp (or (symbol-function keymap) (symbol-value keymap))))
    (describe-keymap-bindings-internal (or (symbol-function keymap) (symbol-value keymap)) prefix))
   ((keymapp keymap)
    (describe-keymap-bindings-internal keymap prefix))
   (t
    (error "Invalid keymap argument! %s" keymap))))

(defun describe-keymap-bindings-internal (keymap prefix)
  ;;(insert (format "Prefix: %s, Keymap: %s\n" prefix keymap))
  (let ((items (cdr keymap))
        (var prefix) item event binding)
    (defun process-table-entry (key value)
      "Analyse a char-table object to determine what the set of KEY VALUE pairs is."
      (cond
       ((commandp value)
        (describe-key-sequence keymap prefix key value))
       ((keymapp value)
        (describe-key-sequence keymap prefix key value)
        (describe-keymap-bindings value (format "%s %s" prefix (key-description (vector key)))))
       (t
        (error "%s %s - Encountered an unsupported type in a char-table! Object: %s\n"
               (key-description key) prefix value))))
    (while items
      (setq item (car items)
            items (cdr items)
            event (and (listp item) (car item))
            binding (and (listp item) (cdr item)))
      (cond
       ((and (listp item) (eventp event) (alt-keymapp binding))
        ;; Found a simple event bound to a keymap, i.e. a prefix key binding.
        (describe-key-sequence keymap prefix event binding)
        (describe-keymap-bindings binding (format "%s %s" prefix (key-description (vector event)))))
       ((and (listp item) (eventp event))
        ;; Found a simple event-command keymap element.
        (describe-key-sequence keymap prefix event binding))
       ((char-table-p item)
        ;; Found a character table.
        (setq active-prefix prefix)
        (map-char-table 'process-table-entry item))
       (t (error "\nUnrecognized element with Event: %s, Binding: %s and Item: %s\n" event binding item))))))

(defun describe-key-sequence (keymap prefix key value)
  "Process by printing or converting."
  (let (text)
    (cond
     ((and (keymapp value) (symbolp value))
      (setq text (format "%s %s %s\n" prefix (key-description (vector key)) (symbol-name value))))
     ((keymapp value)
      (setq text (format "%s %s <anonymous-map>\n" prefix (key-description (vector key)))))
     (t
      (setq text (format "%s %s %s\n" prefix (key-description (vector key)) value))))
    (describe-key-sequence-maybe keymap prefix key value text)))

(defun describe-key-sequence-maybe (keymap prefix key value text)
  (let ((type (kbd (format "s-%s" (key-description (vector (event-basic-type key)))))))
    (if t
        (if (member 'control (event-modifiers key))
            (progn
              ;;(insert (format "Binding key: %s to value: %s\n" type value))
              (define-key keymap type value)))
      (insert text))))

(defun alt-keymapp (binding)
  ;; Test for a documented keymap and for an undocumented keymap variable.
  (and (not (commandp binding))
       (or (keymapp binding) (keymapp (and (boundp binding) (symbol-value binding))))))

;(describe-keymap 'menu-map "<menu>")

(provide 'keyboard-support)
