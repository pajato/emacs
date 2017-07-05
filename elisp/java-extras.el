;;; java-setup --- Support for Java coding.

;;; Commentary:

;;; Code:

(defun java-setup ()
  "Provide enhanced Java support."
  ;;(require 'cc-defs)
  (require 'java-mode-indent-annotations)
  ;;(require 'jtags)
  ;;(require 'jtags-extras)
  (require 'javadoc-lookup)
  (require 'java-import)
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92)
        indent-tabs-mode nil
        tab-width 4
        c-basic-offset 4
        fill-column 120
        c-comment-start-regexp "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  ;;(jtags-mode 1)
  (java-mode-indent-annotations-setup))

;; Use emacs-eclim to drive Eclipse support if it is supplied on the executing system.
;;(eclim-setup)

;; Use Javadoc-lookup.
(global-set-key (kbd "C-h j") 'javadoc-lookup)

;; Initialize the Jtags tags table to the default Java being used.
(setq tags-table-list (cond
                       ((eq system-type 'Xdarwin)
                        (list (format "%s/src" (getenv "JAVA_HOME"))))
                       (t nil)))

;; (setq jtags-javadoc-root-list (cond
;;                                ((eq system-type 'Xdarwin)
;;                                 (list (format "%s/apidocs" (getenv "JAVA_HOME"))))
;;                                (t nil)))

(defun get-java-sources ()
  "Return the path to the installed JDK on this system."
  (cond
   ((eq system-type 'darwin)
    (format "%s/src" (getenv "JAVA_HOME")))))

(defun load-project ()
  "Search for a project file and load/update the tags and doc reference files."
  (interactive)
  ;; search in and up from the current directory looking for a file named ".project.el"
  (let* ((filename ".project.el")
     (path (find-sentinel-path filename)))
    (if (not path)
    (error "No project file %s could be found" filename)
      (load-file path))))

(defun add-javadoc (path)
  "Add a set of javadoc files from PATH to the current javadoc table."
  (interactive "sPath to javadoc directory: ")
  (or (member path jtags-javadoc-root-list)
      (setq jtags-javadoc-root-list (append (list path) jtags-javadoc-root-list))))

(defun add-source (path)
  "Add a set of sources to the current tags table"
  (interactive "sPath to source directory: ")
  (or (member path tags-table-list)
      (setq tags-table-list (append (list path) tags-table-list))))

(defun add-constant (access name type value desc)
  "Add a constant field to the current source file at point."
  (interactive "sAccess Type: (public private(default))\nsName: \nsType: \nsValue: \nsDescription: ")
  (insert-field access t t nil name type value desc))

(defun add-property (access name type desc)
  "Add code to support the given property to the current source file at point."
  (interactive "sAccess Type: (public protected private(default))\nsName: \nsType: \nsDescription: ")
  (insert-field access nil nil nil name type nil desc))

(defun add-constructor (access desc)
  "Add code to support the given property to the current source file at point."
  (interactive "sAccess Type: (public(default) protected private)\nsDescription: ")
  (if (or (null access) (= (length access) 0))
      (setq access "public"))
  (insert-method access nil nil nil nil nil nil desc))

(defun add-method (access name type desc)
  "Add code to support the given property to the current source file at point."
  (interactive "sAccess Type: (public protected private(default))\nsName: \nsType: \nsDescription: ")
  (insert-method access nil nil nil name type nil desc))

(defun insert-field (access final static transient name type value desc)
  "Declare a field in this class."
  (let ((start-pos (point)))
    (if (or (null access) (= (length access) 0))
        (setq access "private"))
    (if (or (null desc) (= (length desc) 0))
        (setq desc "@todo Complete this documentation"))
    (insert (format "\n/**\n* %s\n*/\n%s " desc access))
    (if final (insert "final "))
    (if static (insert "static "))
    (if transient (insert "transient "))
    (insert (format "%s %s" type name))
    (if value (insert (format " = %s" value)))
    (insert ";\n")
    (indent-region start-pos (point))))

(defun insert-method (access static transient synchronized name type exceptions desc)
  "Declare a field in this class."
  (let ((start-pos (point)))
    (if (or (null access) (= (length access) 0))
        (setq access "private"))
    (if (or (null desc) (= (length desc) 0))
        (setq desc "@todo Complete this documentation"))
    (insert (format "\n/**\n* %s\n*\n* @author pmr\n*/\n%s " desc access))
    (if static (insert "static "))
    (if synchronized (insert "synchronized "))
    (if transient (insert "transient "))
    (if (and (null name) (null type))
        (insert (format "%s()" (file-name-sans-extension (buffer-name))))
      (insert (format "%s %s() " type name)))
    (if exceptions (insert (format "throws %s" exceptions)))
    (insert " {\n")
    (if (or (null type) (string= "void" type))
        (insert "// To be completed ...\n}\n")
      (insert (format "%s result;\n\nreturn result;\n\n" type)))
    (indent-region start-pos (point))))

(defun decl-setter (name type)
  "Declare the property setter (mutator)."
  (format "/**\n     * Set the property value.\n     *\n     * @param value The value to use.\n     */\n    public void set%s( %s value ) {\n        %s = value;\n    }"
          (concat (capitalize (substring name 0 1)) (substring name 1)) type name))

(defun decl-getter (name type)
  "Declare the property getter (accessor)."
  (format "/**\n     * @return The property value.\n     */\n    public %s get%s() {\n        return %s;\n    }"
          type (concat (capitalize (substring name 0 1)) (substring name 1)) name))

(provide 'java-setup)
