;; Support for JDE (Java Developement Environment for Emacs)

(let ((jde-site "/usr/local/share/emacs/site-lisp/"))
  (add-to-list 'load-path (expand-file-name (concat jde-site "jde/lisp")))
  (add-to-list 'load-path (expand-file-name (concat jde-site "cedet/common")))
  (load-file (expand-file-name (concat jde-site "cedet/common/cedet.el")))
  (add-to-list 'load-path (expand-file-name (concat jde-site "elib")))
  (require 'jde))

;; Enabling various SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
;; Select one of the following
(semantic-load-enable-code-helpers)
;; (semantic-load-enable-guady-code-helpers)
;; (semantic-load-enable-excessive-code-helpers)

;; Enable this if you develop in semantic, or develop grammars
;; (semantic-load-enable-semantic-debugging-helpers)

;; Configure debugging support:

(setq jde-debugger 'JDEbug)

(defun add-property (name type desc) 
  "Add code to support the given property to the current source file."
  (interactive "sName: \nsType: \nsDescription: ")
  (insert (format "\n    %s\n\n    %s\n\n    %s\n\n"
                  (decl-var name type desc)
                  (decl-setter name type)
                  (decl-getter name type))))

(defun decl-var (name type desc)
  "Declare the property variable."
  (format "/**\n     * Property: %s.\n     */\n    private %s %s;" desc type name))

(defun decl-setter (name type)
  "Declare the property setter (mutator)."
  (format "/**\n     * Set the property value.\n     *\n     * @param value The value to use.\n     */\n    public void set%s( %s value ) {\n        %s = value;\n    }"
          (concat (capitalize (substring name 0 1)) (substring name 1)) type name))

(defun decl-getter (name type)
  "Declare the property getter (accessor)."
  (format "/**\n     * @return The property value.\n     */\n    public %s get%s() {\n        return %s;\n    }"
          type (concat (capitalize (substring name 0 1)) (substring name 1)) name))

(provide 'jde-support)


