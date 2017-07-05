;;; Enable the use of James Clark's nXML package.
;;(load "/usr/share/emacs/site-lisp/nxml-mode/rng-auto.el")
;;(setq load-path (append (list (concat site-lisp "/nxml-mode")) load-path))
;;(load-library "rng-auto.el")

;;; Set up file-extension/mode associations.   
; Note that I use xml-mode for html... that's because i'm writing 
; XHTML and I want my html to conform to XML.
(setq auto-mode-alist 
      (append '(
                ("\\.fo" . nxml-mode)
                ("\\.idd" . nxml-mode)
                ("\\.ide" . nxml-mode)
                ("\\.jsp" . nxml-mode)
		("\\.jspf" . nxml-mode)
		("\\.jspi" . nxml-mode)
		("\\.jspx" . nxml-mode)
                ("\\.jxp" . nxml-mode)
                ("\\.jxf" . nxml-mode)
                ("\\.htm" . nxml-mode)
                ("\\.html" . nxml-mode)
                ("\\.xml" . nxml-mode)
                ("\\.xsd" . nxml-mode)
                ("\\.xsl" . nxml-mode)
                ("\\.sgml" . sgml-mode)
                ("\\.tld" . nxml-mode))
              auto-mode-alist))

(defun nxml-setup ()
  (setq tab-stop-list '(2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78)
        indent-tabs-mode nil
	nxml-child-indent 4
        tab-width 2))

(add-hook 'nxml-mode-hook 'nxml-setup)

;;(add-to-list 'load-path "~/gd/emacs/elisp/html5-el")
;;(eval-after-load "rng-loc"
;;  '(add-to-list 'rng-schema-locating-files "~/gd/emacs/elisp/html5-el/schemas.xml"))
;;(require 'whattf-dt)

;; Configure the nXML mode support.
(custom-set-faces
 '(nxml-name-face ((nil (:foreground "red"))))
 '(nxml-attribute-local-name-face ((t (:inherit nxml-name-face :foreground "green"))))
 '(nxml-comment-content-face ((t (:foreground "green" :slant italic))))
 '(nxml-comment-delimiter-face ((t (:inherit nxml-delimiter-face :foreground "blue"))))
 '(nxml-processing-instruction-delimiter-face ((t (:inherit nxml-delimiter-face :foreground "red"))))
 '(nxml-tag-delimiter-face ((t (:inherit nxml-delimiter-face :foreground "red"))))
 '(nxml-tag-slash-face ((t (:inherit nxml-name-face :foreground "red")))))

(provide 'sgml-support)
