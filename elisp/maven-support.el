;;; Support for Maven 2

(require 'compile)

(setq compile-search-file "pom.xml")

(defun find-search-file ()
  ;; Search for the pom file traversing up the directory tree.
  (let ((path (find-sentinel-path compile-search-file))
	dir)
    (if (not path)
	(error "Sentinel file %s is missing" compile-search-file)
      (setq default-directory (directory-file-name path)))))

;; Add the following to support Emacs' compile mode:
(add-to-list 'compilation-error-regexp-alist 'mvn)
(add-to-list 'compilation-error-regexp-alist-alist
 '(mvn "^\\[ERROR\\] \\(.+?\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\].*" 1 2 3))

;;(setq compilation-process-setup-function nil)'find-search-file)

(provide 'maven-support)
