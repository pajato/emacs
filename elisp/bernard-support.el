;;; Support for the bernard project, a Maven project used to maintain
;;; music repositories.
;;;
;;; Execute this script from the top level directory.

;; Set up the (j)tags support.
(require 'java-support)
(setq tags-table-list (list
                       (get-java-sources)
                       (concat default-directory "src/main/java")
                       (concat default-directory "src/test/java")))


(provide 'bernard-support)


