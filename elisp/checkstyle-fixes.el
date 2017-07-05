;;; A library of useful source fixing function.

(defun fix-non-javadoc ()
  "Turn default eclipse non-Javadoc comments into TRUST \
checkstyle compliant source code"
  (interactive)
  (goto-char (point-min))
  (replace-regexp "/\\*$" "/**")
  (goto-char (point-min))
  (replace-regexp " (non-Javadoc)" " Eclipse supplied source code.\n\t *\n\t * @version $Id$\n\t *")
  (untabify (point-min) (point-max)))

(provide 'checkstyle-fixes)