(defun go-format ()
  (add-hook 'before-save-hook #'gofmt-before-save)
  (setq tab-width 4))

