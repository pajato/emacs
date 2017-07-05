;;; Set up Java coding style to support PSG standards.

(defun psg-setup ()
  (setq tab-stop-list
        '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)
        indent-tabs-mode nil
        tab-width 4))

(add-hook 'java-mode-hook 'psg-setup)

(provide 'psg-support)
