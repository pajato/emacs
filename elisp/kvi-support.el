;;; Set up Java coding style to support PSG standards.

(defun kvi-setup ()
  (setq tab-stop-list
        '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)
        indent-tabs-mode t
        tab-width 4))

(add-hook 'java-mode-hook 'kvi-setup)
(add-hook 'sgml-mode-hook 'kvi-setup)

(provide 'kvi-support)
