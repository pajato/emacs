;; Support for PGP encryption/decryption support.

(require 'epa-file)
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption: either the Key ID or set to nil to
;; use symmetric encryption.
(setq org-crypt-key "896DCE175E9D974C")

(provide 'org-pgp-support)

