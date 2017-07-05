(require 'net-utils)

(cond
 ((and (eq system-type 'darwin)
       (not ping-program-options))
  (setq ping-program-options (list "-c" "4"))))

(provide 'net-utils-support)
