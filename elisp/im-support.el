(require 'package-support)

;; Determine if the package has already been installed.
(if (not (package-installed-p "tnt"))

    ;; It hasn't.  Attempt to install it.
    (package-register "tnt" (concat site-lisp "/tnt")))

(require 'tnt)
(setq tnt-default-username "pajatopmr"
      tnt-beep-on-incoming-message 'audible
      tnt-beep-on-visible-incoming-message nil
      tnt-beep-on-outgoing-message nil
      tnt-beep-on-chat-invitation 'audible
      tnt-beep-on-visible-chat-message nil
      tnt-beep-on-buddy-signon nil
      tnt-beep-on-buddy-signoff nil
      tnt-beep-on-signon nil
      tnt-beep-on-signoff nil
      tnt-beep-on-error 'visible
      tnt-message-on-buddy-signonoff nil
      tnt-message-on-chatroom-message t
      tnt-use-timestamps t
      tnt-use-split-buddy t
      tnt-use-keepalive t
      tnt-use-buddy-update-timer t
      tnt-use-idle-timer t
      tnt-separator "\n\n")

(provide 'im-support)
