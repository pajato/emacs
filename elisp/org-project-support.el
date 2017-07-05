(setq org-export-html-postamble nil)

(setq org-publish-project-alist
      '(("emacs-notes"
         :base-directory "~/gd/emacs/org/"
         :base-extension "org"
         :publishing-directory "~/gd/emacs/org/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         )

        ("org-notes"
         :base-directory "~/org/"
         :base-extension "org"
         :publishing-directory "~/public_html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         :auto-index t
         :index-filename "index.org"
         :index-title "Pajatome: Notes, Stories and Other Communications"
         )

        ("personal-journal"
         :base-directory "~/gd/personal/journal/"
         :base-extension "org"
         :publishing-directory "~/gd/personal/journal/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :html-preamble nil
         :htlp-postamble nil
         :index-filename "index.org"
         :index-title "Personal Journal"
         )

        ("personal-status-notes"
         :base-directory "~/gd/pgi/EngineeringNotebook/status"
         :base-extension "org"
         :publishing-directory "~/gd/pgi/EngineeringNotebook/status/html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         )

        ("brightcove-brown-bag-notes"
         :base-directory "~/gd/brightcove/EngineeringNotebook/brown-bag-lunch"
         :base-extension "org"
         :publishing-directory "~/gd/brightcove/EngineeringNotebook/brown-bag-lunch/html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         )

        ("brightcove-onceux"
         :base-directory "~/gd/brightcove/EngineeringNotebook/onceux"
         :base-extension "org"
         :publishing-directory "~/gd/brightcove/EngineeringNotebook/onceux/html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         )

        ("brightcove-ootbp"
         :base-directory "~/gd/brightcove/EngineeringNotebook/ootbp"
         :base-extension "org"
         :publishing-directory "~/gd/brightcove/EngineeringNotebook/ootbp/html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         )

        ("pgi-invoices"
         :base-directory "~/gd/pgi/EngineeringNotebook/invoices"
         :base-extension "org"
         :publishing-directory "~/gd/pgi/EngineeringNotebook/invoices/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         )

        ("org-static"
         :base-directory "~/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/public_html/"
         :recursive t
         :publishing-function org-publish-attachment)

        ("giftapp"
         :base-directory "~/gd/pgi/EngineeringNotebook/GiftApp/"
         :base-extension "org\\|css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/public_html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :index-filename "giftapp.org"
         :index-title "The Family Gift App"
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         :auto-index t)

        ("org" :components ("org-notes" "org-static"))))

(provide 'org-project-support)
