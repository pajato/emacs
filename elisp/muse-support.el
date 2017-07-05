(add-to-list 'load-path "/usr/share/emacs/site-lisp/muse")
     
(require 'muse-mode)     ; load authoring mode
     
(require 'muse-html)     ; load publishing styles I use
(require 'muse-latex)
(require 'muse-texinfo)
(require 'muse-docbook)

(require 'muse-project)
     
(setq muse-project-alist
      '(("blog"                 ; my various writings
         ("~/lib/muse/blog" :default "index")
         (:base "info" :path "~/info")
         (:base "journal" :path "~/public_html/blog")
         (:base "html" :path "~/public_html")
         (:base "pdf" :path "~/public_html/pdf"))))

(provide 'muse-support)
