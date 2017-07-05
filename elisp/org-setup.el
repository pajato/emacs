(defun org-setup()
  (require 'ox-publish)
  (require 'org-invoice-support)
  (require 'org-pgp-support)
  (require 'org-project-support)

  ;;(org-remember-insinuate)
  (setq org-directory "~/org"
        org-default-notes-file (concat org-directory "/notes.org")
        org-ditaa-jar-path "~/lib/java/ditaa0_8.jar")
  ;;      org-clock-persist 'history)
  ;;(org-clock-persistence-insinuate)

  ;;(global-set-key "\C-ca" 'org-agenda)
  ;;(global-set-key "\C-cb" 'org-iswitchb)
  ;;(global-set-key "\C-cl" 'org-store-link)
  ;;(global-set-key "\C-cr" 'org-remember)

  (setq org-remember-templates
        '(("Todo" ?t "* TODO %?\n  %i\n  %a" "~/org/todo.org" "Tasks")
          ("Journal" ?j "* %U %?\n\n  %i\n  %a" "~/org/journal.org" 'top)
          ("Idea" ?i "* %^{Title}\n  %i\n  %a" "~/org/journal.org" "New Ideas")))

  (eval-after-load "org"
  ;;   '(progn
  ;;      (define-prefix-command 'org-todo-state-map)
       
  ;;      (define-key org-mode-map "\C-cx" 'org-todo-state-map)

  ;;      (define-key org-todo-state-map "x"
  ;;        #'(lambda nil (interactive) (org-todo "CANCELLED")))
  ;;      (define-key org-todo-state-map "d"
  ;;        #'(lambda nil (interactive) (org-todo "DONE")))
  ;;      (define-key org-todo-state-map "f"
  ;;        #'(lambda nil (interactive) (org-todo "DEFERRED")))
  ;;      (define-key org-todo-state-map "l"
  ;;        #'(lambda nil (interactive) (org-todo "DELEGATED")))
  ;;      (define-key org-todo-state-map "s"
  ;;        #'(lambda nil (interactive) (org-todo "STARTED")))
  ;;      (define-key org-todo-state-map "w"
  ;;        #'(lambda nil (interactive) (org-todo "WAITING")))

  ;;      (define-key org-agenda-mode-map "\C-n" 'next-line)
  ;;      (define-key org-agenda-keymap "\C-n" 'next-line)
  ;;      (define-key org-agenda-mode-map "\C-p" 'previous-line)
  ;;      (define-key org-agenda-keymap "\C-p" 'previous-line)))

    ;; Enable support for really, really long lines while using org
    ;; mode.  Tables drive the requirement for the whitespace line
    ;; column setting.
    (setq fill-column 160
          whitespace-line-column 1024)))
