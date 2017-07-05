;;; An engine for processing lists in a dired like fashion.

;;; ISSUES: 

;; # Is there a very top level that collects statistics on contained
;;   mail spools, a sort of mail repository?

;;; DESIGN SPEC

;; The base data structure is:
;;
;; 1) A hierarchical list mapping a name to content, e.g.
;;    (("psg" 'spool ...
;;       ("Junk" 'file "Junk" ...)
;;       ("inbox" 'file "inbox" ...)
;;       ("Family" 'folder "Family/" ...
;;         ("Bob" 'file ...)
;;         ("Mark"  'file ...)
;;         ("Cousins 'folder ...
;;           ("Erin" 'file ...)
;;           ("Brother" 'file ...)))
;;       ("Emacs" 'folder ...
;;         ("emms" 'file ...)
;;         ("Devel" 'file ...))
;;     ("gmail" 'spool ...)
;;
;;    which produces a tree that looks like:
;;
;;    psg
;;      Junk
;;      inbox
;;      Family
;;        Bob
;;        Mark
;;        Cousins
;;          Erin
;;          Brother
;;      Emacs
;;        emms
;;        Devel
;;    gmail

;; In the context of a mail spool, a mail "directory" is a list of
;; lists.  Each sublist is one of three types: 1) a mail spool entry,
;; 2) a mail folder entry or 3) a mail file entry.

;; Each entry specifies a name.  The spool entry name is the mail
;; spool nickname.

The root buffer should look like:

/:
Type    New    Unread  Delete  Total   Name
imap       12      32       4   12777  psg
pop         0       0       0    7523  gmail
local       0       0       0       6  pmr

If "psg" is opened the buffer shows:

/psg:
Type    New    Unread  Delete  Total   Name
file        2      12       0    5699  Junk  
file       10      20       4      96  inbox
folder      0       0       0    6982  Family

and opening "Family" shows:

/psg/Family:
Type    New    Unread  Delete  Total   Name
file        0       0       0    2472  Bob
file        2       0       0    2596  Mark
folder      0       0       0    1914  Cousins


An alternative format:
+ psg (n12,u32,d4,t15654)

- psg [new#,unread#,delete#,total#]            s(how all) h(ide) r(efresh)
  inbox [...]          v(isit) s(how) h(ide) r(efresh)
  Junk [...]
  Sent [...]
  + DotCom
  + Emacs
  + Family


