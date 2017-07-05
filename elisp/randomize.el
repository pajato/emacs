;;; randomize.el --- commands to randomly arrange lines in an Emacs buffer.

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Paul Reilly
;; Maintainer: FSF
;; Keywords: unix

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

;;; Randomly rearrange the lines in the current buffer.

(defun copy-line (number)
  "Copy the line at NUMBER + 1 to the end of the buffer."
  (let (start end)
    (goto-char (point-min))
    (forward-line number)
    (setq start (point))
    (forward-line 1)
    (setq end (point))
    (goto-char (point-max))
    (insert (buffer-substring start end))))

(defun randomize-buffer ()
  "Rearrange the lines the the current buffer in a totally random order."
  (interactive)
  (randomize-lines (point-min) (point-max)))

(defun randomize-lines (beg end)
  "Rearrange the lines in a region in a totally random order.
From a program takes two point arguments, BEG and END
a totally random order."
  (interactive "r")
  (if (> beg end)
      (let (mid) (setq mid end end beg beg mid)))
  (let ((limit (count-lines beg end))
	(count 0) (rlist nil) value)
    (random t)
    (while (< count limit)
      (setq val (random limit))
      (if (not (memq val rlist))
	  (setq rlist (cons val rlist)
		count (1+ count))))
    ;; At this point rlist has the sequence of line numbers.
    (mapcar 'copy-line rlist)
    (goto-char (point-min))
    (kill-line limit)))

;;; randomize.el ends here
