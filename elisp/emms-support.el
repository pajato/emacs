;;; emms-support.el --- The Emacs Multimedia System support library

;; Copyright (C) 2009 Pajato Systems Group, Inc.

;; Author: Paul Reilly <pmr@pajato.com>
;; Keywords: emms, flac, mp3, mpeg, multimedia, ogg

;; EMMS-SUPPORT is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; EMMS-SUPPORT is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is a set of configurations and functions to setup and enhance
;; the personal use of EMMS.

;; Additional keys on the Microsoft Natural Multimedia Pro are mapped
;; to functions as follows:

;; <Play/Pause> emms-play-toggle

;;; Code:

(require 'emms-setup)
(require 'emms-mode-line)
(require 'emms-playing-time)
(require 'emms-info-metaflac)
(emms-devel)
(emms-default-players)
(emms-mode-line 1)
(emms-playing-time 1)
(add-to-list 'emms-info-functions 'emms-info-metaflac)

;; Library functions

(defun emms-play-toggle ()
  "Start and stop emms based on the current play state."
  (interactive)
  (cond
   (emms-player-playing-p (emms-player-pause))
   (t (emms-start))))

(defun xwl-emms-track-description-function (track)
  "Return a description of the current track."
  (let* ((name (emms-track-name track))
	 (type (emms-track-type track))
	 (short-name (file-name-nondirectory name))
	 (play-count (or (emms-track-get track 'play-count) 0))
	 (last-played (or (emms-track-get track 'last-played) '(0 0 0)))
	 (empty "..."))
    (setq xwl-emms-playlist-last-track track)
    (cond
     ((eq type 'file)
      (let* ((artist (or (emms-track-get track 'info-artist) empty))
	     (year (emms-track-get track 'info-year))
	     (playing-time (or (emms-track-get track 'info-playing-time) 0))
	     (min (/ playing-time 60))
	     (sec (% playing-time 60))
	     (album (or (emms-track-get track 'info-album) empty))
	     (tracknumber (emms-track-get track 'info-tracknumber))
	     (short-name (file-name-sans-extension
			  (file-name-nondirectory name)))
	     (title (or (emms-track-get track 'info-title) short-name))
	     
	     ;; last track
	     (ltrack xwl-emms-playlist-last-track)
	     (lartist (or (and ltrack (emms-track-get ltrack 'info-artist))
			  empty))
	     (lalbum (or (and ltrack (emms-track-get ltrack 'info-album))
			 empty))
	     
	     (same-album-p (and (not (string= lalbum empty))
				(string= album lalbum))))
	(format "%10s  %3d   %-20s%-60s%-35s%-15s%s"
		(emms-last-played-format-date last-played)
		play-count
		artist
		
		;; Combine indention, tracknumber, title.
		(concat
		 (if same-album-p ; indention by album
		     (setq xwl-emms-playlist-last-indent
			   (concat " " xwl-emms-playlist-last-indent))
		   (setq xwl-emms-playlist-last-indent "\\")
		   "")
		 (if (and tracknumber ; tracknumber
			  (not (zerop (string-to-number tracknumber))))
		     (format "%02d." (string-to-number tracknumber))
		   "")
		 title        ; title
		 )
		
		;; album
		(cond ((string= album empty) empty)
		      ;; (same-album-p "  ")
		      (t (concat "ã€Š" album "ã€‹")))
		
		(or year empty)
		(if (or (> min 0)  (> sec 0))
		    (format "%02d:%02d" min sec)
		  empty))))
     ((url)
      (concat (symbol-name type) ":" name))
     (t
      (format "%-3d%s"
	      play-count
	      (concat (symbol-name type) ":" name))))))

(setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find
      emms-source-file-default-directory "/mnt/data/audio/flac/Heather Scott"
      emms-last-played-format-alist
      '(((emms-last-played-seconds-today) . "%a %H:%M")
	(604800                           . "%a %H:%M") ; this week
	((emms-last-played-seconds-month) . "%d")
	((emms-last-played-seconds-year)  . "%m/%d")
	(t                                . "%Y/%m/%d"))
      xwl-emms-playlist-last-track nil
      xwl-emms-playlist-last-indent "\\"
      emms-track-description-function 'xwl-emms-track-description-function)

(emms-add-directory-tree "/mnt/data/audio/flac/Heather Scott")

;; Key mappings

(define-key global-map [XF86AudioPlay] 'emms-play-toggle)
(define-key global-map [XF86Documents] 'emms-playlist-mode-go)
(define-key global-map [XF86AudioStop] 'emms-stop)

