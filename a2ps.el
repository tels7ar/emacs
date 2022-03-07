;;; a2ps.el --- print Emacs buffer on line printer.

;; Keywords: unix

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Commands to send the region or a buffer your printer.  Entry points
;; are `a2ps-buffer', or a2ps-region'; option
;; variables include `a2ps-switches' and `a2ps-command'.

;;; Usage:
;; Put the unix command `a2ps' in your execution path
;; if the autoloads in this file are not autoloaded in the file loaddefs.el
;; then define the two commands a2ps-buffer a2ps-region in your .emacs file

;;; Code:

;;;###autoload
(defvar a2ps-switches '("-p" "-ns" "-nH" "-F11")
  "*List of strings to pass as extra switch args to `a2ps' when it is invoked.")

;;;###autoload
(defvar a2ps-command "a2ps"
  "*Shell command for printing a file")

(defvar a2ps-headers-switches nil
  "*List of strings to use as options for `a2ps' to request page headings.")

(defvar a2ps-print-region-function nil
  "Function to call to print the region on a printer.
See definition of `a2ps-print-region-1' for calling conventions.")

;;;###autoload
(defun a2ps-buffer ()
  "Print buffer contents as with Unix command `a2ps'.
`a2ps-switches' is a list of extra switches (strings) to pass to a2ps."
  (interactive)
  (a2ps-print-region-1 (point-min) (point-max) a2ps-switches nil))

;;;###autoload
(defun a2ps-region (start end)
  "Print region contents as with Unix command `a2ps'.
`a2ps-switches' is a list of extra switches (strings) to pass to a2ps."
  (interactive "r")
  (a2ps-print-region-1 start end a2ps-switches nil))

(defun a2ps-print-region-1 (start end switches page-headers)
  ;; On some MIPS system, having a space in the job name
  ;; crashes the printer demon.  But using dashes looks ugly
  ;; and it seems to annoying to do for that MIPS system.
  (let ((name (concat (buffer-name) " Emacs buffer"))
	(title (concat (buffer-name) " Emacs buffer"))
	(width tab-width))
    (save-excursion
      (message "Spooling...")
      (if (/= tab-width 8)
	  (progn
	    (a2ps-print-region-new-buffer start end)
	    (setq tab-width width)
	    (save-excursion
	      (goto-char end)
	      (setq end (point-marker)))
	    (untabify (point-min) (point-max))))
      (if page-headers
	  (if a2ps-headers-switches
	      ;; On BSD, use an option to get page headers.
	      (setq switches (append (if (stringp a2ps-headers-switches)
					 (list a2ps-headers-switches)
				        a2ps-headers-switches)
				     switches))
	    (a2ps-print-region-new-buffer start end)
	    (call-process-region start end "pr" t t nil)
	    (setq start (point-min) end (point-max))))
      (apply (or a2ps-print-region-function 'call-process-region)
	     (nconc (list start end a2ps-command
			  nil nil nil)
		    (nconc nil
			   switches)))
      (if (markerp end)
	  (set-marker end nil))
      (message "Spooling...done"))))

;; This function copies the text between start and end
;; into a new buffer, makes that buffer current,
;; and sets start and end to the buffer bounds.
;; start and end are used free.
(defun a2ps-print-region-new-buffer (ostart oend)
  (or (string= (buffer-name) " *spool temp*")
      (let ((oldbuf (current-buffer)))
	(set-buffer (get-buffer-create " *spool temp*"))
	(widen) (erase-buffer)
	(insert-buffer-substring oldbuf ostart oend)
	(setq start (point-min) end (point-max)))))

(defun a2ps-printify-region
  (begin end)
  "Turn nonprinting characters (other than TAB, LF, SPC, RET, and FF)
in the current buffer into printable representations as control or
hexadecimal escapes."
  (interactive "r")
  (save-excursion
    (goto-char begin)
    (let (c)
      (while (re-search-forward "[\^@-\^h\^k\^n-\^_\177-\377]" end t)
	(setq c (preceding-char))
	(delete-backward-char 1)
	(insert 
	 (if (< c ?\ )
	     (format "\\^%c" (+ c ?@))
	   (format "\\%02x" c)))))))

(provide 'a2ps)
;;; a2ps.el ends here
