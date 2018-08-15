;;; workdir.el --- Use work sheets within dirs to organize your projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <joerg@joergvolbers.de>
;; Keywords: files
;; Version: 0.1
;; Package-Requires: (seq reader-db)

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Lightweight project management. A 'workdir' is simply a directory
;; with at least one single file (called 'work sheet'). All work
;; sheets are persistently stored in a data base file. Every change is
;; immediately reflected in this data base.
;;

;; The tools provided are:
;;
;;  - Create, move and delete workdirs interactively
;;  - Some special handling if work sheets are in org mode
;;     - On visiting, org mode work sheets are called with a sparse todo tree
;;  - API to automatically 'populate' the data base


;;; Code:

;; --------------------------------------------------------------------------------
;; * Dependencies
(require 'seq)
(require 'reader-db)

;; --------------------------------------------------------------------------------
;; * Some Basic Variables

(defvar workdir--selection-history nil
  "List of previously selected workdirs.")

(defvar workdir-parent-dir-regexp
  "\\`.*/\\(.+?\\)/$"
  "Regexp matching the parent directory of a directory path.

Path has to end with a trailing slash.")

(defvar workdir-database-definition '((worksheets))
  "Definition for the reader db data base.")

;; --------------------------------------------------------------------------------
;; * Customizable Variables

(defgroup workdir
  nil
  "Lightweight project management.")

(defcustom workdir-dirlist-file-name
  "worksheets"
  "File storing all available work sheets.

File will be located in the user directory."
  :group 'workdir
  :type 'string)

(defcustom workdir-default-sheet
  "konzeptblatt.org"
  "Default name for automatically created work sheets."
  :group 'workdir
  :type 'string)

(defcustom workdir-archive-directory
  "~/Dokumente/Archiv"
  "Default move target for archiving work dirs."
  :group 'workdir
  :type 'file)

(defcustom workdir-new-dirs-directory
  "~/Dokumente/projekte"
  "Directory in which new work dirs are created."
  :group 'workdir
  :type 'string)


;; --------------------------------------------------------------------------------
;; * Helper Functions

(defsubst workdir-curry (fn &rest args)
  "Return FN curried wih ARGS (from left to right)."
  (lambda (&rest more) (apply fn (append args more))))

(defsubst workdir-rcurry (fn &rest args)
  "Return FN curried with ARGS from reverse (from right to left)."
  (lambda (&rest more) (apply fn (append more args))))

(defsubst workdir-compose (fn &rest more-fn)
  "Return the composition of FN and MORE-FN."
  (seq-reduce (lambda (f g)
		(lambda (&rest args)
		  (funcall f (apply g args))))
	      more-fn
	      fn))

(defun workdir-path-separator ()
  "Find out the separator used by the operating system."
  (substring (file-name-as-directory "x") 1 2))

;; --------------------------------------------------------------------------------
;; * Database

(defun workdir-worksheet-database-file ()
  "Return full path to the data base file."
  (locate-user-emacs-file workdir-dirlist-file-name))

(defun workdir-new-data-base ()
  "Create a completely new data base file."
  (reader-db-init (workdir-worksheet-database-file) workdir-database-definition))

(defun workdir-read-worksheets ()
  "Return the work sheets stored in the data base, excluding non-existent files."
  (seq-filter #'file-exists-p (reader-db-get (workdir-worksheet-database-file) 'worksheets)))

(defun workdir-write-worksheets (worksheets)
  "Write WORKSHEETS to the workdir data base."
  (when (not (file-exists-p (workdir-worksheet-database-file)))
    (workdir-new-data-base))
  (reader-db-put (workdir-worksheet-database-file) 'worksheets worksheets))

(defun workdir-sanitize-data-base ()
  "Clean up data base.

Remove duplicate files, normalize path names, only readable files."
  (workdir-write-worksheets
   (seq-remove #'null
	       (seq-map
		(lambda (f) (and (file-readable-p f) f))
		(seq-uniq (seq-map #'expand-file-name
				   (workdir-read-worksheets)))))))


;; Populate the data base programmatically.

(defun workdir-find-sheets-in-dir (dir)
  "Return the name of the worksheet in DIR, if it exists."
  (let* ((file
	  (concat (file-name-as-directory dir)
		  workdir-default-sheet)))
    (when (file-readable-p file)
      file)))

(defun workdir-find-sheets-recursively (dir)
  "Traverse DIR recursively and return a list all files matching `workdir-default-sheet'."
  (directory-files-recursively
   dir
   (concat (regexp-quote workdir-default-sheet) "$")))

(defun workdir-populate-data-base (dir)
  "Recurse DIR and store found work sheets in the database."
  (interactive (list workdir-new-dirs-directory))
  (message "%s" dir)
  (workdir-write-worksheets (workdir-find-sheets-recursively dir)))

;; --------------------------------------------------------------------------------
;; * Convenience API to fill the data


(defun workdir-add-file (file)
  "Add FILE to the work sheet data base, if not already in there."
  (workdir-write-worksheets
   (seq-uniq (cons (expand-file-name file) (workdir-read-worksheets)))))

;;;###autoload
(defun workdir-add ()
  "Add currently visited file as a new work sheet to the data base."
  (interactive)
  (when-let ((file (buffer-file-name)))
    (workdir-add-file file)
    (when (org-agenda-file-p) (org-agenda-file-to-front))
    (message "Added visiting file as work sheet.")))
    
(defun workdir-remove-file (file)
  "Remove FILE from the work sheet data base. Exact match required."
  (let ((_file (expand-file-name file)))
    (workdir-write-worksheets (seq-remove (lambda (f) (string= f _file))
					  (workdir-read-worksheets)))))

;;;###autoload
(defun workdir-remove ()
  "Remove currently visited file from the work sheet data base."
  (when-let ((file (buffer-file-name)))
    (workdir-remove-file file)
    (when (org-agenda-file-p) (org-remove-file))
    (message "Removed visiting file from work sheet data base.")))

(defun workdir-remove-by-base-dir (dir)
  "Remove all references to files, directories and subdirectories matching DIR in the work sheet data base."
  (let ((file-name (expand-file-name (file-name-as-directory dir))))
    (workdir-write-worksheets
     (seq-remove
      (workdir-curry #'string-match-p
		     (concat "\\`" (regexp-quote file-name)))
      (workdir-read-worksheets)))))

;; -----------------------------------------------------------
;; * Prettify paths

(defun workdir-parent-directory (file)
  "Return the parent directory of FILE.

If FILE is itself a directory path, it has to end with a trailing
slash."
  (let* ((file-name  (file-name-directory (expand-file-name file)))
	 (regexp     (replace-regexp-in-string "/" (regexp-quote (workdir-path-separator)) workdir-parent-dir-regexp)))
    (if (string-match regexp file-name)
	(match-string 1 file-name)
      "")))

(defun workdir-abbreviate-path (file)
  "Return an abbreviated version of FILE.

FILE should point to a file, not to a directory."
  (concat
   (file-name-as-directory (workdir-parent-directory file))
   (unless (string= (file-name-nondirectory file) workdir-default-sheet)
     (file-name-nondirectory file))))


;; * Minibuffer interface

;; not covered by test
(defun workdir-sort-by-date (worksheets)
  "Sort WORKSHEETS by modification date."
  (seq-sort #'file-newer-than-file-p worksheets))

;; not covered by test
(defvar workdir--selector-format
  '(("%9s" workdir--selector-agenda-info)
    ("%1s" workdir--selector-visited-info)
    ("%1s" workdir--selector-modified-info)
    ("%s"  workdir-abbreviate-path))
    "Format specification for displaying a worksheet as selection candidate.

This has to be a list defining the format string and a function.
The funcion takes the path as an argument and returns the data
appropriate for the format string.

The results will be joined with a blank space.")

;; not covered by test
(defun workdir--selector-agenda-info (worksheet)
  "Return a string indicating that WORKSHEET is a registered org agenda file."
  (if (seq-contains (org-agenda-files) worksheet) " (Agenda)" ""))


;; not covered by test
(defun workdir--selector-visited-info (worksheet)
  "Return a string indicating that WORKSHEET is a currently visited buffer."
  (if (find-buffer-visiting worksheet) "V" ""))

;; not covered by test
(defun workdir--selector-modified-info (worksheet)
  "Return a string indicating that WORKSHEET is modified and not saved yet."
  (let (buf)
    (if (and (setq buf (find-buffer-visiting worksheet))
	     (buffer-modified-p buf))
	"*"
      "")))
  
;; not covered by test
(defun workdir-path-selector (format-list worksheet)
  "Build a string representing WORKSHEET for minibuffer selection.

For the format of FORMAT-LIST, see `workdir--selector-format'."
  (string-join (seq-map (lambda (spec)
			  (format (nth 0 spec) (funcall (nth 1 spec) worksheet)))
			format-list)
	       " "))
  
;; not covered by test
(defun workdir-worksheets-as-alist (worksheets)
  "Return WORKSHEETS as an alist suitable for `completing-read'."
  (seq-group-by (workdir-curry #'workdir-path-selector workdir--selector-format)
		worksheets))

;; not covered by test
(defun workdir--do-select (worksheets prompt &optional no-match-required)
  "PROMPT the user to select one of WORKSHEETS."
  (when (featurep 'ivy)
    (add-to-list 'ivy-sort-functions-alist `(,this-command . nil)))
  (let* ((alist  (workdir-worksheets-as-alist (workdir-sort-by-date worksheets)))
	 (key
	  (completing-read
	   prompt
	   alist
	   nil (not no-match-required) nil
	   'workdir--selection-history))
	 (path (cadr (assoc key alist))))
    (if (and (null path) no-match-required)
	key
      path)))

;; * Visit worksheet

(defun workdir-visit--todo-tree ()
  "Show org mode todo tree."
  (when (eq major-mode 'org-mode)
    (save-window-excursion
      (org-show-todo-tree nil))
    ))
  
(defvar workdir-visit-worksheet-hook '(workdir-visit--todo-tree
				       beginning-of-buffer)
  "Hook called after visiting a worksheet.

All functions are called in sequential order with the worksheet
buffer current.")

;;;###autoload
(defun workdir-select-or-create-worksheet (path other-window)
  "Visit or create worksheet in PATH.

With prefix OTHER-WINDOW, visit worksheet in other window.
Finally run hook `workdir-visit-worksheet-hook'."
  (interactive (list (workdir--do-select (workdir-read-worksheets) "Select or create a work dir: " t)
		     (not (null current-prefix-arg))))
  (if (not (seq-contains (workdir-read-worksheets) path #'string=))
      (workdir-create path t)
    (if (find-buffer-visiting path)
	(funcall (if other-window 'switch-to-buffer-other-window 'switch-to-buffer) (find-buffer-visiting path))
      (funcall (if other-window 'find-file-other-window 'find-file) path)
      (run-hooks 'workdir-visit-worksheet-hook))))

;; * Create Workdir

(defun workdir-convert-name-to-project-path (name)	
  "Remove whitespace in NAME."
  (thread-last
      name
    (string-trim)
    (replace-regexp-in-string "[^[:alnum:]]" "_")
    (concat (file-name-as-directory (expand-file-name workdir-new-dirs-directory)))))

;;;###autoload
(defun workdir-create (name &optional confirm)
  "Create workdir NAME within `workdir-new-dirs-directory'.

Ask for confirmation if CONFIRM is set."
  (interactive "MNew workdir project: ")
  (when (or (null name) (string-blank-p name))
    (user-error "No project name. Canceled"))
  (when (string-match-p (regexp-quote (workdir-path-separator)) name)
    (user-error "Name '%s' contains a path separator" name))
  (let ((path (workdir-convert-name-to-project-path name)))
    (if (or (not confirm) (y-or-n-p (format "Create new project '%s'? " path)))
	(if (file-exists-p path)
	    (user-error "Directory '%s' already exists" path)
	  (make-directory path)
	  (let ((sheet (concat (file-name-as-directory path) workdir-default-sheet)))
	    (with-temp-file sheet) ;; create empty file
	    (workdir-add-file sheet)
	    (with-current-buffer (find-file sheet)
	      (when (eq major-mode 'org-mode)
		(org-agenda-file-to-front)))))
      (message "Canceled."))))

;; * Delete Workdir

;;;###autoload
(defun workdir-delete (worksheet)
  "Delete WORKSHEET and the complete workdir defined by WORKSHEET."
  (interactive (list (workdir--do-select (workdir-read-worksheets) "Delete workdir: ")))
  (unless worksheet
    (user-error "Canceled"))
  (let* ((files (directory-files (file-name-directory worksheet) nil directory-files-no-dot-files-regexp t)))
    (if (not (workdir-kill-buffers worksheet))
	(user-error "There are unsaved buffers belonging to this project; canceled")
      (if (y-or-n-p (format "Directory '%s' contains %d files. Delete? "
			    (abbreviate-file-name (file-name-directory worksheet))
			    (length files)))
	  (progn
	    (workdir-remove-file worksheet)
	    (when (org-agenda-file-p worksheet) (org-remove-file worksheet))
	    (delete-directory (file-name-directory worksheet) t)
	    (message "Directory deleted."))
	(message "Canceled.")))))


;; * Guess Workdir

(defun workdir-guess-file-name (&optional buffer)
  "Return file name of BUFFER.

Also handles some edge cases, like dired or indirect buffers.."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ((derived-mode-p 'dired-mode) (expand-file-name default-directory))
     ((buffer-base-buffer) (with-current-buffer (buffer-base-buffer) (expand-file-name buffer-file-name)))
     (buffer-file-name (expand-file-name buffer-file-name))
     (t nil))))
     
(defun workdir-guess-workdir ()
  "Guess to what workdir the current buffer's file might belong to."
  (when-let ((file-name (workdir-guess-file-name)))
    (let* ((workdir-list         (mapcar #'file-name-directory (workdir-read-worksheets)))
	   (workdirs-as-regexps  (mapcar (workdir-compose (workdir-curry #'concat "\\`") #'regexp-quote) workdir-list))
	   (dir                  (seq-find (workdir-rcurry #'string-match-p file-name) workdirs-as-regexps)))
      (when dir
	(seq-elt workdir-list (seq-position workdirs-as-regexps dir))))))

(defun workdir-guess-or-select-workdir (prompt)
  "Guess current buffer's workdir, or PROMPT user to select a currently visited worksheet."
  (or (workdir-guess-workdir)
      (workdir--do-select (seq-filter #'find-buffer-visiting (workdir-read-worksheets))
			 prompt)))

;; * Killing or Ibuffer all Workdir Buffers

(defun workdir-belongs-to-directory-p (file base-dir)
  "Return t if FILE is part of the directory tree rooted at BASE-DIR."
  (string-match-p (concat "\\`" (regexp-quote (expand-file-name base-dir))) (expand-file-name file)))

(defun workdir-buffer-belongs-to-worksheet-p (worksheet buffer)
  "Check whether BUFFER refers to a file belonging to the workdir defined by WORKSHEET."
  (let* ((base-dir (file-name-directory worksheet))
	 (file     (workdir-guess-file-name buffer)))
    (when file (workdir-belongs-to-directory-p file base-dir))))

(defun workdir-buffers (worksheet)
  "Return all open buffers, including dired, for WORKSHEET."
  (seq-filter (workdir-curry #'workdir-buffer-belongs-to-worksheet-p worksheet) (buffer-list)))

;;;###autoload
(defun workdir-save-and-kill-buffers (worksheet)
  "Save and kill all buffers defined by WORKSHEET."
  (interactive (list (workdir-guess-or-select-workdir " Save and delete all buffers from workdir: ")))
  (when-let ((buffers (workdir-buffers worksheet)))
    (when (y-or-n-p (format "Save and kill all %d buffers belonging to '%s'? "
			    (length buffers)
			    (abbreviate-file-name worksheet)))
      (while (and buffers
		  (with-current-buffer (pop buffers)
		    (save-buffer)
		    (kill-buffer)))))))



;;;###autoload
(defun workdir-ibuffer (worksheet)
  "Call ibuffer with all files belonging to WORKSHEET."
  (interactive (list (workdir-guess-or-select-workdir " Select workdir for ibuffer: ")))
  (let (ibuffer-use-header-line)
    (ibuffer nil "* WORKDIR IBUFFER *"
	     `((predicate ,(workdir-compose (workdir-curry #'workdir-buffer-belongs-to-worksheet-p worksheet) #'buffer-name))))))


;; * Archive

(defun workdir-kill-buffers (worksheet)
  "Kill all open buffers defined by WORKSHEET.

Returns t if all buffers have been successfully killed."
  (interactive (list (workdir-guess-or-select-workdir " Kill all open buffers from workdir: ")))
  (let ((buffers (workdir-buffers worksheet)))
    (not (seq-find (workdir-compose #'not #'kill-buffer) buffers))))

;;;###autoload
(defun workdir-archive (worksheet target)
  "Archive the workdir defined by WORKSHEET by moving the whole dir to TARGET.

Kill all open buffers before archiving.
Remove the file from the work sheet data base."
  (interactive (let* ((dir  (workdir--do-select (workdir-read-worksheets) " Select workdir to move: "))
		      (tar  (read-directory-name
			     (format "Move '%s' to: " (workdir-abbreviate-path dir))
			     (file-name-as-directory workdir-archive-directory) nil t)))
		 (list dir tar)))
  (unless (workdir-kill-buffers worksheet)
    (user-error "There are still buffers visiting files; could not archive workdir"))
  (let* ((from (file-name-directory worksheet))
	 (to   (file-name-directory target)))
    (rename-file from to)
    (when (org-agenda-file-p worksheet) (org-remove-file worksheet))
    ;; we don't remove the original from the data base, since non-existing files are filtered out automatically.
    ;; we don't add the moved file to the data base, since archiving means: get out of my way
    (message "Moved %s to %s." from to)))

(provide 'workdir)
;;; workdir.el ends here
