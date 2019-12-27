;;; workdir.el --- Use work sheets within dirs to organize your projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019

;; Author:  Public Image Ltd. (joerg@joergvolbers.de)
;; Keywords: files
;; Version: 0.2
;; Package-Requires: ((seq "2.20") (reader-db "0.1") (emacs "26.1"))
;; URL: https://github.com/publicimageltd/workdir

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
;;  - Some API to automatically 'populate' the data base

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

(defvar-local workdir-actively-chosen-buffer nil
  "Buffer local marker set by `workdir-visit-or-create-worksheet'.

Useful for hooks to determine \"once only actions\":

If the variable is undefined or its value nil, the buffer might
have been visited by internal functions like `find-file', but not
via the official workdir selection interfaces
`workdir-visit-or-create-worksheet' or `workdir-visit-worksheet',
respectively.

If the variable is set and t, the buffer had been actively
selected at least once.")

;; --------------------------------------------------------------------------------
;; * Customizable Variables

(defgroup workdir
  nil
  "Lightweight project management."
  :group files)

(defcustom workdir-database-name
  "worksheets"
  "Data base file name. File will be located in the user directory."
  :group 'workdir
  :type 'string)

(defcustom workdir-default-sheet
  "konzeptblatt.org"
  "Default name for automatically created work sheets."
  :group 'workdir
  :type 'string)

(defcustom workdir-archive-directory
  nil
  "Default move target for archiving work dirs."
  :group 'workdir
  :type 'directory)

(defcustom workdir-new-dirs-directory
  nil
  "Directory in which new work dirs are created."
  :group 'workdir
  :type 'directory)

(defcustom workdir-post-selection-hook nil
  "Hook run after switching to a workdir."
  :group 'workdir
  :type 'hook)

(defcustom workdir-visit-worksheet-hook
  '(workdir-visit--todo-tree  workdir-visit--bob)
  "Hook called after visiting a worksheet.
All functions are called in sequential order with the worksheet
buffer current."
  :group 'workdir
  :type 'hook
  :options '(workdir-visit--todo-tree  workdir-visit--bob))

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
  "Find out the path separator used by the operating system."
  (substring (file-name-as-directory "x") 1 2))

;; --------------------------------------------------------------------------------
;; * Database

(defun workdir-worksheet-database-file ()
  "Return the full path to the data base file."
  (locate-user-emacs-file workdir-database-name))

(defun workdir-new-data-base ()
  "Create a completely new data base file."
  (reader-db-init (workdir-worksheet-database-file) workdir-database-definition))

(defun workdir-read-worksheets (&optional prompt-for-basedir)
  "Return the stored work sheets.
Returns a list file paths. If the work sheet does not
exist (anymore), silently remove its path from the result list.

Option PROMPT-FOR-BASEDIR lets the user prompt for a basedir. In
this case, do not retrieve the file list from the data base.
Instead, scan the base dir given by the user for possible work
dirs and return that list."
  (if prompt-for-basedir
      (let* ((basedir   (completing-read " Select basedir:"
				       (list workdir-new-dirs-directory workdir-archive-directory)
				       nil t))
	     (file-list	(workdir-fast-find-sheets basedir)))
	(or file-list
	    (user-error (format "Directory '%s' contains no workdirs." basedir))))
    (seq-filter #'file-exists-p (reader-db-get (workdir-worksheet-database-file) 'worksheets))))

(defun workdir-write-worksheets (worksheets)
  "Write WORKSHEETS to the workdir data base."
  (when (not (file-exists-p (workdir-worksheet-database-file)))
    (workdir-new-data-base))
  (reader-db-put (workdir-worksheet-database-file) 'worksheets worksheets))

(defun workdir-sanitize-data-base ()
  "Clean up the data base.
Remove duplicate files, normalize the path names, allow only readable files."
  (workdir-write-worksheets
   (seq-remove #'null
	       (seq-map
		(lambda (f) (and (file-readable-p f) f))
		(seq-uniq (seq-map #'expand-file-name
				   (workdir-read-worksheets)))))))

;; Populate the data base programmatically.

(defun workdir-find-sheet-in-dir (dir-name)
  "Return the full path of the worksheet located in DIR-NAME.
If there is no readable work sheet in that directory, return
nil."
  (let* ((file (concat (file-name-as-directory dir-name) workdir-default-sheet)))
    (when (file-readable-p file)
      file)))

(defun workdir-find-sheets-recursively (dir-name)
  "Traverse DIR-NAME recursively and return a list work sheets.
A work sheet is defined as a file name matching
`workdir-default-sheet'."
  (with-temp-message (format "Collecting workdirs in %s" dir-name)
    (directory-files-recursively  dir-name
				  (concat (regexp-quote workdir-default-sheet) "$"))))

(defun workdir-fast-find-sheets (basedir)
  "Return a list of all WORKDIRS within BASEDIR (no recursion)."
  (let* ((dirs (directory-files basedir t nil t)))
    (seq-filter #'identity
		(seq-map #'workdir-find-sheet-in-dir dirs))))
 
;; (workdir-find-sheets-recursively workdir-archive-directory)
;; (workdir-fast-find-sheets workdir-archive-directory)
(defun workdir-populate-data-base (dir-name)
  "Recurse DIR-NAME and store found work sheets in the database."
  (interactive (list workdir-new-dirs-directory))
  (message "%s" dir-name)
  (workdir-write-worksheets (workdir-find-sheets-recursively dir-name)))

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
    (if (seq-contains (workdir-read-worksheets) (expand-file-name file) #'string=)
	(user-error "Current buffer's file already registered as work sheet")
      (workdir-add-file file)
      (message "Registered current buffer's file as work sheet"
	       (if (org-agenda-file-p)
		   "."
		 (org-agenda-file-to-front)
		 " and added it to the agenda file list.")))))
    
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

(defun workdir-remove-by-base-dir (dir-name)
  "Remove all paths matching DIR-NAME from the data base."
  (let ((file-name (expand-file-name (file-name-as-directory dir-name))))
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

(defun workdir-sort-by-date (worksheets)
  "Sort WORKSHEETS by modification date."
  (seq-sort #'file-newer-than-file-p worksheets))

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

(defun workdir--selector-agenda-info (worksheet)
  "Return a string indicating that WORKSHEET is a registered org agenda file."
  (if (seq-contains (org-agenda-files) worksheet) " (Agenda)" ""))

(defun workdir--selector-visited-info (worksheet)
  "Return a string indicating that WORKSHEET is a currently visited buffer."
  (if (find-buffer-visiting worksheet) "V" ""))

(defun workdir--selector-modified-info (worksheet)
  "Return a string indicating that WORKSHEET is modified and not saved yet."
  (let (buf)
    (if (and (setq buf (find-buffer-visiting worksheet))
	     (buffer-modified-p buf))
	"*"
      "")))
  
(defun workdir-path-selector (format-list worksheet)
  "Build a string representing WORKSHEET for minibuffer selection.
For the format of FORMAT-LIST, see `workdir--selector-format'."
  (string-join (seq-map (lambda (spec)
			  (format (nth 0 spec) (funcall (nth 1 spec) worksheet)))
			format-list)
	       " "))
  
(defun workdir-worksheets-as-alist (worksheets)
  "Return WORKSHEETS as an alist suitable for `completing-read'."
  (seq-group-by (workdir-curry #'workdir-path-selector workdir--selector-format)
		worksheets))

(defun workdir--prompt-for-worksheet (worksheets prompt &optional no-match-required)
  "PROMPT the user to select one of WORKSHEETS."
  (when (featurep 'ivy)
    (add-to-list 'ivy-sort-functions-alist `(,this-command . nil)))
  (let* ((alist  (workdir-worksheets-as-alist (workdir-sort-by-date worksheets)))
	 (key    (completing-read prompt alist nil
				  (not no-match-required)
				  nil
				  'workdir--selection-history))
	 (path (cadr (assoc key alist))))
    (if (and (null path) no-match-required)
	key
      path)))

;; * Add-ons when visiting a worksheet:

(defun workdir-visit--todo-tree ()
  "Show org mode todo tree."
  (when (and (eq major-mode 'org-mode)
	     (not (local-variable-p 'workdir-actively-chosen-buffer)))
    (save-window-excursion
      (org-show-todo-tree nil))))

(defun workdir-visit--bob ()
  "Move point to beginning of buffer."
  (when (not (local-variable-p 'workdir-actively-chosen-buffer))
    (beginning-of-buffer)))

;; * Create / visit workdir

;;;###autoload
(defun workdir-visit-worksheet (worksheet &optional prompt-for-basedir)
  "Visit WORKSHEET in the selected window.
With prefix PROMPT-FOR-BASEDIR set, prompt the user for a
directory and return all workdirs in that directory."
  (interactive (list (workdir--prompt-for-worksheet (workdir-read-worksheets current-prefix-arg) "Visit work dir: ")
		     current-prefix-arg))
  (push-mark nil t)
  (let* ((target-buffer (or (find-buffer-visiting worksheet)
			    (find-file-noselect worksheet))))
    ;; Maybe allow to pass an fn for more display flexibility?
    (switch-to-buffer target-buffer)
    (run-hooks 'workdir-visit-worksheet-hook)
    (setq-local workdir-actively-chosen-buffer t)
    (run-hooks 'workdir-post-selection-hook)))

(defun workdir-sanitize-name (name)
  "Remove whitespaces in NAME."
  (thread-last
      name
    (string-trim)
    (replace-regexp-in-string "[^[:alnum:]]" "_")))

(defun workdir-join-paths (basedir dirname)
  "Join BASEDIR and DIRNAME to form a path."
  (concat (file-name-as-directory (expand-file-name basedir)) dirname))

(defun workdir-do-create (basedir name sheet-name &optional register-file)
  "Within BASEDIR, create new workdir NAME and a worksheet file SHEET-NAME.
If REGISTER-FILE is non-nil and the created file is in `org-mode`,
also register the file as an agenda file."
  (let* ((path (workdir-join-paths basedir name)))
    (if (file-exists-p path)
	(user-error "Directory '%s' already exists" path)
      (make-directory path)
      (let ((sheet (concat (file-name-as-directory path) sheet-name)))
	(with-temp-file sheet) ;; create empty file
	(workdir-add-file sheet)
	(when register-file
	  (with-current-buffer (find-file sheet)
	    (when (eq major-mode 'org-mode)
	      (org-agenda-file-to-front))))))))
    
;;;###autoload
(defun workdir-create (name)
  "Create workdir NAME within `workdir-new-dirs-directory'."
  (interactive "MNew workdir project: ")
  ;; some chceks:
  (unless workdir-new-dirs-directory
    (user-error "Variable `workdir-new-dirs-directory' has to be set"))
  (when (or (null name) (string-blank-p name))
    (user-error "No project name. Canceled"))
  (when (string-match-p (regexp-quote (workdir-path-separator)) name)
    (user-error "Name '%s' must not contain a path separator" name))
  (if (not (y-or-n-p (format "Create new workdir project '%s'? " name)))
      (message "Canceled.")
    (workdir-do-create workdir-new-dirs-directory name workdir-default-sheet t)))

;;;###autoload
(defun workdir-visit-or-create-worksheet (worksheet)
  "Visit WORKSHEET in the selected window or create it.
WORKSHEET must be either a path to an existing file or a string
representing a new workdir to be created. If WORKSHEET does not
point to an existing file, create a new workdir with that name."
  (interactive (list (workdir--prompt-for-worksheet (workdir-read-worksheets) "Select or create a work dir: " t)))
  (unless workdir-new-dirs-directory
    (user-error "Variable `workdir-new-dirs-directory' has to be set"))
  (if (not (seq-contains (workdir-read-worksheets) worksheet #'string=))
      (workdir-create worksheet)
    (workdir-visit-worksheet worksheet)))

;; * Unregister Workdir

;;;###autoload
(defun workdir-unregister (worksheet)
  "Remove WORKSHEET from the internal register and from the org agenda list."
  (interactive (list (workdir-guess-or-prompt-worksheet "Unregister worksheet: ")))
  (unless worksheet
    (user-error "Canceled"))
  ;;
  (when (org-agenda-file-p worksheet) (org-remove-file worksheet))
  (workdir-remove-file worksheet)
  (message "Unregistered worksheet '%s'" worksheet))

;; * Delete Workdir

;;;###autoload
(defun workdir-delete (worksheet &optional unconditionally)
  "Delete WORKSHEET and the complete workdir defined by WORKSHEET.
Do it UNCONDITIONALLY (no questions asked) if wanted."
  (interactive (list (workdir--prompt-for-worksheet (workdir-read-worksheets) "Delete workdir: ")))
  (unless worksheet
    (user-error "Canceled"))
  (let* ((files      (directory-files (file-name-directory worksheet) nil directory-files-no-dot-files-regexp t))
	 (dir-name   (abbreviate-file-name (file-name-directory worksheet))))
    (if (not (workdir-kill-buffers worksheet unconditionally))
	(user-error "Could not kill all  buffers belonging to the project; canceled")
      (if (and (not unconditionally)
	       (y-or-n-p (format "Directory '%s' contains %d files. Delete? " dir-name (length files))))
	  (progn
	    (workdir-remove-file worksheet)
	    (when (org-agenda-file-p worksheet) (org-remove-file worksheet))
	    (delete-directory (file-name-directory worksheet) t)
	    (message "Deleted directory '%s'." dir-name))
	(message "Canceled.")))))


;; * Guess Workdir

(defun workdir-guess-file-name (&optional buffer)
  "Return the file name of BUFFER.
Also handles some edge cases, like dired or indirect buffers."
  (if (stringp buffer)
      buffer
    (with-current-buffer (or buffer (current-buffer))
      (cond
       ((derived-mode-p 'dired-mode) (expand-file-name default-directory))
       ((buffer-base-buffer) (with-current-buffer (buffer-base-buffer) (expand-file-name buffer-file-name)))
       (buffer-file-name (expand-file-name buffer-file-name))
       (t nil)))))
     
(defun workdir-guess-workdir ()
  "Guess the workdir the current buffer's file might belong to.
Return NIL if no associated worksheet can be found."
  (when-let* ((file-name            (workdir-guess-file-name))
	      (workdir-list         (mapcar #'file-name-directory (workdir-read-worksheets)))
	      (workdirs-as-regexps  (mapcar (workdir-compose (workdir-curry #'concat "\\`") #'regexp-quote) workdir-list))
	      (dir-name             (seq-find (workdir-rcurry #'string-match-p file-name) workdirs-as-regexps)))
    (seq-elt workdir-list (seq-position workdirs-as-regexps dir-name))))

(defun workdir-get-worksheet (workdir)
  "Return the worksheet associated with WORKDIR."
  (when workdir
    (when-let ((match  (assoc-string (file-name-as-directory workdir)
				     (seq-map (lambda (s) (cons (file-name-directory s) s))
					      (workdir-read-worksheets)))))
      (cdr match))))


(defun workdir-guess-or-prompt-visiting-workdir (prompt)
  "Guess current buffer's workdir, or PROMPT user to select a currently visited worksheet."
  (or (workdir-guess-workdir)
      (when-let ((worksheet (workdir--prompt-for-worksheet
			     (seq-filter #'find-buffer-visiting (workdir-read-worksheets))
			     prompt)))
	(file-name-directory worksheet))))

(defun workdir-guess-or-prompt-worksheet (prompt)
  "Guess current worksheet or PROMPT user to select one."
  (if-let ((dir (workdir-guess-workdir)))
      (buffer-file-name)
    (workdir--prompt-for-worksheet (workdir-read-worksheets) prompt)))

;; * Killing or Ibuffer all Workdir Buffers

(defun workdir-buffer-belongs-to-worksheet-p (worksheet-or-workdir buffer)
  "Check if BUFFER belongs to WORKSHEET-OR-WORKDIR.
Argument can be either a full file path or a directory. If
WORKSHEET-OR-WORKDIR is a file path, check the buffer file name
against its parent directory."
  (when-let* ((base-dir (file-name-directory worksheet-or-workdir))
	      (file     (workdir-guess-file-name buffer))
	      (exp-dir  (expand-file-name base-dir))
	      (exp-file (expand-file-name file)))
    (string-match-p (concat "\\`" (regexp-quote exp-dir)) exp-file)))


(defun workdir-buffers (worksheet-or-workdir)
  "Return all open buffers, including dired, for WORKSHEET-OR-WORKDIR.
Argument can be either a full path to a worksheet file or just a
directory path."
  (seq-filter (workdir-curry #'workdir-buffer-belongs-to-worksheet-p worksheet-or-workdir) (buffer-list)))

;;;###autoload
(defun workdir-save-and-kill-buffers (worksheet-or-workdir)
  "Save and kill all buffers defined by WORKSHEET-OR-WORKDIR."
  (interactive (list (workdir-guess-or-prompt-visiting-workdir " Save and delete all buffers from workdir: ")))
  (when-let ((buffers (workdir-buffers worksheet-or-workdir)))
    (when (y-or-n-p (format "Save and kill all %d buffers belonging to '%s'? "
			    (length buffers)
			    (abbreviate-file-name worksheet-or-workdir)))
      (while (and buffers
		  (with-current-buffer (pop buffers)
		    (save-buffer)
		    (kill-buffer)))))))

;;;###autoload
(defun workdir-ibuffer (worksheet)
  "Call ibuffer with all files belonging to WORKSHEET."
  (interactive (list (workdir-guess-or-prompt-visiting-workdir " Select workdir for ibuffer: ")))
  (let (ibuffer-use-header-line)
    (ibuffer nil "* WORKDIR IBUFFER *"
	     `((predicate ,(workdir-compose (workdir-curry #'workdir-buffer-belongs-to-worksheet-p worksheet) #'buffer-name))))))

;; * Archive

(defun workdir-kill-buffers (worksheet &optional unconditionally)
  "Kill all open buffers defined by WORKSHEET.
Even kill modified buffers if UNCONDITIONALLY is set.
Returns t if all buffers have been successfully killed."
  (interactive (list (workdir-guess-or-prompt-visiting-workdir " Kill all open buffers from workdir: ")))
  (let ((buffers (workdir-buffers worksheet)))
    (when unconditionally
      (seq-do (lambda (buf) (with-current-buffer buf (set-buffer-modified-p nil))) buffers))
    (not (seq-find (workdir-compose #'not #'kill-buffer) buffers))))

;;;###autoload
(defun workdir-archive (worksheet target)
  "Move the whole directory of WORKSHEET to TARGET.
Kill all open buffers before archiving. Also remove the file from the
work sheet data base."
  (interactive (let* ((interactive-worksheet (workdir--prompt-for-worksheet (workdir-read-worksheets) " Select workdir to move: "))
		      (interactive-target    (read-directory-name
					      (format "Move '%s' to: " (workdir-abbreviate-path interactive-worksheet))
					      (file-name-as-directory workdir-archive-directory) nil t)))
		 (list interactive-worksheet interactive-target)))
  (unless (workdir-kill-buffers worksheet)
    (user-error "There are still buffers visiting files; could not archive workdir"))
  (let* ((from (file-name-directory worksheet))
	 (to   (file-name-directory target)))
    (rename-file from to)
    (when (org-agenda-file-p worksheet) (org-remove-file worksheet))
    ;; we don't remove the original from the data base, since non-existing files are filtered out automatically.
    ;; we don't add the moved file to the data base, since archiving means: get out of my way
    (message "Moved %s to %s." from to)))


;;;###autoload
(defun workdir-go-to-root ()
  "Go to the root file of the current workdir.
Set the mark before switching to the file."
  (if-let ((target-dir (workdir-guess-workdir)))
    (if-let ((target-sheet (workdir-get-worksheet target-dir)))
	(workdir-visit-worksheet target-sheet)
      (message "Could not find work sheet file, opening work directory instead.")
      (dired target-dir))
    (user-error "No workdir project associated with current buffer")))

;;;###autoload
(defun workdir-dired-root ()
  "Open root directory of current workdir in dired."
  (interactive)
  (dired (workdir-guess-workdir)))

(provide 'workdir)
;;; workdir.el ends here
