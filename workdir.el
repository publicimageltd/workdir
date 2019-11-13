;;; workdir.el --- Use work sheets within dirs to organize your projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019

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

;; TODO
;;  - Die interaktiven Funktionen müssen besser von den GRundfunktionen getrennt werden
;;    (also v.a. bei "create" und "visit"), um besseres Interface zu ermöglichen....
;;    traue ich mich aber nicht ran, zu müde heute

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

(defcustom workdir-refile-as-tree-target
  "~/Dokumente/Hefte/projekte.org"
  "Default target when refiling worksheets as trees."
  :group 'workdir
  :type 'file)

(defcustom workdir-new-dirs-directory
  "~/Dokumente/projekte"
  "Directory in which new work dirs are created."
  :group 'workdir
  :type 'string)

(defcustom workdir-pre-selection-hook nil
  "Hook run before switching to a workdir."
  :group 'workdir)

(defcustom workdir-post-selection-hook nil
  "Hook run after switching to a workdir."
  :group 'workdir)

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

(defun workdir-find-sheets-in-dir (dir-name)
  "Return the name of the worksheet in DIR, if it exists."
  (let* ((file
	  (concat (file-name-as-directory dir-name)
		  workdir-default-sheet)))
    (when (file-readable-p file)
      file)))

(defun workdir-find-sheets-recursively (dir-name)
  "Traverse DIR recursively and return a list all files matching `workdir-default-sheet'."
  (directory-files-recursively
   dir-name
   (concat (regexp-quote workdir-default-sheet) "$")))

(defun workdir-populate-data-base (dir-name)
  "Recurse DIR and store found work sheets in the database."
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
	(user-error "Current buffer's file already registered as work sheet.")
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
  "Remove all references to files, directories and subdirectories matching DIR in the work sheet data base."
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

;; not covered by test
(defun workdir-sort-by-date (worksheets)
  "Sort WORKSHEETS by modification date."
  (seq-sort #'file-newer-than-file-p worksheets))

;; not covered by test
(defvar workdir--selector-format
  '(;;("%15s" workdir--selector-title-info)
    ("%9s" workdir--selector-agenda-info)
    ("%1s" workdir--selector-visited-info)
    ("%1s" workdir--selector-modified-info)
    ("%s"  workdir-abbreviate-path))
  "Format specification for displaying a worksheet as selection candidate.

This has to be a list defining the format string and a function.
The funcion takes the path as an argument and returns the data
appropriate for the format string.

The results will be joined with a blank space.")

;; not covered by test
(defun workdir--selector-title-info (worksheet)
  (let* ((rg "rg '^#\\+TITLE' -m 1"))
    (concat 
     (when-let ((title (shell-command-to-string (concat rg " " (shell-quote-argument worksheet)))))
       (unless (string-empty-p title)
	 (string-trim
	  (substring (string-trim title) 8)))))))

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

;; * Visit worksheet

(defvar-local workdir-actively-chosen-buffer nil
  "Buffer local marker set by `workdir-select-or-create-worksheet'.

Useful for hooks to determine \"once only actions\".

If nil, buffer might have been visited with internal functions
like `find-file', but not with the official workdir selection
interface `workdir-select-or-create-worksheet'.

If set and t, buffer had been actively selected at least once.")

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

(defvar workdir-visit-worksheet-hook '(workdir-visit--todo-tree
				       workdir-visit--bob)
  "Hook called after visiting a worksheet.

All functions are called in sequential order with the worksheet
buffer current.")

;;;###autoload
(defun workdir-select-or-create-worksheet (worksheet prefix &optional only-select)
  "Visit or create WORKSHEET. 

Display worksheet according to PREFIX.

If PREFIX is nil, switch to worksheet in current window.

If PREFIX is :full-frame, 4 or (4), switch to worksheet in current window and delete other windows.

If PREFIX is :other-window, 16 or (16), switch to worksheet in other window.

If ONLY-SELECT is t, only select existing workdir in WORKSHEET. Do
nothing if PATH does not exist yet (i.e., do not create a new
workdir).

Finally run hook `workdir-visit-worksheet-hook'."
  (interactive (list (workdir--prompt-for-worksheet (workdir-read-worksheets) "Select or create a work dir: " t)
		     (car current-prefix-arg)))
  (push-mark nil t)
  (run-hooks 'workdir-pre-selection-hook)
  (if (and (not (seq-contains (workdir-read-worksheets) worksheet #'string=))
	   (not only-select))
      (workdir-create worksheet)
    ;;
    (let* ((visit-mode    (let* ((_prefix (if (listp prefix) (car prefix) prefix)))
			    (cond
			     ((eq _prefix 4)  :full-frame)
			     ((eq _prefix 16) :other-window)
			     (t               _prefix))))
	   (target-buffer (or
			   (find-buffer-visiting worksheet)
			   ;; one of the rare occasions where find-file-noselect seems fully appropriate
			   (find-file-noselect worksheet))))
      (cond
       ((eq visit-mode :full-frame)
	(progn
	  (switch-to-buffer target-buffer)
	  (delete-other-windows)))
       ((eq visit-mode :other-window)
	(switch-to-buffer-other-window target-buffer))
       (t              (switch-to-buffer target-buffer))))
    
    ;; Worksheet is found (or created). Now run some hooks.
    (run-hooks 'workdir-visit-worksheet-hook)
    (setq-local workdir-actively-chosen-buffer t)
    (run-hooks 'workdir-post-selection-hook)))


(defun workdir-visit-or-create-worksheet (worksheet)
  "Visit WORKSHEET in the selected window or create it.
If WORKSHEET does not point to an existing file, try to create a
new workdir with that name."
  (if (file-readable-p worksheet)
      (workdir-visit-worksheet worksheet)
    (workdir-create worksheet t)))

(defun workdir-visit-worksheet (worksheet)
  "Visit WORKSHEET in the selected window."      
  (let* ((target-buffer (or (find-buffer-visiting worksheet)
			    (find-file-noselect worksheet))))
    ;; Maybe allow to pass an fn for more display flexibility?
    (switch-to-buffer target-buffer)
    (run-hooks 'workdir-visit-worksheet-hook)))

;; * Create Workdir

(defun workdir-convert-name-to-project-path (name)	
  "Remove whitespace in NAME."
  (thread-last
      name
    (string-trim)
    (replace-regexp-in-string "[^[:alnum:]]" "_")
    (concat (file-name-as-directory (expand-file-name workdir-new-dirs-directory)))))

;;;###autoload
(defun workdir-create (name)
  "Create workdir NAME within `workdir-new-dirs-directory'."
  (interactive "MNew workdir project: ")
  (when (or (null name) (string-blank-p name))
    (user-error "No project name. Canceled"))
  (when (string-match-p (regexp-quote (workdir-path-separator)) name)
    (user-error "Name '%s' must not contain a path separator" name))
  (let* ((path (workdir-convert-name-to-project-path name)))
    (if (y-or-n-p (format "Create new workdir project '%s'? " path))
	(if (file-exists-p path)
	    (user-error "Could not create new project, directory '%s' already exists" path)
	  (make-directory path)
	  (let ((sheet (concat (file-name-as-directory path) workdir-default-sheet)))
	    (with-temp-file sheet) ;; create empty file
	    (workdir-add-file sheet)
	    (with-current-buffer (find-file sheet)
	      (when (eq major-mode 'org-mode)
		(org-agenda-file-to-front)))))
      (message "Canceled."))))

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
  "Check whether BUFFER refers to a file belonging to the workdir defined by WORKSHEET-OR-WORKDIR.

Argument can be either a full file path or a directory."
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
  "Save and kill all buffers defined by WORKSHEET."
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
  "Archive the workdir defined by WORKSHEET by moving the whole directory to TARGET.

Kill all open buffers before archiving.
Remove the file from the work sheet data base."
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
(defun workdir-go-to-root (&optional prefix)
  "Go to the root file of the current workdir.

Set the mark before switching to the file."
  (interactive (list (car current-prefix-arg)))
  (when-let ((target-dir (workdir-guess-workdir)))
    (if-let ((target-sheet (workdir-get-worksheet target-dir)))
	(workdir-select-or-create-worksheet target-sheet prefix)
      (message "Could not find root file")
      (push-mark nil t)
      (dired target-dir) ;; open in dired instead
      (user-error "Could not identify workdir project associated with the current buffer."))))

;;;###autoload
(defun workdir-dired-root ()
  "Open root directory of current workdir in dired."
  (interactive)
  (dired (workdir-guess-workdir)))

;; * Workdir <-> single subtree

(defun workdir-org-file-p (file)
  "Check for a suffix '.org' in FILE."
  (string-match-p (concat (regexp-opt '(".org")) "$") file))

(defun workdir-select-worksheet-subset (prompt pred)
  "PROMPT the user to select from a subset (filtered by PRED) of all available worksheets."
  (let* ((subset (seq-filter pred (workdir-read-worksheets)))
	 (selection (workdir--prompt-for-worksheet subset prompt)))
    selection))

(defun workdir-walk-org-tree (tree depth)
  "Return tree structure in an abbreviated form.

Intended for interactive debugging."
  (let ((type (org-element-type tree)))
    (cond
     ((eq 0 depth) type)
     ((not tree))
     ((not type)
      (mapcar
       (workdir-rcurry #'workdir-walk-org-tree (- depth 1))
       tree))
     ((eq type 'org-data)
      (list 'org-data
       (mapcar
	(workdir-rcurry #'workdir-walk-org-tree (- depth 1))
	(org-element-contents tree))))
     (t
      (seq-remove #'null
		  (let ((contents (org-element-contents tree)))
		    (list type
			  (seq-length contents)
			  (workdir-walk-org-tree
			   contents
			   (1- depth)))))))))

(defun workdir--first-section (buffer-tree)
  "Return first section of org buffer tree (before headline) or nil."
  (let* ((section-candidate
	  (nth 2 buffer-tree))
	 (type (org-element-type section-candidate)))
    (when (eq type 'section)
      section-candidate)))

(defun workdir--first-section-p (buffer-tree)
  "Check for some stuff (keywords, plain text) above first headline."
  (eq 'section (org-element-type (nth 2 buffer-tree))))

(defun workdir--keyword-value-pair (element)
  "Return :key :value pairs for org-element-property ELEMENT."
  (list 
   (org-element-property :key element)
   (org-element-property :value element)))

(defun workdir--get-file-keywords (buffer-tree)
  "Get top org keywords caught in BUFFER-TREE."
  (org-element-map (workdir--first-section buffer-tree)
      'keyword
    #'workdir--keyword-value-pair))

(defun workdir--comment-file-keywords (buffer-tree)
  "Comment out the file wide comments of an org document (passed as a parsed BUFFER-TREE)."
  (when-let ((section (workdir--first-section buffer-tree)))
    (let* ((begin (org-element-property :begin section))
	   (end   (car (org-element-map
			   section
			   'paragraph
			 (workdir-curry #'org-element-property :begin)))))
      (comment-region begin end))))

(defun workdir--indent-headline-at-pos (pos correction)
  "Insert a star at (pos + correction)."
  (goto-char (+ pos correction))
  (insert "*"))

(defun workdir--indent-all-headlines (buffer-tree)
  "Indent all headlines in the org buffer represented by BUFFER-TREE."
  (let* ((positions (org-element-map
			buffer-tree
			'headline
		      (workdir-curry #'org-element-property :begin))))
    (seq-do-indexed #'workdir--indent-headline-at-pos positions)))

(defun workdir--buffer-to-tree (file-name)
  "Change the current buffer to a tree."
  (let* ((buffer-tree (org-element-parse-buffer))
	 (keywords (workdir--get-file-keywords buffer-tree))
	 (filetags (cadr (assoc "FILETAGS" keywords)))
	 (title    (cadr (assoc "TITLE" keywords))))
    ;; indent existing headlines:
    (workdir--indent-all-headlines buffer-tree)
    ;; comment out file comments:
    (workdir--comment-file-keywords buffer-tree)
    ;; create top tree:
    (goto-char (point-min))
    (insert (concat "* "
		    (or title
			(workdir-abbreviate-path file-name))
		    "  "
		    filetags
		    "\n"))))

(defun workdir-paste-at-beginning ()
  "Paste subtree at the beginning of current org mode buffer."
  (goto-char (point-min))
  (when-let ((pos (re-search-forward (concat "^\\(" org-outline-regexp "\\)") nil t)))
    (goto-char pos)
    (forward-line 0)
    (open-line 1)
    (org-paste-subtree)))

;; We might want to convert a file which has not been saved yet, so we
;; insert its content with this function instead of
;; `insert-file-contents':
(defun workdir--get-file-contents (file)
  "Return the contents of FILE as a string."
  (save-window-excursion
    (find-file file)		     ; we do want line conversion etc.
    (buffer-string)))

(defun workdir--refile-as-tree (source-file target-file)
  "Paste contents of SOURCE-FILE as a tree into TARGET-FILE, saving it."
  (save-window-excursion
    ;; create the tree in an extra buffer...
    (let* ((buf (generate-new-buffer "*temporary refiling buffer*")))
      (with-current-buffer buf
	(insert (workdir--get-file-contents source-file))
	(org-mode)
	(workdir--buffer-to-tree source-file)
	(copy-region-as-kill (point-min) (point-max)))
      (kill-buffer buf))
    ;; ...then paste it:
    (find-file target-file)
    (workdir-paste-at-beginning)
    (save-buffer)))

(defun workdir-refile-as-tree (worksheet target-file)
  "Add the contents of WORKSHEET under a single headline to TARGET-FILE."
  (interactive (list (workdir-guess-or-prompt-worksheet "Select worksheet to refile: ")
		     (or workdir-refile-as-tree-target
			 (read-file-name "Select target file for refiling: " "~"))))
  (when (file-directory-p target-file)
    (user-error "Target has to be a file."))
  (when (not (workdir-org-file-p target-file))
    (user-error "Target file has to be an org mode file."))
  (when (not (workdir-org-file-p worksheet))
    (user-error "Only org mode files can be refiled as a tree."))
  (workdir--refile-as-tree worksheet target-file)
  (message "Refiled '%s'." (workdir-abbreviate-path worksheet))
  (workdir-delete worksheet))

(provide 'workdir)
;;; workdir.el ends here
