;;; workdir.el --- Use work sheets within dirs to organize your projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020

;; Author:  Public Image Ltd. (joerg@joergvolbers.de)
;; Keywords: files
;; Version: 0.3
;; Package-Requires: ((seq "2.20") (emacs "26.1") (hydra))
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
;; with at least one single file (called 'work sheet'). 

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
(require 'hydra)

;; --------------------------------------------------------------------------------
;; * Some Basic Variables

(defvar workdir-selection-history nil
  "List of previously selected workdirs.")

(defvar workdir-parent-dir-regexp
  "\\`.*/\\(.+?\\)/$"
  "Regexp matching the parent directory of a directory path.

Path has to end with a trailing slash.")

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
  :group 'files)

(defcustom workdir-default-sheet
  "konzeptblatt.org"
  "Default name for automatically created work sheets."
  :group 'workdir
  :type 'string)

(defcustom workdir-directories nil
  "List of directories which might contain workdirs.
Each element is either a directory path or the direct path to a
worksheet. Note that a direct path to a worksheet is compared
against `workdir-default-sheet', so do not forget to update this
variable if you choose another default directory name."
  :group 'workdir
  :type 'list)

(defcustom workdir-archive-directory
  nil
  "Default move target for archiving work dirs."
  :group 'workdir
  ;; FIXME does not work as expected
  :type '(repeat (choice file directory)))

(defcustom workdir-new-dirs-directory
  nil
  "Directory in which new work workdirs are created."
  :group 'workdir
  :type 'directory)

(defcustom workdir-additional-dirs
  nil
  "List of directories to select from when selecting a workdir."
  :group 'workdir
  :type 'list)

(defcustom workdir-post-selection-hook nil
  "Hook run after switching to a workdir."
  :group 'workdir
  :type 'hook)

(defcustom workdir-visit-worksheet-hook
  '(workdir-visit-todo-tree  workdir-visit-bob)
  "Hook called after visiting a worksheet.
All functions are called in sequential order with the worksheet
buffer current."
  :group 'workdir
  :type 'hook
  :options '(workdir-visit-todo-tree  workdir-visit-bob))

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

;; -----------------------------------------------------------
;; Find worksheets

(defvar workdir-use-find-binary (not (eq window-system 'w32))
  "Use `find' to find worksheets; else use internal functions.")

(defvar workdir-use-awk-binary (not (eq window-system 'w32))
  "Use `awk' to find worksheet titles; else use internal functions.")

(defun workdir-find-sheets-in-dir (dir)
  "Return all workdirs within DIR (no recursion).
If DIR is a path to a worksheet, return this file path."
  (if (string= (file-name-nondirectory dir) workdir-default-sheet)
      (list (expand-file-name dir))
    (if workdir-use-find-binary
	;; on linux:
	(or
	 (ignore-errors
	   (process-lines  "find"
			   (expand-file-name dir)
			   "-maxdepth" "2"
			   "-name" (replace-regexp-in-string "\\." "\\\\."
							     workdir-default-sheet)))
	 (unless (file-readable-p dir)
	   (error "%s is not a valid directory" dir)))
      ;; else if find is not available:
    (let* ((dirs (directory-files dir t nil t)))
      (cl-labels ((create-name (s) (concat (file-name-as-directory s)
					   workdir-default-sheet))
		  (file-ok (f) (and (not (string-prefix-p "." f))
				    (file-readable-p f))))
	(seq-filter #'file-ok (seq-map #'create-name dirs)))))))

(defun workdir-find-sheets (dirs)
  "Return all workdirs within DIRS, a list of directory names."
  (apply #'append (seq-map #'workdir-find-sheets-in-dir
			   (if (listp dirs) dirs (list dirs)))))

(defun workdir-get-worksheets (&optional prompt-for-dir)
  "Return the work sheets in `workdir-directories'.
Returns a list file paths. Option PROMPT-FOR-DIR lets the
user prompt for an alternative basedir."
  (let ((dir (if prompt-for-dir (completing-read " Select basedir:"
						 (append (list workdir-new-dirs-directory)
							 (list workdir-archive-directory)
							 workdir-directories
							 workdir-additional-dirs)
						 nil t)
	       workdir-directories)))
  (or
   (workdir-find-sheets dir)
   (user-error (format "Directory '%s' contains no workdirs." dir)))))

;; * Prettify paths

(defun workdir-parent-directory (file)
  "Return the parent directory of FILE.
If FILE has a trailing slash, it is treated as a directory path."
  (let* ((file-name  (file-name-directory (expand-file-name file)))
	 (regexp     (replace-regexp-in-string "/" (regexp-quote (workdir-path-separator)) workdir-parent-dir-regexp)))
    (if (string-match regexp file-name)
	(match-string 1 file-name)
      "")))

(defun workdir-abbreviate-path (file &optional _)
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

(defun workdir-selector-get-title-from-buf (buf)
  "Return document title of BUF, if any."
  (with-current-buffer buf
    (let (org-struct)
      (when (and (derived-mode-p 'org-mode)
		 (setq org-struct (plist-get (org-export-get-environment) :title)))
	(substring-no-properties (car org-struct))))))

(defun workdir-selector-get-title-from-file (file)
  "Return org document title of FILE, if any."
  (if workdir-use-awk-binary
      (let* ((awk-script "'BEGIN{NR==1; FS=\":\"} {print $2; nextfile}'")
	     (title    (ignore-errors
			 (shell-command-to-string
			  (concat "awk " awk-script " '" file "'")))))
	(when title
	  (string-trim title)))
    (let* ((loaded (get-file-buffer file))
	   (buf    (or loaded (find-file-noselect file)))
	   (title  (workdir-selector-get-title-from-buf buf)))
      (unless loaded
	(kill-buffer buf))
      title)))

(defun workdir-selector-prefetch-titles (files)
  "Return an alist associating each file in FILES with its title."
  (let* ((readable-files    (seq-filter #'file-readable-p files))
	 (nonreadable-files (seq-difference files readable-files #'string=))
	 (titles            (if workdir-use-awk-binary
				(let* ((awk-script "BEGIN{NR==1; FS=\":\"} {print $2; nextfile}"))
				  (ignore-errors (apply #'process-lines "awk" awk-script readable-files)))
			      (mapcar #'workdir-selector-get-title-from-file readable-files))))
    (unless titles
      (setq nonreadable-files files
	    readable-files nil))
    ;; and here is the result:
    (append
     (seq-mapn #'cons nonreadable-files (mapcar #'ignore nonreadable-files))
     (seq-mapn #'cons readable-files    (mapcar #'string-trim titles)))))

(defvar workdir-selector-format
  '(("%9s"     workdir-selector-agenda-info)
    ("%1s"     workdir-selector-visited-info)
    ("%1s"     workdir-selector-modified-info)
    ("%-80s"   workdir-selector-get-title)
    ("%s"      workdir-abbreviate-path))
  "Format specification for displaying a worksheet as selection candidate.
This has to be a list defining the format string and a function.
The function takes the path as an argument and returns the data
appropriate for the format string. As a second optional argument,
an alist will be passed to the function. This alist matches each
file name with the title of the corresponding worksheet.

If the return value of the function is nil, it will be converted
to an empty string. 

All results will be joined with a blank space.")


(defun workdir-selector-get-title (worksheet &optional prefetched-titles)
  "Return the title of WORKSHEET.
Use the alist PREFETCHED-TITLES, if passed."
  (if prefetched-titles
      (assoc-default worksheet prefetched-titles #'string= "")
    (let* ((buf  (get-file-buffer worksheet)))
      (if buf
	  (workdir-selector-get-title-from-buf buf)
	(workdir-selector-get-title-from-file worksheet)))))

(defun workdir-selector-agenda-info (worksheet &optional _)
  "Return a string indicating that WORKSHEET is a registered org agenda file."
  (when (seq-contains (org-agenda-files) worksheet) " (Agenda)"))

(defun workdir-selector-visited-info (worksheet &optional _)
  "Return a string indicating that WORKSHEET is a currently visited buffer."
  (when (find-buffer-visiting worksheet) "V"))

(defun workdir-selector-modified-info (worksheet &optional _)
  "Return a string indicating that WORKSHEET is modified and not saved yet."
  (let (buf)
    (when (and (setq buf (find-buffer-visiting worksheet))
	       (buffer-modified-p buf))
      "*")))

(defun workdir-selector-build-item (format-list prefetched-titles worksheet)
  "Build a string representing WORKSHEET for minibuffer selection.
For the format of FORMAT-LIST, see `workdir-selector-format'."
  (string-join (seq-map (lambda (spec)
			  (format (nth 0 spec) (or (funcall (nth 1 spec) worksheet prefetched-titles) "")))
			format-list)
	       " "))


(defun workdir-worksheets-for-completion (worksheets)
  "Return WORKSHEETS as an alist suitable for `completing-read'."
  (with-temp-message "Collecting worksheets..."
    (seq-group-by (apply-partially #'workdir-selector-build-item
				   workdir-selector-format
				   (workdir-selector-prefetch-titles worksheets))
		  worksheets)))

(defun workdir-prompt-for-worksheet (worksheets prompt &optional no-match-required)
  "PROMPT the user to select one of WORKSHEETS."
  (when (featurep 'ivy)
    (add-to-list 'ivy-sort-functions-alist `(,this-command . nil)))
  (unless worksheets
    (user-error "No worksheets available"))
  (let* ((alist  (workdir-worksheets-for-completion (workdir-sort-by-date worksheets)))
	 (key    (completing-read prompt alist nil
				  (not no-match-required)
				  nil
				  'workdir-selection-history))
	 (path (cadr (assoc key alist))))
    (if (and (null path) no-match-required)
	key
      path)))

;; * Add-ons when visiting a worksheet:

(defun workdir-visit-todo-tree ()
  "Show org mode todo tree."
  (when (and (eq major-mode 'org-mode)
	     (not (local-variable-p 'workdir-actively-chosen-buffer)))
    (save-window-excursion
      (org-show-todo-tree nil))))

(defun workdir-visit-bob ()
  "Move point to beginning of buffer."
  (when (not (local-variable-p 'workdir-actively-chosen-buffer))
    (call-interactively #'beginning-of-buffer)))

;; * Create / visit workdir

;;;###autoload
(defun workdir-visit-worksheet (worksheet &optional prompt-for-basedir)
  "Visit WORKSHEET in the selected window.
With prefix PROMPT-FOR-BASEDIR set, prompt the user for a
directory and return all workdirs in that directory."
  (interactive (list (workdir-prompt-for-worksheet (workdir-get-worksheets current-prefix-arg) "Visit work dir: ")
		     current-prefix-arg))
  (ignore prompt-for-basedir) ;; silence byte compiler
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

(defun workdir-do-create (dir name sheet-name &optional register-file)
  "Within DIR, create a new workdir NAME with worksheet file SHEET-NAME.
If REGISTER-FILE is non-nil and the created file is in
`org-mode`, also register the file as an agenda file."
  (let* ((path (concat (file-name-as-directory (expand-file-name dir)) name)))
    (if (file-exists-p path)
	(user-error "Directory '%s' already exists" path)
      (make-directory path)
      (let ((sheet (concat (file-name-as-directory path) sheet-name)))
	(with-temp-file sheet) ;; create empty file
	(when register-file
	  (workdir-register sheet))))))

;;;###autoload
(defun workdir-create (name)
  "Create workdir NAME within `workdir-new-dirs-directory'.
NAME will be converted to a more file-friendly name.
The resulting workdir will be added to the agenda file list."
  (interactive "MNew workdir project: ")
  ;; some chceks:
  (unless workdir-new-dirs-directory
    (user-error "Variable `workdir-new-dirs-directory' has to be set"))
  (when (or (null name) (string-blank-p name))
    (user-error "No project name. Canceled"))
  (when (string-match-p (regexp-quote (workdir-path-separator)) name)
    (user-error "Name '%s' must not contain a path separator" name))
  (setq name (workdir-sanitize-name name))
  (if (not (y-or-n-p (format "Create new workdir project '%s'? " name)))
      (message "Canceled.")
    (workdir-do-create workdir-new-dirs-directory name workdir-default-sheet t)))

;;;###autoload
(defun workdir-visit-or-create-worksheet (worksheet)
  "Visit WORKSHEET in the selected window or create it.
WORKSHEET must be either a path to an existing file or a string
representing a new workdir to be created. If WORKSHEET does not
point to an existing file, create a new workdir with that name."
  (interactive (list (workdir-prompt-for-worksheet (workdir-get-worksheets) "Select or create a work dir: " t)))
  (unless workdir-new-dirs-directory
    (user-error "Variable `workdir-new-dirs-directory' has to be set"))
  (if (not (seq-contains (workdir-get-worksheets) worksheet #'string=))
      (workdir-create worksheet)
    (workdir-visit-worksheet worksheet)))

;; * Un/register Workdir

;;;###autoload
(defun workdir-register (worksheet)
  "Add WORKSHEET to the org agenda list"
  (interactive (list (workdir-guess-or-prompt-worksheet "Add worksheet to org agenda list: ")))
  (with-current-buffer (find-file-noselect worksheet)
    (when (eq major-mode 'org-mode)
      (org-agenda-file-to-front))))

(defun workdir-register-list (worksheets)
  "Add a list of WORKSHEETS to the org agenda list."
  (let* (;; this is copied from `org-agenda-file-to-front'
	 (file-alist (mapcar (lambda (f)
			       (cons (file-truename f) f))
			     (org-agenda-files t)))
	 (sheets-alist (mapcar (lambda (f)
				 (cons (file-truename f) f))
			       worksheets))
	 ;; cl-union might destroy the sequence order,
	 ;; but we don't care:
	 (new-list (cl-union file-alist sheets-alist
			     :key #'car
			     :test #'string=)))
    (org-store-new-agenda-file-list (mapcar #'cdr new-list))
    (org-install-agenda-files-menu)))

;;;###autoload
(defun workdir-register-all-worksheets ()
  "Register all current worksheets as org agenda files."
  (interactive)
  (let* ((file-list (org-agenda-files t))
	 (sheets    (workdir-get-worksheets))
	 (diff      nil))
    (unless sheets
      (user-error "No worksheets to register"))
    (workdir-register-list sheets)
    (message "Merged %d files into the agenda file list; effectively added %d files."
	     (length sheets)
	     (- (length (org-agenda-files t)) (length file-list)))))

;;;###autoload
(defun workdir-unregister (worksheet)
  "Remove WORKSHEET from the org agenda list."
  (interactive (list (workdir-guess-or-prompt-worksheet "Remove worksheet from org agenda list: ")))
  (when (org-agenda-file-p worksheet) (org-remove-file worksheet))
  (message "Worksheet '%s' is not in the agenda list anymore." worksheet))

;; * Delete Workdir

;;;###autoload
(defun workdir-delete (worksheet &optional unconditionally)
  "Delete the whole directory containing WORKSHEET.
If wanted, do it UNCONDITIONALLY (no questions asked)."
  (interactive (list (workdir-prompt-for-worksheet (workdir-get-worksheets) "Delete workdir: ")))
  (let* ((files      (directory-files (file-name-directory worksheet) nil directory-files-no-dot-files-regexp t))
	 (dir-name   (abbreviate-file-name (file-name-directory worksheet))))
    (if (not (workdir-kill-buffers worksheet unconditionally))
	(user-error "Could not kill all buffers belonging to the project; canceled")
      (if (or unconditionally
	      (y-or-n-p (format "Directory '%s' contains %d files. Delete? " dir-name (length files))))
	  (progn
	    (workdir-unregister worksheet)
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
  (when-let* ((file-name  (workdir-guess-file-name)))
    (locate-dominating-file (file-name-directory file-name)
			    workdir-default-sheet)))
	      

(defun workdir-get-worksheet (workdir)
  "Return the worksheet associated with WORKDIR."
  (when workdir
    (when-let ((match  (assoc-string (file-name-as-directory workdir)
				     (seq-map (lambda (s) (cons (file-name-directory s) s))
					      (workdir-get-worksheets)))))
      (cdr match))))

(defun workdir-guess-or-prompt-visiting-workdir (prompt)
  "Guess current buffer's workdir, or PROMPT user to select a currently visited worksheet."
  (or (workdir-guess-workdir)
      (when-let ((worksheet (workdir-prompt-for-worksheet
			     (seq-filter #'find-buffer-visiting (workdir-get-worksheets))
			     prompt)))
	(file-name-directory worksheet))))

(defun workdir-guess-or-prompt-worksheet (prompt)
  "Guess current worksheet or PROMPT user to select one."
  (if-let ((dir (workdir-guess-workdir)))
      (buffer-file-name)
    (workdir-prompt-for-worksheet (workdir-get-worksheets) prompt)))

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
  (interactive (list (workdir-guess-or-prompt-visiting-workdir " Save and kill all buffers belonging to workdir: ")))
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
Kill all open buffers before archiving."
  (interactive (let* ((interactive-worksheet (workdir-prompt-for-worksheet (workdir-get-worksheets) " Select workdir to move: "))
		      (interactive-target    (read-directory-name
					      (format "Move '%s' to: " (workdir-abbreviate-path interactive-worksheet))
					      (file-name-as-directory workdir-archive-directory) nil t)))
		 (list interactive-worksheet interactive-target)))
  (unless (workdir-kill-buffers worksheet)
    (user-error "There are still buffers visiting files; could not archive workdir"))
  (let* ((from (file-name-directory worksheet))
	 (to   (file-name-directory target)))
    (workdir-unregister worksheet)
    (rename-file from to)
    (message "Moved %s to %s." from to)))


;;;###autoload
(defun workdir-go-to-root ()
  "Go to the worksheet of the current workdir.
Set the mark before switching to the file."
  (interactive)
  (if-let ((target-dir (workdir-guess-workdir))
	   (target-sheet (concat (file-name-as-directory target-dir) workdir-default-sheet)))
    (if (file-readable-p target-sheet)
	(workdir-visit-worksheet target-sheet)
      (message "Could not find work sheet file, opening work directory instead.")
      (dired target-dir))
    (user-error "No workdir project associated with current buffer")))

;;;###autoload
(defun workdir-dired-root ()
  "Open root directory of current workdir in dired."
  (interactive)
  (dired (workdir-guess-workdir)))

;; * Hydra

(defcustom workdir-counsel-find-file-initial-input "\\(org\\|pdf\\)$ "
  "Initial input when using counsel to jump to a project file."
  :group 'workdir
  :type 'regexp)

(defun workdir-counsel-find-project-file (&optional no-initial-input)
  "Find file within project. Initial input defaults to
`workdir-counsel-find-file-initial-input'. If called with prefix,
do not set any initial input."
  (interactive "P")
  (unless (require 'counsel nil t)
    (user-error "workdir-find-project-file: library `counsel' required"))
  (counsel-file-jump (unless no-initial-input workdir-counsel-find-file-initial-input)
		     (workdir-guess-workdir)))

(defhydra workdir-hydra (:color blue :hint none)
    "
[_s_]elect or create workdir                   [_i_]buffer
[_+_] add file to org agenda file list         [_f_]ind file in workdir
[_-_] remove from agenda file list             [_r_]oot file
                                             [_\\^_] dired root directory

[_d_]elete workdir                             [_k_] save workdir files and kill its buffers
[_a_]rchive workdir                            [_v_]isit workdir
"
    ("s" workdir-visit-or-create-worksheet)
    ("v" workdir-visit-worksheet)
    ("i" workdir-ibuffer )
    ("a" workdir-archive)
    ("f" workdir-counsel-find-project-file )
    ("d" workdir-delete)
    ("+" workdir-register)
    ("-" workdir-unregister)
    ("r" workdir-go-to-root)
    ("^" workdir-dired-root)
    ("k" workdir-save-and-kill-buffers))

(provide 'workdir)
;;; workdir.el ends here
