;;; test-workdir.el --- test suite for workdir.el    -*- lexical-binding: t; -*-


(add-to-list 'load-path "~/.emacs.d/lisp/packages/reader-db")
(require 'workdir)

;; -------------------------------------------------

(describe "Functions"
  (it "can be 'curried'"
    (expect (mapcar (workdir-curry #'+ 2) '(1 2 3))
	    :to-equal
	    '(3 4 5))
    (expect (mapcar (workdir-rcurry #'- 1) '(1 2 3))
	    :to-equal
	    '(0 1 2)))
  (it "can be 'composed'"
    (expect (mapcar (workdir-compose #'not #'not) '(t t))
	    :to-equal
	    '(t t))))

(describe "The data base"
  :var (db-file non-existing-file)
  ;;
  (before-all
    (setq db-file (make-temp-file "dbfile_"))
    (spy-on 'workdir-worksheet-database-file
	    :and-return-value
	    db-file))
  (after-all
    (delete-file db-file))
  ;;  
  (describe
    "has a test environment"
    (it "which provides a db file name"
      (expect (workdir-worksheet-database-file)
	      :to-equal
	      db-file))
    (it "where the data base refers to an existing file"
      (expect (file-exists-p db-file)
	      :to-be-truthy))
    (it "where the data base is a readable file"
      (expect (file-readable-p db-file)
	      :to-be-truthy))
    (it "where the data base is  a writeable file"
      (expect (file-writable-p db-file)
	      :to-be-truthy))
    (it "which is able to locally store effectively some unspecified data"
      (workdir-new-data-base)
      (workdir-write-worksheets '("test"))
      (expect (file-attribute-size (file-attributes db-file))
	      :to-be-greater-than
	      0))
    (it "will silently be recreated upon next write operation"
      (let (read-list
	    (a-file (make-temp-file "test_file_")))
	(delete-file (workdir-worksheet-database-file))
	(workdir-write-worksheets (list a-file))
	(setq read-list (workdir-read-worksheets))
	(delete-file a-file)
	(expect read-list
		:to-equal
		(list a-file)))))

  ;;
  (describe "returns stored file names"
    :var (temp-file)
    (before-all
      (setq temp-file (make-temp-file "write")))
    (after-all
      (delete-file temp-file))
    ;;
    (it "if these file exists"
      (workdir-write-worksheets (list temp-file))
      (expect (file-exists-p temp-file)
	      :to-be-truthy)
      (expect (workdir-read-worksheets)
	      :to-equal
	      (list temp-file)))
    (it "and filters out non-existing files"
      (setq non-existing-file (make-temp-file "non-existing"))
      (delete-file non-existing-file)
      (workdir-write-worksheets (list non-existing-file temp-file))
      (expect (workdir-read-worksheets)
	      :to-equal
	      (list temp-file))))
  ;;
  (describe "can be cleaned up manually"
    :var (temp-file non-existing-file)
    (before-all
      (setq temp-file (make-temp-file "duptest")))
    (after-all
      (delete-file temp-file))
    ;;
    (it "and removes not readable files"
      (setq non-existing-file (make-temp-file "non-existing"))
      (delete-file non-existing-file)
      (workdir-write-worksheets (list non-existing-file))
      (workdir-sanitize-data-base)
      (expect (workdir-read-worksheets)
	      :to-equal
	      nil))
    (it "and removes duplicate files"
	(workdir-write-worksheets (list temp-file temp-file))
	(workdir-sanitize-data-base)
	(expect (workdir-read-worksheets)
		:to-equal
		(list temp-file))))
  ;;
  (describe "can be populated manually"
    :var (temp-dir temp-file)
    (before-all
      (setq temp-dir    (file-name-as-directory (make-temp-file "poptest" t)))
      (setq temp-file   (concat temp-dir workdir-default-sheet)))
    (after-all
      (when temp-dir
	(delete-directory temp-dir t)))
    ;;
    (it "and works with a default file name"
      (expect workdir-default-sheet
	      :not :to-be nil)
      (expect (stringp workdir-default-sheet)
	      :to-be-truthy))
    (it "and finds work sheets within a dir"
      (with-temp-file temp-file)
      (workdir-populate-data-base temp-dir)
      (expect (workdir-read-worksheets)
	      :to-equal
	      (list temp-file)))))

;; -----------------------------------------------------------

(describe "Worksheets"
  ;;
  (before-all
    (setq db-file (make-temp-file "dbfile_"))
    (spy-on 'workdir-worksheet-database-file
	    :and-return-value
	    db-file))
  (after-all
    (delete-file db-file))
  ;;
  (it "will only be added once to the data base"
    (let ((a-file (make-temp-file "testfile_")))
      (delete-file (workdir-worksheet-database-file))
      (workdir-add-file a-file)
      (workdir-add-file a-file)
      (expect (workdir-read-worksheets)
	      :to-equal
	      (list a-file))))
  (it "can be removed from the data base"
    (let ((a-file (make-temp-file "testfile_")))
      (delete-file (workdir-worksheet-database-file))
      (workdir-add-file a-file)
      (workdir-remove-file a-file)
      (expect (workdir-read-worksheets)
	      :to-be
	      nil)))
  (it "can be removed by their base directory"
    (let ((a-file (make-temp-file "test-file-"))
	  (b-file (make-temp-file "test-file-")))
      (delete-file (workdir-worksheet-database-file))
      (workdir-add-file a-file)
      (workdir-add-file b-file)
      (workdir-remove-by-base-dir (file-name-directory a-file))
      (expect (workdir-read-worksheets)
	      :to-be
	      nil))))
	  
(describe "Prettyfing paths"
  (it "abbreviates the parent dir, if pointing to a default worksheet"
    (let ((a-file (concat
		   (file-name-as-directory user-emacs-directory)
		   (file-name-as-directory "a-parent-dir")
		   workdir-default-sheet)))
      (expect (workdir-abbreviate-path a-file)
	      :to-equal
	      (file-name-as-directory "a-parent-dir"))))
  (it "shows parent dir and file name, if not pointing to a default worksheet"
    (let ((a-file (concat
		   (file-name-as-directory user-emacs-directory)
		   (file-name-as-directory "a-parent-dir")
		   "test")))
      (expect (workdir-abbreviate-path a-file)
	      :to-equal
	      (concat
	       (file-name-as-directory "a-parent-dir")
	       "test")))))


    

    


;;; test-workdir.el ends here
