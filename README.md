# Workdir - a lightweight project management

## What it is

Workdir is a not-so-intelligent alternative to `projectile` and other
project management tools for Emacs. It was written because I felt
projectile to be too big, too complicated, and centered too much on
the need of programmers. In my daily work, I usually do not need any
version control for my projects, and I love to use org files to
organize them (i.e., to plan actions and to take project-related
notes). Thus, I got weary of always having to explicitly visit this
organizing file -- and built some tools to automate this.

## How it works

The whole package is grouped around the notion of a **workdir**. A
workdir is simply a directory with at least one single file, which is
called the *work sheet*. The package allows you to access and create a
persistent list of these work sheets (each in its own directory). In
particular, it allows you to switch around between them quickly.

The workdir thus represents a project, and the **work sheet** serves
as the permant access point to the project. The idea is that you use
the work sheet to organize your project. In the work sheet, you store
all notes, tasks and other relevant stuff for your project. In
particular, if the work sheet is an org mode file, you can store TODO
items which will be presented to you as an org mode sparse tree upon
your first visit of that work sheet.

## Features

 * Interactively select, create, move and delete workdirs.
 * Interactively add the currently visited file as a work sheet.
 * Open `ibuffer` with all files belonging to the currently visited work
   sheet.
 * Save and kill all buffers belonging to the currently visited work
   sheet (kind of "cleaning up").
 * If the work sheet is an org mode file, open it with a sparse todo
   tree (action is customizable via hook).
 * Populate the data base automatically by finding all work sheets
   within a given directory (no interactive function, only via elisp).

## Dependencies

Workdir depends on the following packages:

* `reader-db`
 * `seq`

You can find `reader-db` in [my repository](https://github.com/publicimageltd/reader-db "Link to repository 'reader-db'").

# Setup

## Minimal setup

Minimally, you have to set the variables `workdir-archive-directory`
and `workdir-new-dirs-directory`.  I.e.:

``` emacs-lisp
(use-package workdir
   :config
   (setq workdir-archive-directory  "~/Documents/archive")
   (setq workdir-new-dirs-directory "~/Documents/projects"))
```

## Keybindings

Workdir does not define any key binding by itself. I myself use a
hydra. The hydra uses another function, which is not (yet) part of the
package, `workdir-counsel-find-project-file`. This function offers you
all files (recursive search) of the project with a certain suffix (in
the case below, ending with .org or .pdf).

``` emacs-lisp
;; The additional function to find a project file:
  (defcustom workdir-counsel-find-file-initial-input "\\(org\\|pdf\\)$ "
    "Initial input when using counsel to jump to a project file.")
	
  (defun workdir-counsel-find-project-file (&optional no-initial-input)
    "Find file within the whole project, including subdirs.
Initial input defaults to `workdir-counsel-find-file-initial-input'. 
If called with prefix, do not set any initial input."
    (interactive "P")
    (unless (require 'counsel nil t)
      (user-error "workdir-find-project-file: library `counsel' required"))
    (counsel-file-jump (unless no-initial-input workdir-counsel-find-file-initial-input)
		       (workdir-guess-workdir)))
		
;; The hydra:
  (defhydra workdir-hydra (:color blue :hint none)
    "
[_v_]isit or create workdir                    [_i_]buffer                     
[_+_] add current file as worksheet            [_f_]ind file in workdir        
[_u_]nregister current file                    [_r_]oot file
                                             [_^_] dired root directory
                        
[_d_]elete workdir                             [_k_] save workdir files and kill its buffers
[_a_]rchive workdir
"
    ("v" workdir-visit-or-create-worksheet )
    ("i" workdir-ibuffer )
    ("a" workdir-archive )
    ("f" workdir-counsel-find-project-file )
    ("d" workdir-delete)
    ("u" workdir-unregister)
    ("r" workdir-go-to-root)
    ("^" workdir-dired-root)
    ("k" workdir-save-and-kill-buffers )
    ("+" workdir-add))
```

Alternatively, use the following minimal configuration:

``` emacs-lisp
(use-package workdir
	:bind
	(:map global-map
		("C-x p s" workdir-visit-or-create-worksheet)
		("C-x p i" workdir-ibuffer)
		("C-x p d" workdir-delete)
		("C-x p a" workdir-archive)
		("C-x p +" workdir-add)
		("C-x p 0" workdir-save-and-kill-buffers)))
```

# Customization

You can customize the following variables:

<dl>
<dt>workdir-database-name</dt>
<dd>File name for the data base. Will be
located in the user directory (i.e. `.emacs.d`)</dd>
<dt>workdir-default-sheet</dt>
<dd>Name used when creating new work sheets.
Currently defaults to `konzeptblatt.org`. This is german and means
	'a sheet of paper for conceptual stuff'.</dd>
<dt>workdir-archive-directory</dt>
<dd> Default directory for archiving  workdirs.</dd>
<dt> workdir-new-dirs-directory</dt>
   <dd> Default directory when creating new</dd>
	   <dt>workdir-visit-worksheet-hook</dt>
	<dd>Hook with a list of functions
which are called when visiting a work sheet the first time. The
functions are called with the work sheet file as the current buffer
and should not accept any argument.</dd>
</dl>

