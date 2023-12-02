# Workdir - a lightweight project management

## What it is

Workdir is a not-so-intelligent alternative to `projectile` and other
project management tools for Emacs. It was written because I felt
`projectile` to be too big, too complicated to configure for my needs,
and centered too much on the need of programmers. E.g., I do not need
to put my project directories under version control. Furthermore, I
organize each project with a separate `org` file, and I want this file
to show up in the agenda list. Plus, I wanted to be able to create a
new workdir quick and easy, and to move it out of the way just as
quick.

So `workdir` might be of use...

 - ...if you work with changing (possibly small) org-based projects,
   and you want to be able to navigate quickly between them; and:
 - ...if you like to organize these projects with the help of an org
   file in which you list all the project-related todo items and other
   stuff, and these items should show up in the org agenda.

## How it works

The whole package is grouped around the notion of a **workdir**. A
workdir is simply a directory containing at least one single file,
which is called the *worksheet*. The package allows you to access
these workdirs and to navigate quickly to their respective worksheets.

A workdir thus represents a project, and the **worksheet** serves as a
permant access point to this project. The idea is that you use the
worksheet to organize the project. In the worksheet, you store all
notes, tasks and other relevant organizational stuff for your project.
In the workdir, you store all the files which are interesting for your
project.

It is recommended to use an org mode file as a worksheet. Then you can
store TODO items which will be presented to you as an org mode sparse
tree upon your first visit of that work sheet.

## Features

 * Select, create, move and delete workdirs.
 * 'Archive' a workdir by moving it to a predefined archive directory. 
 * Quickly jump to the worksheet file from any file within the workdir. 
 * Add the currently visited file to the org mode agenda ('register'
   the file), or remove it. This is useful to fine tune what is shown
   in the agenda.
 * Open `ibuffer` with all files belonging to the current workdir.
 * Save and kill all buffers belonging to the current workdir (kind of
   "cleaning up" the buffer space).
 * If the work sheet is an org mode file, open it with a sparse todo
   tree (not per default; action is customizable via hook).
* All workdirs will be recognized by `project.el`, so that you can use
  all `project-` functions.

## Dependencies

`Workdir` requires emacs >= 26.1 and the package `hydra`.

For some weird reasons, flycheck keeps telling me that `hydra` is not
installable. I don't know why that should be. If you have any idea how
to correct this, please file an issue.

`Workdir` uses `find` and `awk` to find the workdirs and retrieve the
titles from the org documents (worksheets). On non-free operating
systems, an alternative lisp implementation is used which is
unfortunately a tiny bit slower.

# Setup

## Minimal setup

Minimally, you have to set three variables:

 * `workdir-directories` - A list of directory names in which to look
   for workdirs. Instead of a directory name, you can also use a
   path to a file name which will then be interpreted as a direct
   pointer to a work sheet. This way, you can add a single workdir
   without being forced to add all workdirs within its parent
   directory. Note that the file name has to be the same
   as the one defined in `workdir-default-sheet` (default:
   `konzeptblatt.org`).
 * `workdir-archive-directory` - A single directory name which is used
   for 'archiving' work dirs.
 * `workdir-new-dirs-directory` - A directory in which new work dirs
   will be created. 

``` emacs-lisp
(use-package workdir
   :config
   (setq workdir-directories '("~/Documents/projects" "~/.emacs.d/konzeptblatt.org"))
   (setq workdir-archive-directory  "~/Documents/archive")
   (setq workdir-new-dirs-directory "~/Documents/projects"))
```

See the section "Variables" below for a more detailed listing of all
customizable variables.

## Common use cases 

All interactive functions try to recognize the "current" work tree by
guessing which workdir the currently visited file belongs to. This
mechanism also works when visiting a directory via `dired`.

The most used function will probably be
`workdir-visit-or-create-worksheet`. This function presents a list of
all current worksheets. The user can either switch to one of those or
enter a non-matching name to create a new workdir.

The second most used function (at least for me) is `workdir-archive`.
When a project is finished, select any workdir and move it to a
predefined directory. The operation will be canceled if there are any
unsaved files in the directory to be moved. Use
`workdir-save-and-kill-buffers` to safe all buffers belonging to the
current worktree.

To find a file in one of the archived workdirs, call
`workdir-visit-worksheet` with a prefix. You will be offered a list of
all known directories in which you might find workdirs.

## Keybindings

Workdir does not define any key binding by itself. It offers a hydra,
however. Bind any key you like to `workdir-hydra/body` in order to use
the hydra. 

``` emacs-lisp
(use-package workdir
 :config
  (setq workdir-directories           '("~/Documents/projects" "~/.emacs.d/konzeptblatt.org"))
  (setq workdir-archive-directory   "~/Dokumente/archive")
  (setq workdir-new-dirs-directory  "~/Dokumente/projects")
  :bind
  (:map global-map
	("C-x p" . workdir-hydra/body)))
```

Alternatively, you could adapt the following bindings:

``` emacs-lisp
(use-package workdir
	:bind
	(:map global-map
		("C-x p s" . workdir-visit-or-create-worksheet)
		("C-x p r" . workdir-go-to-root)
		("C-x p i"  . workdir-ibuffer)
		("C-x p d" . workdir-delete)
		("C-x p a" . workdir-archive)
		("C-x p +" . workdir-register)
		("C-x p -"  . workdir-unregister)
		("C-x p k" . workdir-save-and-kill-buffers)))
```

# Variables

You can customize the following variables:

<dl>
<dt>workdir-default-sheet</dt>
<dd>Name used when creating new work sheets.
Currently defaults to `konzeptblatt.org`. This is german and means
	'a sheet of paper for conceptual stuff'.</dd>

<dt>workdir-directories</dt>
<dd> List of directories which might contain workdirs. Each element is
either a directory path or the direct path to a worksheet. Note that a
direct path to a worksheet is compared against
`workdir-default-sheet`, so do not forget to update this variable if
you choose another default directory name.</dd>

<dt>workdir-archive-directory</dt>
<dd> Default directory for archiving  workdirs.</dd>

<dt> workdir-new-dirs-directory</dt>
<dd> Default directory when creating new workdirs</dd>

<dt> workdir-use-find-binary</dt>
<dd> Use `find` to determine the workdirs; else use a (slower)
alternative using elisp.</dd>

<dt> workdir-use-awk-binary</dt> <dd> Use `awk` to determine the
titles of the worksheets; else use a (slower) alternative using
elisp.</dd>

  <dt>workdir-visit-worksheet-hook</dt>  
 <dd>Hook with a list of functions which are called when visiting a
work sheet the first time. The functions are called with the work
sheet file as the current buffer and should not accept any
argument.</dd> 

</dl>

# Changelog

## Current

 - Move default branch to main
 - Improve (and slow down) completion when not using `awk`
 - Some clean up of the code.

## 0.3.

Partial rewrite. The older versions used a persistent data base. This
dependency has been removed now, there is no data base any more. The
list of available workdirs is constructed anew each time the program
prompts for a workdir.

## 0.2

First release you should be able to depend upon. Works like a charm
for quite some time by now.
