# Workdir - a lightweight project management

## What it is

Workdir is a not-so-intelligent alternative to `projectile` and other
project management tools for Emacs. It was written because I felt
projectile to be too big and too much centered on the need of
programmers.

## Principle

A 'workdir' is simply a directory with at least one single file, which
is called a *work sheet*. The main idea is that workdir keeps a list
of work sheets and allows you to switch to them quickly. Switching to
a work dir means visiting the associated work sheet. Workdir provides
some further convenience functions to register, visit, archive or
unregister these work dirs.

A work sheet can be any file you want. Worksheet defines a default
name, though, which will be used when creating a new work sheet.  

The idea is that you use work sheets to write down your project
related notes and tasks, so that you have a main entry point.

The list of work sheets is persistently stored in a data base file.
Every change is immediately reflected in this data base.

## Features

 * Interactively select, create, move and delete workdirs
 * Interactively add the currently visited file as a work sheet
 * Open ibuffer with all files belonging to the currently visited work
   sheet
 * Save and kill all buffers belonging to the currently visited work
   sheet (kind of "cleaning up")
 * If work sheet is an org mode file, open it with a sparse todo tree
   (customizable via hook)
 * Populate the data base automatically by finding all work sheets
   'within' a given directory (only via elisp)

## Requirements

Workdir depends on the following packages:

* `reader-db`
 * `seq`

You can find `reader-db` in my repository.

## Minimal Installation 

Workdir does not define any key binding by itself. Write a hydra.
Alternatively, use the following minimal configuration:

```
(use-package workdir
	:bind
	(:map global-map
		("C-x p s" workdir-select-or-create-worksheet)
		("C-x p i" workdir-ibuffer)
		("C-x p d" workdir-delete)
		("C-x p a" workdir-archive)
		("C-x p +" workdir-add)
		("C-x p 0" workdir-save-and-kill-buffers)))
```

## Customization

You can customize the following variables:

 * `workdir-dirlist-file-name` -- File name  for the data base. Will
   be located in the user directory (i.e. `.emacs.d`)
   
 * `workdir-default-sheet` -- Name used when creating new work sheets.
   Currently defaults to `konzeptblatt.org`.
   
 * `workdir-archive-directory` -- Default directory to move
   ('archive') workdirs to.
   
 * `workdir-new-dirs-directory` -- Default directory for creating new work
   dirs.

 * `workdir-visit-worksheet-hook` -- Hook with a list of functions
   which are called when visiting a work sheet the first time. The
   functions are called with the work sheet file as the current buffer
   and should not accept any argument.
   
