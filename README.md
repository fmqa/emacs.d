# emacs.d

![Screenshot](.assets/screenshot.png?raw=true "This is how it looks")

_Vanilla, discoverable Emacs configuration_

This is my Emacs configuration. It's simple, (mostly) declarative, and discoverable. It requires **Emacs 29+**.

# Principles

* _Configure declaratively_: Prefer `easy-customize` over other configuration methods
* _Keep it simple_: Apply KISS; The `init.el` should be simple enough to be read by a novice
* _Keep the overhead low_: Install only the neccessary 3rd-party packages
* _Use the included batteries_: The standard Emacs packages are tested & curated by the Emacs community, use them!
* _Retain the defaults as much as possible_: We try to keep the default keybinds as much as possible and avoid rebinding everything

# Requirements

* Emacs 29+
* A keyboard with a menu key (≣) (also sometimes called "application key"). C-≣ is used as a prefix key since it is usually not bound to anything. On a Mac keyboard, this config should be edited and the key reassigned to another key e.g. Command (⌘).

# Features

_Links_ denote external packages

* `desktop-save-mode` for simple session persistence
* `undo-redo` for traditional undo behavior (requires Emacs 28+)
* `whitespace-mode` to visualize trailing whitespace & tab indentations
* `vertical-fido-mode` for improved completion & discoverability
* `recentf-mode` to reopen past edits

# Cheat Sheet

Entries with _emphasis_ are custom bindings

- General
  - `≣` Enter command
  - `C-≣` _Opens the global menu_
  - `C-x ≣ o` _Opens the "recent files" list_
  - `C-x ≣ e` _Edit the "recent files" list_
  - `C-x ≣ c` _Clear the "recent files" list_
  - `C-/, C-_, C-x u` Undo
  - `C-?` Redo
  - `C-x h` Select all
  - `C-SPACE` Block selection (Classic)
  - `C-u <N> C-DEL` Delete N words forward
  - `C-u <N> C-BCKSP` Delete N word backward
  - `C-u <N> M-@` `C-u <N> C-S-Right` Mark N words forward
  - `C-u <N> C-S-Left` Mark N words backward
  - `C-SPC C-s <STR>` Mark until string
  - `C-SPC C-s <STR>` Mark until string backwards
  - `M-^` _Join lines_
  - `C-x j` _Duplicate line_
  - `M-/` _Simple autocompletion_
  - `C-M-/` _Show possible completions_
  - `C-x C-/` _Smart autocompletion_
  - `C-x r s`  Copy to register
  - `C-x r t` Insert text before rectangle
  - `C-x r i` Insert register

