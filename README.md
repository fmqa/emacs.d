# emacs.d

_Vanilla, discoverable Emacs configuration_

This is my Emacs configuration. It's simple, mostly declarative, and discoverable. It requires **Emacs 28+**.

# Principles

* _Configure declaratively_: Prefer `easy-customize` over other configuration methods.
* _Keep it simple_: Apply KISS; The `init.el` should be simple enough to be read by a novice.
* _Keep the overhead low_: Install only the neccessary 3rd-party packages.
* _Use the included batteries_: The standard Emacs packages are tested & curated by the Emacs community, use them!

# Features

_Links_ denote external packages

* `cua-mode` enabled (less mental overhead)
* `desktop-save-mode` for simple session persistence
* `undo-redo` for traditional undo behavior (requires Emacs 28+)
* `whitespace-mode` to visualize trailing whitespace & tab indentations
* [Dracula Theme](https://github.com/dracula/emacs) for a similar colour scheme to common GNOME dark mode themes
* [which-key](https://github.com/justbur/emacs-which-key) for discoverability

# Cheat Sheet

- `Menu` Enter command
- `C-Menu` Opens the global menu
- `C-z, C-/, C-_, C-x u` Undo
- `C-?` Redo
- `C-x h` Select all
- `C-RET` Block selection (CUA)
- `C-SPACE` Block selection (Classic)
- `C-u <N> C-DEL` Delete N words forward
- `C-u <N> C-BCKSP` Delete N word backward
- `C-u <N> M-@`  Mark N words
- `C-SPC C-s <STR>` Mark until string
- `C-SPC C-s <STR>` Mark until string backwards
- `C-x C-x (rapidly) r s` `C-S-x` Copy to register
- `C-x C-x (rapidly) r t` `C-S-x r t` Insert text before rectangle

