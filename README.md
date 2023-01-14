# emacs.d

_Vanilla, discoverable Emacs configuration_

This is my Emacs configuration. It's simple, (mostly) declarative, and discoverable. It requires **Emacs 28+**.

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
* `vertical-fido-mode` for improved completion & discoverability
* `recentf-mode` to reopen past edits
* [which-key](https://github.com/justbur/emacs-which-key) for discoverability
* [dracula](https://github.com/dracula/emacs) theme for a similar colour scheme to common GNOME dark mode themes

# Cheat Sheet

Entries with _emphasis_ are custom bindings 

- `Menu` Enter command
- `M-Menu` _Opens the global menu_
- `C-Menu o` _Opens the "recent files" list_
- `C-Menu e` _Edit the "recent files" list_
- `C-Menu c` _Clear the "recent files" list_
- `C-z, C-/, C-_, C-x u` Undo
- `C-?` Redo
- `C-x h` Select all
- `C-RET` Block selection (CUA)
- `C-SPACE` Block selection (Classic)
- `C-u <N> C-DEL` Delete N words forward
- `C-u <N> C-BCKSP` Delete N word backward
- `C-u <N> M-@` `C-u <N> C-S-Right` Mark N words forward
- `C-u <N> C-S-Left` Mark N words backward
- `C-SPC C-s <STR>` Mark until string
- `C-SPC C-s <STR>` Mark until string backwards
- `C-,` _Join lines_
- `C-. C-.` _Simple autocomplete_
- `C-. C-SPACE` _Show possible completions_
- `C-#` _Smart autocompletion_ 
- `C-x C-x (rapidly) r s` `C-S-x` Copy to register
- `C-x C-x (rapidly) r t` `C-S-x r t` Insert text before rectangle

