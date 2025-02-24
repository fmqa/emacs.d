# emacs.d

![Screenshot](.assets/screenshot.png?raw=true "This is how it looks")

_Vanilla, discoverable Emacs configuration_

This is my Emacs configuration. It's simple, declarative, and discoverable. It requires **Emacs 30+**.

# Principles

* _Configure declaratively_: Prefer `use-package` and `easy-customize` over other configuration methods
* _Keep it simple_: Apply KISS; The `init.el` should be simple enough to be read by a novice
* _Keep the overhead low_: Install only the neccessary 3rd-party packages
  * `markdown-mode` is installed for pretty-printed `eglot` documentation, and for markdown formatting.
* _Use the included batteries_: The standard Emacs packages are tested & curated by the Emacs community, use them!
* _Retain the defaults as much as possible_: We try to keep the default keybinds as much as possible and avoid rebinding everything

# Requirements

* Emacs 30+
* A keyboard with a menu key (≣) (also sometimes called "application key"). C-≣ is used as a prefix key since it is usually not bound to anything. On a Mac keyboard, this config should be edited and the key reassigned to another key e.g. Command (⌘).

# Features

* `desktop-save-mode` for simple session persistence
* `undo-redo` for traditional undo behavior (requires Emacs 28+)
* `whitespace-mode` to visualize trailing whitespace & tab indentations
* `vertical-fido-mode` for improved completion & discoverability
* `recentf-mode` to reopen past edits

# Custom keybinds

The following key bindings are registered by this configuration. These can be also shown inside Emacs using `M-x describe-personal-keybindings`:

* `C-x ≣` or `C-x <menu>` is used a prefix key for recentf
  * `C-x <menu> o` Open recentf menu (`recentf-open-files`)
  * `C-x <menu> e` Edit recentf list (`recentf-edit-list`)
  * `C-x <menu> c` Cleanup recentf list (`recentf-cleanup`)
* `C-x j` duplicates a line via `duplicate-dwim`
* `C-x C-/` completes via `hippie-expand`
* `C-x w` is set to the windmove prefix
  * `C-x w <up>` `C-x w <down>` `C-x w <left>` `C-x w <right>` moves to the next window in the specified direction
* `C-x w` also contains additional `windmove.el` keybindings
  * `C-x w C-/` `C-x w C-_` and `C-x w C-_` are bound to `winner-undo`
  * `C-x w C-?` and `C-x w C-?` are bound to `winner-redo`

