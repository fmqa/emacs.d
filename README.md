# emacs.d

_Vanilla, vim-flavoured Emacs configuration_

This is my Emacs configuration. It's simple, mostly declarative, and discoverable. It requires **Emacs 28+**.

# Principles

* _Use the included batteries_: The standard Emacs packages are tested & curated by the Emacs community, use them!
* _Keep the overhead low_: Install only the neccessary 3rd-party packages.
* _Configure declaratively_: Prefer `easy-customize` over other configuration methods.
* _Keep it simple_: Apply KISS; The `init.el` should be simple enough to be read by a novice.

# Features

* [Dracula Theme](https://github.com/dracula/emacs) for a similar colour scheme to common GNOME dark mode themes
* `cua-mode` enabled (less mental overhead)
* `desktop-save-mode` for simple session persistence
* `undo-redo` for traditional undo behavior (requires Emacs 28+)
* `whitespace-mode` to visualize trailing whitespace & tab indentations
* [evil-mode](https://github.com/emacs-evil/evil) for modal editing
* [which-key](https://github.com/justbur/emacs-which-key) for discoverability

# Cheat Sheet

- `C-Menu` opens the global menu
- `C-z` Switch to Emacs mode
- `C-/, C-_, C-x u` Undo (In Emacs mode, as an alternative to C-z)
- `C-?` Redo (In Emacs mode)

