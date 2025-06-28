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

# Features

* `desktop-save-mode` for simple session persistence
* `undo-redo` for traditional undo behavior (requires Emacs 28+)
* `whitespace-mode` to visualize trailing whitespace & tab indentations
* `vertical-fido-mode` for improved completion & discoverability
* `recentf-mode` to reopen past edits

# Custom keybinds

Use `M-x describe-personal-keybindings` to show custom keybindings. All custom keybinds are registered via `use-package'.
