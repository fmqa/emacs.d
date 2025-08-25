;; -*- lexical-binding: t; indent-tabs-mode: nil -*-

(use-package emacs
  ;; Basic editing conveniences.
  :config
  (put 'narrow-to-region 'disabled nil)
  :custom
  (menu-bar-mode nil)
  (tool-bar-mode nil)
  (use-short-answers t)
  (ring-bell-function 'ignore)
  (enable-recursive-minibuffers t)
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (initial-major-mode 'text-mode)
  (tab-width 4))

(use-package cus-edit
  ;; Use separate file for customization variables.
  :config
  (setopt custom-file (file-name-concat user-emacs-directory "custom.el"))
  (load custom-file t))

(use-package package
  ;; Configure package sources (MELPA, Devel ELPA).
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/") t)
  :custom
  (package-archive-priorities '(("gnu" . 10)
                                ("nongnu" . 10))))

(use-package simple
  ;; More basic editing conveniences.
  :bind (("M-u" . upcase-dwim)
         ("M-l" . downcase-dwim)
         ("M-c" . capitalize-dwim))
  :custom
  (undo-no-redo t)
  (column-number-mode t)
  (completion-auto-select 'second-tab)
  (mail-user-agent 'gnus-user-agent))

(use-package dnd
  ;; Make Drag-and-Drop behave like in other programs.
  :defer t
  :custom
  (dnd-indicate-insertion-point t))

(use-package repeat
  ;; Repeat mode.
  :defer t
  :custom
  (repeat-mode t)
  (repeat-exit-key "<return>"))

(use-package delsel
  ;; Delete selection on edit.
  :custom
  (delete-selection-mode t))

(use-package pixel-scroll
  ;; Smooth scrolling.
  :disabled ;; Due to display issues on 30.1
  :custom
  (pixel-scroll-precision-mode t))

(use-package ibuffer
  ;; Prefer ibuffer for buffer management.
  :defer t
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-save-with-custom nil))

(use-package project
  ;; project.el stuff.
  :defer t
  :custom
  (project-buffers-viewer 'project-list-buffers-ibuffer))

(use-package mb-depth
  ;; Show minibuffer depth.
  :defer t
  :custom
  (minibuffer-depth-indicate-mode t))

(use-package isearch
  ;; Incremental search.
  :defer t
  :custom
  (isearch-lazy-count t))

(use-package tab-bar
  ;; Better-looking tab groups + improved winner-mode integration.
  :preface
  (defun dwim-toggle-tab-bar-history-mode ()
    "Enable tab-bar-history-mode if needed, otherwise disable it."
    (tab-bar-history-mode (if tab-bar-mode +1 -1)))
  (defun dwim-toggle-winner-mode ()
    "Enable winner-mode if needed, otherwise disable it."
    (winner-mode (if tab-bar-history-mode -1 +1)))
  :defer t
  ;; tab-bar-history-mode should be preferred to winner-mode when tab-bar-mode
  ;; is enabled
  :hook ((tab-bar-mode . dwim-toggle-tab-bar-history-mode)
         (tab-bar-history-mode . dwim-toggle-winner-mode))
  :custom
  (tab-bar-format '(tab-bar-format-history tab-bar-format-tabs-groups tab-bar-separator tab-bar-format-add-tab)))

(use-package whitespace
  ;; Highlight whitespace characters.
  :defer t
  :custom
  (whitespace-display-mappings
   '((tab-mark ?\t [10095 ?\t] [62 ?\t])
     (space-mark 32 [183] [46])
     (space-mark 160 [8942] [95])
     (newline-mark ?\n [172 ?\n] [36 ?\n])
     (newline-mark ?\r [182] [35])))
  (whitespace-style
   '(tab-mark
     face trailing tabs newline missing-newline-at-eof
     space-after-tab space-before-tab))
  :hook (prog-mode conf-mode text-mode))

(use-package desktop
  ;; Explicit, non-intrusive session management.
  :defer t
  :bind (("C-x <f10> d s" . desktop-save)
         ("C-x <f10> d r" . desktop-read)
         ("C-x <f10> d d" . desktop-remove)
         ("C-x <f10> d c" . desktop-clear)
         ("C-x <f10> d v" . desktop-revert))
  :custom
  (desktop-save-mode t)
  (desktop-load-locked-desktop nil)
  (desktop-save nil))

(use-package icomplete
  ;; Minibuffer completions.
  :defer t
  :custom
  (fido-vertical-mode t))

(use-package outline
  ;; Outlines.
  :defer t
  :custom
  (outline-minor-mode-use-buttons 'in-margins))

(use-package completion-preview
  ;; Completion previews (for suitable modes).
  :defer t
  :hook (prog-mode conf-mode inferior-emacs-lisp-mode eshell-mode shell-mode))

(use-package dired
  ;; Better dired defaults.
  :defer t
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-dwim-target t))

(use-package autorevert
  ;; Automatically refresh modified file buffers.
  :defer t
  :custom
  (global-auto-revert-mode t)
  (auto-revert-avoid-polling t)
  (auto-revert-check-vc-info t))

(use-package misc
  ;; Bind duplicate-dwim (from mickeyp's "Mastering Emacs").
  :defer t
  :bind ("C-x j" . duplicate-dwim))

(use-package recentf
  ;; Accessible recentf.
  :defer t
  :bind (("C-x <f10> o" . recentf-open-files)
         ("C-x <f10> e" . recentf-edit-list)
         ("C-x <f10> c" . recentf-cleanup))
  :custom
  (recentf-mode t))

(use-package display-line-numbers-mode
  ;; Line numbers.
  :defer t
  :hook (text-mode prog-mode conf-mode))

(use-package goto-addr
  ;; Hyperlink buttonization.
  :defer t
  :hook ((compilation-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)
         (conf-mode . goto-address-mode)
         (text-mode . goto-address-mode)
         (eshell-mode . goto-address-mode)
         (shell-mode . goto-address-mode)))

(use-package elec-pair
  ;; Electric pairs
  :defer t
  :hook ((prog-mode conf-mode) . electric-pair-local-mode))

(use-package hippie-expand
  ;; Accessible hippie-expand.
  :defer t
  :bind ("C-x C-/" . hippie-expand)
  (:repeat-map hippie-expand-repeat-map
               ("C-/" . hippie-expand)))

(use-package window
  ;; Windowing rules.
  :preface
  (defun shell-command-buffer-name-condition (name action)
    (or (equal name shell-command-buffer-name)
        (equal name shell-command-buffer-name-async)))
  :config
  ;; Reuse existing shell output windows, even those on other frames
  (add-to-list 'display-buffer-alist '(shell-command-buffer-name-condition display-buffer-reuse-window (reusable-frames . t))))

(use-package windmove
  ;; Set windmove prefix to C-x w + add a repeat map.
  :config
  (defvar-keymap windmove-repeat-map
    :repeat t
    "<up>"    #'windmove-up
    "<down>"  #'windmove-down
    "<left>"  #'windmove-left
    "<right>" #'windmove-right)
  :custom
  (windmove-default-keybindings '([24 119]))
  (windmove-swap-states-default-keybindings '([24 119] shift))
  (windmove-display-default-keybindings '([24 119] meta)))

(use-package winner
  ;; Window management.
  :defer t
  :custom
  (winner-mode t))

(use-package diff-mode
  ;; Diff display.
  :defer t
  :custom
  ;; Fix non-deterministic order of font-lock overlays
  (diff-font-lock-syntax nil))

(use-package prog-mode
  ;; Prettify checkboxes in org-mode.
  :preface
  (defun prettify-org-symbols ()
    "Prettify checkboxes in org-mode."
    (setf (alist-get "[ ]" prettify-symbols-alist nil nil 'equal) "☐")
    (setf (alist-get "[X]" prettify-symbols-alist nil nil 'equal) "☑")
    (setf (alist-get "[-]" prettify-symbols-alist nil nil 'equal) "⊟")
    (prettify-symbols-mode 1))
  :defer t
  :custom
  (prettify-symbols-unprettify-at-point 'right-edge)
  :hook ((prog-mode . prettify-symbols-mode)
         (org-mode . prettify-org-symbols)))

(use-package org-keys
  ;; Org keybindings.
  :defer t
  :custom
  (org-replace-disputed-keys t))

(use-package erc
  ;; IRC client.
  :defer t
  :ensure t
  :pin gnu-devel ;; Use the Erc snapshot for latest features & fixes.
  :config
  (setopt erc-modules
          (seq-union '(bufbar nicks notifications scrolltobottom services irc-format-normalize)
                     erc-modules))
  :custom
  (erc-interactive-display 'buffer)
  (erc-receive-query-display 'bury)
  (erc-server-reconnect-function 'erc-server-delayed-check-reconnect)
  (erc-prompt "\u27a4"))

(use-package erc-match
  ;; Better fool hiding.
  :defer t
  :hook ((erc-text-matched . erc-hide-fools))
  :custom
  (erc-fool-highlight-type 'all))

(use-package erc-button
  ;; IRC client button commands.
  :defer t
  :config
  (setf (alist-get "Ignore" erc-nick-popup-alist nil nil 'equal) 'erc-cmd-IGNORE))

(use-package erc-fill
  ;; IRC client line wrap.
  :defer t
  :custom
  (erc-fill-column 120)
  (erc-fill-static-center 20)
  (erc-fill-function 'erc-fill-static))

(use-package erc-track
  ;; IRC client activity tracking.
  :defer t
  :config
  ;; Prevent JOINs and PARTs from lighting up the mode-line.
  (setopt erc-track-faces-priority-list
          (remq 'erc-notice-face erc-track-faces-priority-list))
  :custom (erc-track-priority-faces-only 'all))

(use-package erc-goodies
  ;; IRC client formatting.
  :defer t
  :hook (erc-mode . erc-keep-place-indicator-enable)
  :custom
  (erc-scrolltobottom-all t)
  (erc-interpret-mirc-color t))

(use-package erc-status-sidebar
  ;; IRC client sidebar.
  :defer t
  :custom
  (erc-status-sidebar-click-display-action '(display-buffer-same-window (inhibit-same-window))))

(use-package erc-irc-format
  ;; Enable https://github.com/fmqa/erc-irc-format for transient-based formatting UI.
  :defer t
  :vc (:url "https://github.com/fmqa/erc-irc-format.git" :branch "main")
  :bind (:map erc-mode-map ("C-c q" . erc-irc-format)))

(use-package ediff-wind
  ;; Saner ediff.
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package message
  ;; Message composition.
  :defer t
  :custom
  (message-mail-user-agent t))

(use-package gnus-sum
  ;; Gnus summary.
  :defer t
  :custom
  (gnus-auto-select-first nil)
  (gnus-sum-thread-tree-false-root "")
  (gnus-sum-thread-tree-indent " ")
  (gnus-sum-thread-tree-leaf-with-other "├► ")
  (gnus-sum-thread-tree-root "")
  (gnus-sum-thread-tree-single-leaf "╰► ")
  (gnus-sum-thread-tree-vertical "│")
  (gnus-summary-display-arrow nil)
  (gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B (%c) %s%)\12")
  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
  (gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))))

(use-package gnus-art
  ;; Gnus articles.
  :defer t
  :custom
  (gnus-article-browse-delete-temp t)
  (gnus-mime-display-multipart-related-as-mixed t)
  (gnus-treat-strip-trailing-blank-lines 'last)
  ;; From https://xristos.sdf.org/fix-gnus-mime.el.txt
  (gnus-inhibit-images t)
  (gnus-buttonized-mime-types
   '("multipart/alternative" "multipart/encrypted" "multipart/signed" ".*/signed"
     "text/x-org" "text/richtext" "text/enriched")))

(use-package gnus-start
  ;; Gnus start screen.
  :defer t
  :custom
  (gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[”]”[#’()]"))

(use-package gnus-group
  ;; Gnus newsgroups.
  :defer t
  :custom
  (gnus-group-line-format "%M%S%p%P%5y:%B %G\12"))

(use-package gnus
  ;; Gnus news/mailreader.
  :defer t
  :custom
  (gnus-keep-backlog 0))

(use-package mm-decode
  ;; Secure mm-decode, courtesy of https://xristos.sdf.org/fix-gnus-mime.el.txt
  :defer t
  :custom
  (mm-html-inhibit-images t)
  (mm-enable-external 'ask)
  (mm-discouraged-alternatives '("text/html" "text/richtext" "text/enriched" "image/.*"))
  (mm-automatic-display '("text/plain"))
  (mm-inlined-types '("text/plain" "text/html"))
  (mm-inline-media-tests
   `(("text/plain" mm-inline-text identity)
     ("text/html"
      mm-inline-text-html
      ,(lambda (_handle)
         mm-text-html-renderer))
     (".*" ignore identity))))

(use-package mailcap
  ;; Secure mailcap, courtesy of https://xristos.sdf.org/fix-gnus-mime.el.txt
  :defer t
  :custom
  (mailcap-download-directory "/tmp")
  (mailcap-user-mime-data '(("xdg-open %s" ".*"))))

(use-package which-key
  ;; Discoverability: which-key key guides.
  :defer t
  :custom
  (which-key-mode t))

(use-package marginalia ;; EXTERNAL
  :defer t
  :ensure t
  :init (marginalia-mode))

(use-package markdown-mode ;; EXTERNAL
  ;; Ensure `markdown-mode' is installed for prettier eglot docs.
  :defer t
  :ensure t)

(use-package rainbow-delimiters ;; EXTERNAL
  ;; Rainbow delimiters.
  :defer t
  :ensure t
  :hook (prog-mode conf-mode))

(use-package doom-themes ;; EXTERNAL
  ;; Visually-appealing dark theming.
  :ensure t
  :config
  (load-theme 'doom-nord t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Load (optional) additional user-specific configuration.
(load (file-name-concat user-emacs-directory "user.el") t)
