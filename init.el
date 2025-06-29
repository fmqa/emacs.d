;; -*- lexical-binding: t; indent-tabs-mode: nil -*-

;; More basic editing conveniences
(use-package emacs
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

;; Use separate file for customization variables
(use-package cus-edit
  :config
  (setopt custom-file (file-name-concat user-emacs-directory "custom.el"))
  (load custom-file))

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/") t)
  :custom
  (package-archive-priorities '(("gnu" . 10)
                                ("nongnu" . 10))))

;; Basic editing conveniences
(use-package simple
  ;; DWIMify case transform commands
  :bind (("M-u" . upcase-dwim)
         ("M-l" . downcase-dwim)
         ("M-c" . capitalize-dwim))
  :custom
  (undo-no-redo t)
  (column-number-mode t)
  (completion-auto-select 'second-tab)
  (mail-user-agent 'gnus-user-agent))

;; Repeat mode
(use-package repeat
  :defer t
  :custom
  (repeat-mode t)
  (repeat-exit-key "<return>"))

;; Delete selection on edit
(use-package delsel
  :custom
  (delete-selection-mode t))

;; Smooth scrolling
(use-package pixel-scroll
  :disabled ;; Display issues on 30.1
  :custom
  (pixel-scroll-precision-mode t))

;; IBuffer
(use-package ibuffer
  :defer t
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-save-with-custom nil))

;; Project
(use-package project
  :defer t
  :custom
  (project-buffers-viewer 'project-list-buffers-ibuffer))

;; Show minibuffer depth
(use-package mb-depth
  :defer t
  :custom
  (minibuffer-depth-indicate-mode t))

;; Incremental search
(use-package isearch
  :defer t
  :custom
  (isearch-lazy-count t))

;; Better-looking tab groups
(use-package tab-bar
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

;; Highlight whitespace characters
(use-package whitespace
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

;; Make session management less annoying
(use-package desktop
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

;; Activate fancy minibuffer completion
(use-package icomplete
  :defer t
  :custom
  (fido-vertical-mode t))

;; Completion previews
(use-package completion-preview
  :defer t
  :hook (prog-mode conf-mode inferior-emacs-lisp-mode eshell-mode shell-mode))

;; Better dired defaults
(use-package dired
  :defer t
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-dwim-target t))

;; Automatically refresh modified file buffers
(use-package autorevert
  :defer t
  :custom
  (global-auto-revert-mode t)
  (auto-revert-avoid-polling t)
  (auto-revert-check-vc-info t))

;; Bind duplicate-dwim as recommended by mickeyp
(use-package misc
  :defer t
  :bind ("C-x j" . duplicate-dwim))

;; Easily access recentf functionality
(use-package recentf
  :defer t
  :bind (("C-x <f10> o" . recentf-open-files)
         ("C-x <f10> e" . recentf-edit-list)
         ("C-x <f10> c" . recentf-cleanup))
  :custom
  (recentf-mode t))

;; Line numbers
(use-package display-line-numbers-mode
  :defer t
  :hook (text-mode prog-mode conf-mode))

;; Hyperlink buttonization
(use-package goto-addr
  :defer t
  :hook ((compilation-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)
         (conf-mode . goto-address-mode)
         (text-mode . goto-address-mode)
         (eshell-mode . goto-address-mode)
         (shell-mode . goto-address-mode)))

;; Electric pairs
(use-package elec-pair
  :defer t
  :hook ((prog-mode conf-mode) . electric-pair-local-mode))

;; Accessible keybind for hippie-expand
(use-package hippie-expand
  :defer t
  :bind ("C-x C-/" . hippie-expand)
  (:repeat-map hippie-expand-repeat-map
               ("C-/" . hippie-expand)))

;; Windowing rules
(use-package window
  :preface
  (defun shell-command-buffer-name-condition (name action)
    (or (equal name shell-command-buffer-name)
        (equal name shell-command-buffer-name-async)))
  :config
  ;; Reuse existing shell output windows, even those on other frames
  (add-to-list 'display-buffer-alist '(shell-command-buffer-name-condition display-buffer-reuse-window (reusable-frames . t))))

;; Set windowmove prefix to C-x w
(use-package windmove
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

;; Window management
(use-package winner
  :defer t
  :custom
  (winner-mode t))

;; Diff display
(use-package diff-mode
  :defer t
  :custom
  ;; Fix non-deterministic order of font-lock overlays
  (diff-font-lock-syntax nil))

;; Prettify checkboxes in org-mode
(use-package prog-mode
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

;; Org keybindings
(use-package org-keys
  :defer t
  :custom
  (org-replace-disputed-keys t))

;; IRC client
(use-package erc
  :defer t
  :ensure t
  :pin gnu-devel
  :config
  (setopt erc-modules
          (seq-union '(bufbar nicks notifications scrolltobottom services irc-format-normalize)
                     erc-modules))
  :custom
  (erc-interactive-display 'buffer)
  (erc-receive-query-display 'bury)
  (erc-server-reconnect-function 'erc-server-delayed-check-reconnect)
  (erc-prompt "\u27a4"))

;; Better fool handling
(use-package erc-match
  :defer t
  :hook ((erc-text-matched . erc-hide-fools))
  :custom
  (erc-fool-highlight-type 'all))

;; IRC client button commands
(use-package erc-button
  :defer t
  :config
  (setf (alist-get "Ignore" erc-nick-popup-alist nil nil 'equal) 'erc-cmd-IGNORE))

;; IRC client line wrap
(use-package erc-fill
  :defer t
  :custom
  (erc-fill-function 'erc-fill-wrap))

;; IRC client activity tracking
(use-package erc-track
  :defer t
  ;; Prevent JOINs and PARTs from lighting up the mode-line.
  :config
  (setopt erc-track-faces-priority-list
          (remq 'erc-notice-face erc-track-faces-priority-list))
  :custom (erc-track-priority-faces-only 'all))

;; IRC client formatting
(use-package erc-goodies
  :defer t
  :hook (erc-mode . erc-keep-place-indicator-enable)
  :custom
  (erc-scrolltobottom-all t)
  (erc-interpret-mirc-color t))

;; IRC client sidebar
(use-package erc-status-sidebar
  :defer t
  :custom
  (erc-status-sidebar-click-display-action '(display-buffer-same-window (inhibit-same-window))))

;; Formatting extension: https://github.com/fmqa/erc-irc-format
(use-package erc-irc-format
  :defer t
  :vc (:url "https://github.com/fmqa/erc-irc-format.git" :branch "main")
  :bind (:map erc-mode-map ("C-c q" . erc-irc-format)))

;; Sane ediff
(use-package ediff-wind
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain))

;; Message composition
(use-package message
  :defer t
  :custom
  (message-mail-user-agent t))

;; Gnus summary
(use-package gnus-sum
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

;; Gnus articles
(use-package gnus-art
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

;; Gnus start screen
(use-package gnus-start
  :defer t
  :custom
  (gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[”]”[#’()]"))

;; Gnus newsgroups
(use-package gnus-group
  :defer t
  :custom
  (gnus-group-line-format "%M%S%p%P%5y:%B %G\12"))

;; Gnus news/mailreader
(use-package gnus
  :defer t
  :custom
  (gnus-keep-backlog 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Security stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN https://xristos.sdf.org/fix-gnus-mime.el.txt ;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package mm-decode
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
  :defer t
  :custom
  (mailcap-download-directory "/tmp")
  (mailcap-user-mime-data '(("xdg-open %s" ".*"))))

;; END https://xristos.sdf.org/fix-gnus-mime.el.txt ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Security stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; THIRD PARTY PACKAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ensure `markdown-mode' is installed for prettier eglot docs
(use-package markdown-mode
  :defer t
  :ensure t)

;; Rainbow delimiters
(use-package rainbow-delimiters
  :defer t
  :ensure t
  :hook (prog-mode conf-mode))

;; Which key
(use-package which-key
  :defer t
  :custom
  (which-key-mode t))

;; Visually-appealing dark theming
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-nord t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; END THIRD PARTY PACKAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load (optional) additional user-specific configuration
(load (file-name-concat user-emacs-directory "user.el") t)
