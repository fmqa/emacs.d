;; Add devel elpa
(use-package package
  :custom
  (package-archive-priorities '(("gnu-devel" . -1)))
  :config
  (add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/")))

;; Ensure `markdown-mode' is installed for prettier eglot docs
(use-package markdown-mode
  :defer t
  :ensure t)

;; Bind Ctrl+Menu to open the global menu
(use-package menu-bar
  :bind ("C-<menu>" . menu-bar-open))

;; Bind duplicate-dwim as recommended by mickeyp
(use-package misc
  :bind ("C-x j" . duplicate-dwim))

;; Easily access recentf functionality with menu keybinds
(use-package recentf
  :bind (("C-x <menu> o" . recentf-open-files)
         ("C-x <menu> e" . recentf-edit-list)
         ("C-x <menu> c" . recentf-cleanup))
  :custom
  (recentf-mode t))

;; Line numbers
(use-package display-line-numbers-mode
  :hook (text-mode prog-mode conf-mode))

;; Hyperlink buttonization
(use-package goto-addr
  :hook ((compilation-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)
         (conf-mode . goto-address-mode)
         (text-mode . goto-address-mode)
         (eshell-mode . goto-address-mode)
         (shell-mode . goto-address-mode)))

;; Electric pairs
(use-package elec-pair
  :hook ((prog-mode conf-mode) . electric-pair-local-mode))

;; Accessible keybind for hippie-expand
(use-package hippie-expand
  :bind ("C-x C-/" . hippie-expand))

;; Set windowmove prefix to C-x w
(use-package windmove
  :custom (windmove-default-keybindings '([24 119])))

;; Prettify checkboxes in org-mode
(use-package prog-mode
  :hook ((prog-mode . prettify-symbols-mode)
         (org-mode . (lambda () (setq prettify-symbols-alist
                                 (append '(("[ ]" . "☐")
                                           ("[X]" . "☑")
                                           ("[-]" . "⊟"))
                                         prettify-symbols-alist))
                       (prettify-symbols-mode 1)))))

(use-package erc
  :if (package-installed-p 'erc '(5 6 -4))
  :defer t
  :custom
  (erc-fill-function 'erc-fill-wrap)
  (erc-interpret-mirc-color t)
  (erc-modules
   '(autoaway autojoin bufbar button completion fill imenu irccontrols keep-place list match menu move-to-prompt netsplit networks nicks notifications readonly ring scrolltobottom services stamp track))
  (erc-status-sidebar-click-display-action '(display-buffer-same-window (inhibit-same-window))))

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

(use-package gnus-art
  :defer t
  :custom
  (gnus-article-browse-delete-temp t)
  (gnus-inhibit-images t)
  (gnus-mime-display-multipart-related-as-mixed t)
  (gnus-treat-strip-trailing-blank-lines 'last))

(use-package gnus-start
  :defer t
  :custom
  (gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[”]”[#’()]"))

(use-package gnus-group
  :defer t
  :custom
  (gnus-group-line-format "%M%S%p%P%5y:%B %G\12"))

(use-package gnus
  :defer t
  :custom
  (gnus-keep-backlog 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Security stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN https://xristos.sdf.org/fix-gnus-mime.el.txt ;;;;;;;;;;;;;;;;;;;;;;;;;;

(setopt mm-html-inhibit-images t)
(setopt mm-enable-external 'ask)
(setopt mm-discouraged-alternatives '("text/html" "text/richtext" "text/enriched" "image/.*"))
(setopt mailcap-download-directory "/tmp")

(setopt mm-automatic-display '("text/plain"))
(setopt mm-inlined-types '("text/plain" "text/html"))
(setopt mm-inline-tests
        `(("text/plain" mm-inline-text identity)
          ("text/html"
           mm-inline-text-html
           ,(lambda (_handle)
              mm-text-html-renderer))
          (".*" ignore identity)))
(setopt mailcap-user-mime-data '(("xdg-open %s" ".*")))

(setopt gnus-buttonized-mime-types
        '("multipart/alternative" "multipart/encrypted" "multipart/signed" ".*/signed"
          "text/x-org" "text/richtext" "text/enriched"))

;; END https://xristos.sdf.org/fix-gnus-mime.el.txt ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Security stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load additional user-specific configuration
(require 'xdg)
(let ((emacs-user-init (file-name-concat (xdg-config-home) "emacs.user.d" "init.el")))
  (when (file-exists-p emacs-user-init)
    (load emacs-user-init)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-avoid-polling t)
 '(auto-revert-check-vc-info t)
 '(column-number-mode t)
 '(completion-auto-select 'second-tab)
 '(custom-enabled-themes '(leuven-dark))
 '(desktop-save-mode t)
 '(dired-kill-when-opening-new-dired-buffer t)
 '(enable-recursive-minibuffers t)
 '(fido-vertical-mode t)
 '(global-auto-revert-mode t)
 '(global-whitespace-mode t)
 '(ibuffer-save-with-custom nil)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'org-mode)
 '(initial-scratch-message nil)
 '(isearch-lazy-count t)
 '(mail-user-agent 'gnus-user-agent)
 '(menu-bar-mode nil)
 '(message-mail-user-agent t)
 '(minibuffer-depth-indicate-mode t)
 '(org-replace-disputed-keys t)
 '(pixel-scroll-precision-mode t)
 '(prettify-symbols-unprettify-at-point 'right-edge)
 '(ring-bell-function 'ignore)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(undo-no-redo t)
 '(whitespace-display-mappings
   '((space-mark 32
				 [183]
				 [46])
	 (space-mark 160
				 [164]
				 [95])
	 (newline-mark 10
				   [36 10])
	 (tab-mark 9
			   [10095 9]
			   [92 9])))
 '(whitespace-global-modes '(not erc-mode))
 '(whitespace-style
   '(face trailing tabs newline missing-newline-at-eof empty space-after-tab space-before-tab tab-mark)))
(put 'narrow-to-region 'disabled nil)
