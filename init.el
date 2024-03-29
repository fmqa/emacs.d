(require 'xdg)

;; Bind Ctrl+Menu to open the global menu
(global-set-key (kbd "C-<menu>") 'menu-bar-open)
;; Easily access recentf functionality with menu keybinds
(global-set-key (kbd "C-x <menu> o") 'recentf-open-files)
(global-set-key (kbd "C-x <menu> e") 'recentf-edit-list)
(global-set-key (kbd "C-x <menu> c") 'recentf-cleanup)
;; Bind duplicate-dwin as recommended by mickeyp
(global-set-key (kbd "C-x j") 'duplicate-dwim)

;; Line numbers
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)

;; URLs are already buttonized by `erc-button'
(defun disable-goto-address-mode () (goto-address-mode 0))
(add-hook 'erc-mode-hook 'disable-goto-address-mode)

;; Electric pairs
(add-hook 'prog-mode-hook 'electric-pair-local-mode)

;; Accessible keybind for hippie-expand
(global-set-key (kbd "C-x C-/") 'hippie-expand)

;; Alternate windmove keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings '(control super)))

;; Prettify checkboxes in org-mode
(defun configure-prettify-symbols-controls ()
  (setq prettify-symbols-alist '(("[ ]" . "☐")
                                 ("[X]" . "☑")
                                 ("[-]" . "⊟"))))
(add-hook 'org-mode-hook (lambda () (configure-prettify-symbols-controls) (prettify-symbols-mode)))

;; DrScheme-like lambdas for LISPs
(defun configure-prettify-symbols-lisps ()
  (setq prettify-symbols-alist '(("lambda" . "λ"))))
(add-hook 'lisp-mode-hook (lambda () (configure-prettify-symbols-lisps) (prettify-symbols-mode)))
(add-hook 'emacs-lisp-mode-hook (lambda () (configure-prettify-symbols-lisps) (prettify-symbols-mode)))
(add-hook 'scheme-mode-hook (lambda () (configure-prettify-symbols-lisps) (prettify-symbols-mode)))

;; Load additional user-specific configuration
(let* ((emacs-user-dir (file-name-concat (xdg-config-home) "emacs.user.d"))
       (emacs-user-dir-init (file-name-concat emacs-user-dir "init.el")))
  (when (file-exists-p emacs-user-dir-init)
    (load emacs-user-dir-init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Security stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN https://xristos.sdf.org/fix-gnus-mime.el.txt ;;;;;;;;;;;;;;;;;;;;;;;;;;

(setopt gnus-inhibit-images t)
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
 '(erc-fill-function 'erc-fill-wrap)
 '(erc-interpret-mirc-color t)
 '(erc-modules
   '(autojoin bufbar button completion fill imenu irccontrols keep-place list match menu move-to-prompt netsplit networks nicks notifications readonly ring scrolltobottom stamp track))
 '(erc-status-sidebar-click-display-action '(display-buffer-same-window (inhibit-same-window)))
 '(fido-vertical-mode t)
 '(global-auto-revert-mode t)
 '(global-goto-address-mode t)
 '(global-whitespace-mode t)
 '(gnus-article-browse-delete-temp t)
 '(gnus-auto-select-first nil)
 '(gnus-group-line-format "%M%S%p%P%5y:%B %G\12")
 '(gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[”]”[#’()]")
 '(gnus-keep-backlog 0)
 '(gnus-mime-display-multipart-related-as-mixed t)
 '(gnus-sum-thread-tree-false-root "")
 '(gnus-sum-thread-tree-indent " ")
 '(gnus-sum-thread-tree-leaf-with-other "├► ")
 '(gnus-sum-thread-tree-root "")
 '(gnus-sum-thread-tree-single-leaf "╰► ")
 '(gnus-sum-thread-tree-vertical "│")
 '(gnus-summary-display-arrow nil)
 '(gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B (%c) %s%)\12")
 '(gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
 '(gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
 '(gnus-treat-strip-trailing-blank-lines 'last)
 '(gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M")))
 '(go-ts-mode-indent-offset 4)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'org-mode)
 '(initial-scratch-message nil)
 '(isearch-lazy-count t)
 '(mail-user-agent 'gnus-user-agent)
 '(menu-bar-mode nil)
 '(message-mail-user-agent t)
 '(minibuffer-depth-indicate-mode t)
 '(org-replace-disputed-keys t)
 '(package-archive-priorities '(("gnu-devel" . -1)))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
	 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
	 ("gnu-devel" . "https://elpa.gnu.org/devel/")))
 '(package-selected-packages '(erc markdown-mode))
 '(pixel-scroll-precision-mode t)
 '(prettify-symbols-unprettify-at-point 'right-edge)
 '(recentf-mode t)
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
