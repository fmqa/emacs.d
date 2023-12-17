;; Bind Ctrl+Menu to open the global menu
(global-set-key (kbd "C-<menu>") 'menu-bar-open)
;; Easily access recentf functionality with menu keybinds
(global-set-key (kbd "C-x <menu> o") 'recentf-open-files)
(global-set-key (kbd "C-x <menu> e") 'recentf-edit-list)
(global-set-key (kbd "C-x <menu> c") 'recentf-cleanup)
;; Bind duplicate-dwin as recommended by mickeyp
(global-set-key (kbd "C-x j") 'duplicate-dwim)
;; Bind right-click/mouse-3 to dired-mouse-find-file, as left-click/mouse-1
;; is bound to dired-find-file-other-window
(use-package dired
  :ensure nil
  :config
  (define-key dired-mode-map [mouse-3] 'dired-mouse-find-file))

;; Line numbers
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)

;; Electric pairs
(add-hook 'prog-mode-hook 'electric-pair-mode)

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

;; ERC: Predicates for buffer mode checks
(defun erc-status-sidebar-mode-p (buffer alist) (with-current-buffer buffer (derived-mode-p 'erc-status-sidebar-mode)))
(defun erc-mode-p (buffer alist) (with-current-buffer buffer (derived-mode-p 'erc-mode)))

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
 '(display-buffer-alist
   '((erc-status-sidebar-mode-p display-buffer-in-side-window
								(side . left))
	 (erc-mode-p display-buffer-reuse-mode-window
				 (mode erc-mode))))
 '(enable-recursive-minibuffers t)
 '(erc-fill-column 158)
 '(erc-interpret-mirc-color t)
 '(erc-modules
   '(autojoin button completion fill irccontrols keep-place list match menu move-to-prompt netsplit networks noncommands notifications readonly ring stamp track))
 '(fido-vertical-mode t)
 '(global-auto-revert-mode t)
 '(global-goto-address-mode t)
 '(global-whitespace-mode t)
 '(go-ts-mode-indent-offset 4)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'org-mode)
 '(initial-scratch-message nil)
 '(isearch-lazy-count t)
 '(menu-bar-mode nil)
 '(minibuffer-depth-indicate-mode t)
 '(org-replace-disputed-keys t)
 '(pixel-scroll-precision-mode t)
 '(prettify-symbols-unprettify-at-point 'right-edge)
 '(recentf-mode t)
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
