;; Bind Ctrl+Menu to open the global menu
(global-set-key (kbd "C-<menu>") 'menu-bar-open)
;; Easily access recentf functionality with menu keybinds
(global-set-key (kbd "M-<menu> o") 'recentf-open-files)
(global-set-key (kbd "M-<menu> e") 'recentf-edit-list)
(global-set-key (kbd "M-<menu> c") 'recentf-cleanup)

;; Line numbers
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; join-line is bound to M-^ by default which is too annoying to use with dead keys
(global-set-key (kbd "C-,") 'join-line)
;; Bind dabbrev commands to something more accessible on a non-US keyboard layout,
;; since they are commonly used
(global-set-key (kbd "C-. C-.") 'dabbrev-expand)
(global-set-key (kbd "C-. C-SPC") 'dabbrev-completion)
;; Bind the "fancy" expansion to something more accessible
(global-set-key (kbd "C-#") 'hippie-expand)

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

;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'which-key)
  (package-refresh-contents)
  (package-install 'which-key))
(with-eval-after-load 'which-key
  ;; Avoid conflict with which-key pager (Originally, this key is defined as `mark-defun')
  (global-unset-key (kbd "C-M-h"))
  ;; Avoid conflict with which-key pager (Originally, this key is defined as `help-for-help')
  (global-unset-key (kbd "C-h C-h")))

(unless (package-installed-p 'dracula-theme)
  (package-refresh-contents)
  (package-install 'dracula-theme))

;; Notes:
;;
;; * `rectangle-preview' is disabled due to an overlay clearing bug

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t)
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("05626f77b0c8c197c7e4a31d9783c4ec6e351d9624aa28bc15e7f6d6a6ebd926" default))
 '(desktop-save-mode t)
 '(fido-vertical-mode t)
 '(global-whitespace-mode t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(org-replace-disputed-keys t)
 '(package-selected-packages '(which-key dracula-theme))
 '(prettify-symbols-unprettify-at-point 'right-edge)
 '(recentf-mode t)
 '(rectangle-preview nil)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(undo-no-redo t)
 '(which-key-mode t)
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
 '(whitespace-style
   '(face trailing tabs newline missing-newline-at-eof empty space-after-tab space-before-tab tab-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-indentation ((t nil))))
