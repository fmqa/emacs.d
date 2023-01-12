;; Bind Ctrl+Menu to open the global menu
(global-set-key (kbd "C-<menu>") 'menu-bar-open)

;; Line numbers
(add-hook 'text-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'linum-mode)

;; join-line is bound to M-^ by default which is too annoying to use with dead keys
(global-set-key (kbd "C-,") 'join-line)

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
 '(cua-mode t)
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("05626f77b0c8c197c7e4a31d9783c4ec6e351d9624aa28bc15e7f6d6a6ebd926" default))
 '(desktop-save-mode t)
 '(fido-mode t)
 '(global-whitespace-mode t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-selected-packages '(which-key dracula-theme))
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
   '(face trailing tabs spaces newline missing-newline-at-eof empty space-after-tab space-before-tab tab-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-indentation ((t nil))))
