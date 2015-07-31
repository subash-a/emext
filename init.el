;;
;; ============================= MY EMACS INIT ==============================
;;
;; ------------------------ BACKUP DIRECTORY ---------------------------------
(setq backup-directory "~/.backups/")

(setq backup-directory-alist
      `((".*" . , backup-directory)))

(setq auto-save-file-name-transforms
      `((".*" , backup-directory t)))

;;------------------------------ PACKAGES ------------------------------------
(load "package")
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;------------------------ MY PACKAGE LIST -----------------------------------
(defvar subash/packages '(auto-complete autopair column-marker deft flx-ido magit org projectile smex web-mode yaml-mode) "Default Packages")

(defun all-my-packages-installed-p ()
  (setq my-packages-installed t)
  (dolist (pkg subash/packages my-packages-installed)
	(if (package-installed-p pkg)
		(setq my-packages-installed nil))))

(unless (all-my-packages-installed-p)
  (message "%s" "Refreshing packages...")
  (package-refresh-contents)
  (dolist (pkg subash/packages)
	(if (not (package-installed-p pkg))
		(package-install pkg))))

;;----------------------- CUSTOM VARIABLES -----------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-marker-1 81)
 '(column-number-mode t)
 '(global-linum-mode 1)
 '(size-indication-mode t)
 '(tab-width 4)
 '(echo-keystrokes 0.1)
 '(use-dialog-box nil)
 '(visible-bell t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(custom-safe-themes
   (quote
	("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "55ed02951e7b458e4cd18837eefa1956884c9afd22bb514f697fd1d2d1abb3d3" default))))

;; ------------------------- SPLASH SCREEN -----------------------------------
;; Disabling startup splash screen, setting the intial message to nil and
;; starting the editor in org-mode
(setq inhibit-splash-screen t
	  initial-scratch-message nil
	  initial-major-mode 'org-mode)

;; ----------------------------- GENERAL SETTINGS ----------------------------
;; Text Marking
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Removing scroll bars, tool bar and menu bar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; show matching parenthesis
(show-paren-mode t)

;; I dont like to type yes and no I would rather type y and n
(defalias 'yes-or-no-p 'y-or-n-p)

;;----------------------------- FONT FACE SETTINGS ---------------------------
(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(set-face-attribute 'default nil
					  :family "Monaco"
					  :height 125
					  :weight 'normal
					  :width 'normal)

;; ------------------------ KEY MAPPINGS -------------------------------------
;; Setting the keyboard mapping of the Ctrl+c u to toggle cua-mode
(global-set-key (kbd "C-c u") 'cua-mode)
;; Setting the keybinding for Enter to Ctrl-j for return and indent
(global-set-key (kbd "RET") 'newline-and-indent)
;; Setting up for single space on hitting SPC bar
(global-set-key (kbd "SPC") 'just-one-space)
;; Setting up key bindings for Ace Jump Mode
(global-set-key (kbd "C-x j") 'ace-jump-mode)
;; Setting up key binding for magit status
(global-set-key (kbd "C-x g") 'magit-status)
;; Setting up key binding for ibuffer(awesome) to the buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Commenting and Uncommenting a region
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
;; Increase the text size incrementally
(global-set-key (kbd "C-+") 'text-scale-increase)
;; Decrease the text size
(global-set-key (kbd "C--") 'text-scale-decrease)
;; Search file using projectile
(global-set-key (kbd "C-c C-f") 'projectile-find-file)
;;--------------------------- HOOKS ------------------------------------------
;; Deleting white space before saving a file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;-------------------------- PATH AND ENV SETTINGS ---------------------------
;; Setting up path for node support by including the path to node_modules
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
;; Setting up the exec path since all node commands are found here
(add-to-list 'exec-path "/usr/local/bin")

;;--------------------------- THEME ------------------------------------------
(if window-system
	(load-theme 'solarized-light t)
  (load-theme 'wombat t))

;;---------------------------- SMEX SETTINGS ---------------------------------
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;------------------------ AUTOCOMPLETE SETTINGS -----------------------------
(require 'auto-complete-config)
(ac-config-default)
;;-------------------------- AUTOPAIR SETTINGS -------------------------------
(require 'autopair)
(autopair-global-mode)

;;--------------------------- WEB MODE ---------------------------------------
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(defun my-web-mode-hook ()
  "customized settings for my web mode hook"
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t))
(add-hook 'web-hook-mode 'my-web-hook-mode)
;;------------------------ PROJECTILE SETTINGS -------------------------------
(projectile-global-mode t)
(setq projectile-enable-caching t)
