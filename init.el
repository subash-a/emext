;; ============================= MY EMACS INIT ==============================

;;------------------------------ PACKAGES ------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("stable-melpa" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; ---------------------------- EXEC PATH FROM SHELL ------------------------
;; initialize PATH variable from shell
(exec-path-from-shell-initialize)

;;------------------------ MY PACKAGE LIST -----------------------------------
(setq mypackages '(company
				   deft
				   exec-path-from-shell
				   go-mode
				   ivy
				   lsp-mode
				   lsp-ui
				   magit
				   projectile
				   smex
				   solarized-theme))
(defun setup-editor ()
  (interactive)
  (message "%s" "refreshing package contents for install...")
  (package-refresh-contents)
  (message "%s" "installing all required packages")
  (dolist (pkg mypackages (message "editor setup complete"))
	(when (not (package-installed-p pkg))
	  (package-install pkg))))

;;----------------------- CUSTOM VARIABLES -----------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "/Users/subhashsharma/.backups" t)))
 '(backup-directory-alist '((".*" . "/Users/subhashsharma/.backups")))
 '(column-marker-1 81)
 '(column-number-mode t)
 '(custom-safe-themes
   '("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "55ed02951e7b458e4cd18837eefa1956884c9afd22bb514f697fd1d2d1abb3d3" default))
 '(echo-keystrokes 0.1)
 '(global-linum-mode 1)
 '(package-selected-packages
   '(magit ivy lsp-ui company company-mode exec-path-from-shell solarized-theme smex projectile lsp-mode go-mode deft))
 '(size-indication-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(use-dialog-box nil)
 '(visible-bell t))

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
;; only if a Window system is being used
(if (display-graphic-p)
	(progn
	  (scroll-bar-mode -1)
	  (tool-bar-mode -1)
	  (menu-bar-mode -1)))


;; show matching parenthesis
(show-paren-mode t)

;; I dont like to type yes and no I would rather type y and n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Custom function for creating zsh term
(defun zsh-term (name)
  (interactive "Bname: ")
  (ansi-term "/bin/zsh" name))
;; Custom function for creating bash term
(defun bash-term (name)
  (interactive "Bname: ")
  (ansi-term "/bin/bash" name))
;;----------------------------- FONT FACE SETTINGS ---------------------------
(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(set-face-attribute 'default nil
					  :family "Hack"
					  :height 135
					  :weight 'normal
					  :width 'normal)

;; ------------------------ KEY MAPPINGS -------------------------------------
;; Setting the keybinding for Enter to Ctrl-j for return and indent
(global-set-key (kbd "RET") 'newline-and-indent)

;; Setting up for single space on hitting SPC bar
(global-set-key (kbd "SPC") 'just-one-space)

;; Setting up key bindings for Ace Jump Mode
(global-set-key (kbd "C-x j") 'ace-jump-mode)

;; Setting up key binding for magit status
(global-set-key (kbd "C-x g") 'magit-status)

;; Setting up key binding for terminal support
(global-set-key (kbd "C-x t") 'zsh-term)

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

;; Projectile switch project
(global-set-key (kbd "C-c C-p") 'projectile-switch-project)

;; Toggle line break on and off
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)

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
	(load-theme 'solarized-dark t)
  (load-theme 'tango-dark t))

;;---------------------------- SMEX SETTINGS ---------------------------------
(require 'smex)
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;--------------------------- DEFT MODE --------------------------------------
(require 'deft)
(setq deft-extensions '("txt" "org" "md"))
(setq deft-directory "~/Dropbox/Notes")
(setq deft-recursive t)
(setq deft-default-extension "org")
(setq deft-auto-save-interval 0)
(global-set-key [f8] 'deft)

;;------------------------ PROJECTILE SETTINGS -------------------------------
(require 'projectile)
(projectile-global-mode t)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'ivy)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

;;------------------------- GO MODE ------------------------------------------
(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)

(eldoc-mode 0) ;; disable eldoc mode since it will be redundant

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
