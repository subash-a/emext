;;
;; ============================= MY EMACS INIT ==============================
;;
;; ------------------------ BACKUP DIRECTORY ---------------------------------
(setq backup-directory "/Users/subhash_sharma/.backups/")

;;------------------------- LOAD FILES ---------------------------------------
(add-to-list 'load-path "/Users/subhash_sharma/Code/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(eval-after-load 'tern
  '(progn
	 (require 'tern-auto-complete)
	 (tern-ac-setup)))
;; go-template-mode is a gist from github which enables template editing in emacs.
(add-to-list 'load-path "/Users/subhash_sharma/.emacs.d/standalone-libs/")
(require 'go-template-mode)
;; go-flymake and go-flycheck imports for go src files
(add-to-list 'load-path "/Users/subhash_sharma/go/src/github.com/dougm/goflymake")
;;------------------------------ PACKAGES ------------------------------------
(load "package")
(package-initialize)
(add-to-list 'package-archives
			 '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; ---------------------------- EXEC PATH FROM SHELL ------------------------
(when (memq window-system '(mac-ns))
  (exec-path-from-shell-initialize))
;; ;;------------------------ MY PACKAGE LIST -----------------------------------
(defvar subash/packages '(auto-complete autopair column-marker company company-go deft flx-ido go-mode grizzl magit org projectile smex solarized-theme tide web-mode yaml-mode) "Default Packages")
(defun setup-editor ()
  (interactive)
  (message "%s" "refreshing package contents for install...")
  (package-refresh-contents)
  (message "%s" "installing all required packages")
  (dolist (pkg subash-packages (message "setup complete"))
	(when (not (package-installed-p pkg))
	  (package-install pkg)))
  (message "%s" "your editor is all setup"))

(defun update-all-packages ()
  (interactive)
  (package-refresh-contents)
  (dolist (pkg subash-packages (message "update complete"))
	(package-install pkg)))

;; (defun all-my-packages-installed-p ()
;;   (setq my-packages-installed t)
;;   (dolist (pkg subash/packages my-packages-installed)
;; 	(if (package-installed-p pkg)
;; 		(setq my-packages-installed nil))))

;; (unless (all-my-packages-installed-p)
;;   (message "%s" "Refreshing packages...")
;;   (package-refresh-contents)
;;   (dolist (pkg subash/packages)
;; 	(if (not (package-installed-p pkg))
;; 		(package-install pkg))))

;;----------------------- CUSTOM VARIABLES -----------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "/Users/subhash_sharma/.backups/" t)))
 '(backup-directory-alist '((".*" . "/Users/subhash_sharma/.backups/")))
 '(column-marker-1 81)
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
	("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "55ed02951e7b458e4cd18837eefa1956884c9afd22bb514f697fd1d2d1abb3d3" default)))
 '(echo-keystrokes 0.1)
 '(global-linum-mode 1)
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
;; (defun pre-save-typescript ()
;;   "Before saving typescript files do this"
;;   (if (eq major-mode 'typescript-mode)
;; 	  (shell-command-on-region (point-min) (point-max) "/Users/subhash_sharma/Modelogiq/frontend/node_modules/.bin/tsfmt --stdin" (current-buffer) t)))
;; (add-hook 'before-save-hook 'pre-save-typescript)
;;-------------------------- PATH AND ENV SETTINGS ---------------------------
;; Setting up path for node support by including the path to node_modules
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
;; Setting up the exec path since all node commands are found here
(add-to-list 'exec-path "/usr/local/bin")
;; Setting up node exec path since version specific node commands can be found here
(add-to-list 'exec-path "/Users/subhash_sharma/.nvm/versions/node/v5.4.1/bin")
;; Setting up path to contain $GOPATH/bin for running go commands
(add-to-list 'exec-path "/Users/subhash_sharma/go/bin")
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

;;------------------------ AUTOCOMPLETE SETTINGS -----------------------------
(require 'auto-complete-config)
(ac-config-default)
;;-------------------------- AUTOPAIR SETTINGS -------------------------------
(require 'autopair)
(autopair-global-mode)
;;--------------------------- ORG MODE ---------------------------------------
;; (require 'org)
;; Overriding some solarized theme settings which mess with org mode
;; (require 'solarized)
;; (defun my-org-hook ()
;;   (setq solarized-use-variable-pitch nil)
;;   (setq solarized-height-plus-4 1.0)
;;   (setq solarized-height-plus-3 1.0)
;;   (setq solarized-height-plus-2 1.0)
;;   (setq solarized-height-plus-1 1.0)
;;   (setq solarized-height-minus-1 1.0))
;; (add-hook 'org-mode-hook 'my-org-hook)
;;--------------------------- CUSTOMIZED HOOKS--------------------------------
;;--------------------------- DEFT MODE --------------------------------------
(require 'deft)
(setq deft-extensions '("txt" "org" "md"))
(setq deft-directory "~/Dropbox/Notes")
(setq deft-recursive t)
(setq deft-default-extension "org")
(setq deft-auto-save-interval 0)
(global-set-key [f8] 'deft)
;;-------------------------- TYPESCRIPT MODE----------------------------------
;; Added typescript linter for checking code when saved
(require 'flycheck)
(flycheck-define-checker typescript-tslint
  "Linter for typescript code"
  :command ("/Users/subhash_sharma/Modelogiq/frontend/node_modules/.bin/tslint" "--config /Users/subhash_sharma/Modelogiq/frontend/tslint.json" source-original)
  :error-patterns ((error line-start (file-name) "[" line ", " column "]:" (message) line-end))
  :modes (typescript-mode))
(add-to-list 'flycheck-checkers 'typescript-tslint)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
;;------------------------ PROJECTILE SETTINGS -------------------------------
(require 'projectile)
(projectile-global-mode t)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)
;;----------------------- COMPANY MODE SETTINGS ------------------------------
(require 'company)
(global-set-key (kbd "C-c C-t") 'company-complete)
;;----------------------- TIDE MODE SETTINGS ---------------------------------
(require 'typescript-mode)
(require 'tide)
(add-hook 'typescript-mode-hook
		  (lambda ()
			"Start the Tide mode with Company mode and Flycheck"
			(tide-setup)
			(flycheck-mode t)
			(eldoc-mode )
			(company-mode t)
			(global-auto-complete-mode nil)))
(setq company-tooltip-align-annotations t)
;;--------------------------- WEB MODE ---------------------------------------

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
		  (lambda ()
			"customized settings for my web mode hook"
			(setq web-mode-markup-indent-offset 4)
			(setq web-mode-css-indent-offset 4)
			(setq web-mode-code-indent-offset 4)
			(setq web-mode-enable-auto-closing t)
			(setq web-mode-enable-auto-pairing t)
			(when (string-equal "tsx" (file-name-extension buffer-file-name))
			  (tide-setup)
			  (flycheck-mode t)
			  (eldoc-mode )
			  (company-mode t)
			  (global-auto-complete-mode nil))))
;;---------------------- TERN MODE -------------------------------------------
;; Start tern when opening js files
(add-hook 'js-mode-hook (lambda() (tern-mode t)))
;;------------------------- GO MODE ------------------------------------------
;; run gofmt before saving the file in go-mode
(require 'company)
(require 'company-go)
;; (require 'go-flymake)
(require 'go-flycheck)
(add-hook 'go-mode-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
						  ;; gocode does not work without this and the below one
						  (exec-path-from-shell-copy-env "GOPATH")
						  (exec-path-from-shell-copy-env "GOROOT")
						  (push 'company-go company-backends)
						  (company-mode)
						  (flycheck-mode 1)
						  (auto-complete-mode -1)
						  (message "go mode is initialized")))
(local-set-key (kbd "M-.") 'godef-jump)
(local-set-key (kbd "C-c C-f") 'projectile-find-file)
;;------------------------ MY BLOG -------------------------------------------
(setq org-publish-project-alist
      '(("my-blog"
	      :base-directory "~/Code/profile/blogs/"
	      :publishing-directory "~/Code/profile/output/blogs/"
		  :org-html-html5-fancy t
		  :publishing-function org-html-publish-to-html
		  :recursive t)
		("my-assets"
		 :base-directory "~/Code/profile/assets/"
		 :publishing-directory "~/Code/profile/output/assets/"
		 :publishing-function org-publish-attachment
		 :recursive t)
		("my-css"
		 :base-directory "~/Code/profile/style/"
		 :publishing-directory "~/Code/profile/output/style/"
		 :publishing-function org-publish-attachment
		 :recursive t)))
