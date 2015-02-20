
(put 'upcase-region 'disabled nil)

(setq backup-directory "c:/temp/")

(setq backup-directory-alist
      `((".*" . , backup-directory)))

(setq auto-save-file-name-transforms
      `((".*" , backup-directory t)))

;;(setq url-using-proxy t)
;;(setq url-proxy-services
;;      `(("http" . "proxy:8080")
;;	("https" . "proxy:8080")))
;;
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload `javascript-mode "javascript" nil t)

(add-to-list 'auto-mode-alist '("\\.xsjs\\'" . javascript-mode))
(autoload `javascript-mode "javascript" nil t)

(load-library "myutils/ace-jump-mode.el")
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  
  ;; Adding New package installation archives
(require `package)
(add-to-list `package-archives
	     `("melpa" . "http://melpa.milkbox.net/packages/") t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-marker-1 81)
 '(column-number-mode t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(linum-mode 1)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "outline" :family "Consolas")))))


;;(set-face-attribute 'default nil :font "Consolas")
(set-face-attribute 'default nil :height 130)
;;Setting up home directory
(setq default-directory "c:/Users/I065682/git_repos/project-sentinel/user_experience/")
(cd default-directory)
;; Setting up gnus email and news feed
(setq user-mail-address "s7subash@gmail.com")
(setq user-full-name "Subhash Sharma")
;; Loading custom libraries when needed to use
(load-library "myutils/learn.el")
;; Setting column markers for 80 column limit
;; Enabling Macros when loading the editor
(load-library "mymacros.el")
;;Enabling awesome CUA Mode for Selecting rectangles regions
(setq cua-enable-cua-keys nil)
(setq cua-highlight-region-shift-only t) ;; no transient mode mark
(setq cua-toggle-set-mark nil)
(cua-mode)
;; Setting the tags file for searching
(setq tags-table-list
      '("~/" "."))
;; Setting the keyboard mapping of the Ctrl+c u to toggle cua-mode
(global-set-key (kbd "C-c u") 'cua-mode)
;; Setting the keybinding for Enter to Ctrl-j for return and indent
(global-set-key (kbd "RET") 'newline-and-indent)
;; Disabling startup splash screen
(setq inhibit-splash-screen t)
;; Disabling startup screen
(setq inhibit-startup-screen t)
