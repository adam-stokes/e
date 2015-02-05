;; base config, no external packages/dependencies

;; Font
;; (set-frame-font "Source Code Pro")
(add-to-list 'default-frame-alist '(font . "Source Code Pro"))
;; No backup files
(setq make-backup-files nil)
;; No pause on redisplay
(setq redisplay-dont-pause t)
;; kill whole line
(setq kill-whole-line t)
;; No startup message
(setq inhibit-startup-message t)
;; default browser
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "firefox")
;; spaces instead of tabs
(setq-default indent-tabs-mode nil)
;; truncate lines
(set-default 'truncate-lines t)
;; Prefer utf8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; Do not ask for confirmation
(setq confirm-nonexistent-file-or-buffer nil)
;; delete trailing whitespace
(defalias 'dtw 'delete-trailing-whitespace)
;; y/n
(defalias 'yes-or-no-p 'y-or-n-p)
;; Auto refresh buffers
(global-auto-revert-mode 1)
;; Show trailing whitespace
(setq-default show-trailing-whitespace t)
;; Do not show annying menu-bar tips
(setq suggest-key-bindings nil)
;; Show column number in mode line
(column-number-mode 1)

;; Remove scrollbars, menubar, toolbars
(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))



;; movement between windows
(global-set-key (kbd "C-c C-j") 'windmove-left)
(global-set-key (kbd "C-c C-k") 'windmove-down)
(global-set-key (kbd "C-c C-l") 'windmove-up)
(global-set-key (kbd "C-c C-;") 'windmove-right)
