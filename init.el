;;; init.el --- emacs config
;;; Commentary:
;;;   My personal config

;;; Code:

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(defvar settings-dir)
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

(defvar defuns-dir)
(setq defuns-dir
      (expand-file-name "defuns" user-emacs-directory))

;; Load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path defuns-dir)

;; Load external packages
(require 'setup-packages)

(unless (packages-installed-p)
  ;;; check for new packages, refresh database and install missing if not updated.
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (message "%s" "done.")
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(require 'defaults)
(require 'utils)
(require 'key-bindings)

;; Load packages

;; Set theme
(load-theme 'monokai t)

(require 'pyenv-mode)
(require 'pyenv-mode-auto)

;; paradox
(require 'paradox)

;; expand-region
(require 'expand-region)
;; Don't use expand-region fast keys
(setq expand-region-fast-keys-enabled nil)

;; Show expand-region command used
(setq er--show-expansion-message t)

;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; magit
(require 'magit)
(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")
(define-key global-map (kbd "C-c m") 'magit-status)

;; anacognda
(require 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-mode)

;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; helm
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-c h o") 'helm-occur)
(helm-mode 1)

(require 'helm-package)

;; rainbow parens
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; ace
(require 'ace-jump-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; autopair
(require 'autopair)
(autopair-global-mode 1)

;; autopep8
(require 'py-autopep8)
(add-hook 'before-save-hook 'py-autopep8-before-save)

;; highlight line
(require 'hl-line)
(global-hl-line-mode t)

;; markdown
(require 'markdown-mode)

;; ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; yaml
(require 'yaml-mode)

;; toml
(require 'toml-mode)

;; sass/scss
(require 'scss-mode)
(require 'sass-mode)

;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; flycheck
(require 'flycheck)
(global-flycheck-mode)
(global-set-key (kbd "C-c j") 'helm-flycheck)
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; helm flycheck
(require 'helm-flycheck)

;; ansi color
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; handlebars
(require 'handlebars-mode)

(require 'org-bullets)

;; web-mode
(require 'web-mode)
;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; js2
(require 'js2-mode)

;; json mode
(require 'json-mode)

;; editorconfig
(require 'editorconfig)

;; jade
(require 'jade-mode)

;; stylus-mode
(require 'stylus-mode)

;; win-switch
(require 'win-switch)
(setq win-switch-idle-time 5)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(require 'powerline)
(powerline-default-theme)

(require 'sqlup-mode)

(beacon-mode 1)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
