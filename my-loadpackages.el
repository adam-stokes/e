;; packages config

;; load packages
(load "~/.emacs.d/my-packages.el")

;; Set theme
(load-theme 'atom-dark t)

;; magit
(require 'magit)
(define-key global-map (kbd "C-c m") 'magit-status)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(yas-load-directory "~/.emacs.d/snippets")
(add-hook 'term-mode-hook (lambda ()
			    (setq yas-dont-activate t)))

;; jedi
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)

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

;; slim
(require 'slim-mode)

;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; flycheck
(require 'flycheck)
(global-flycheck-mode)
(global-set-key (kbd "C-c j") 'helm-flycheck)

;; helm flycheck
(require 'helm-flycheck)

;; coffee script
(require 'coffee-mode)
(setq coffee-tab-width 2)

;; ansi color
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; ruby
(require 'robe)
(require 'rspec-mode)
(require 'ruby-end)

;; cucumber features
(require 'feature-mode)
