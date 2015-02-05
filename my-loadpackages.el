;; packages config

;; load packages
(load "~/.emacs.d/my-packages.el")

;; Set ujelly theme
(load-theme 'ujelly t)

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
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
