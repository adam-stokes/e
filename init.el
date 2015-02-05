;; emacs config

;; load base config, after all initialization.
(add-hook 'after-init-hook '(lambda ()
			      (load "~/.emacs.d/my-noexternals.el")))

;; load external packages/configs
(load "~/.emacs.d/my-loadpackages.el")
