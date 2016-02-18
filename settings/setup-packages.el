;;; setup-packages.el --- Install packages
;;;
;;; Commentary:
;;;  Installs missing packages


;;; Code:
(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; packages to load
(defvar required-packages
  '(
    beacon
    ace-jump-mode
    anaconda-mode
    autopair
    browse-kill-ring
    editorconfig
    expand-region
    flycheck
    flycheck-pyflakes
    fold-this
    handlebars-mode
    helm
    helm-flycheck
    helm-make
    helm-package
    hl-line
    ibuffer
    jade-mode
    js2-mode
    json-mode
    keychain-environment
    magit
    markdown-mode
    monokai-theme
    neotree
    org
    org-bullets
    paradox
    powerline
    py-autopep8
    pyenv-mode
    pyenv-mode-auto
    rainbow-delimiters
    sass-mode
    scss-mode
    sql-indent
    sqlup-mode
    stylus-mode
    toml-mode
    undo-tree
    web-beautify
    web-mode
    win-switch
    yaml-mode
    ) "List of packages that are installed at launch.")

;; check installed packages
(defun packages-installed-p ()
  "Install package if not installed."
  (loop for p in required-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(provide 'setup-packages)
;;; setup-packages.el ends here
