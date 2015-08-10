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
  '(ace-jump-mode
    fill-column-indicator
    expand-region
    undo-tree
    fold-this
    monokai-theme
    anaconda-mode
    paradox
    company
    company-anaconda
    company-web
    adoc-mode
    autopair
    coffee-mode
    editorconfig
    feature-mode
    flycheck
    flycheck-pyflakes
    flymake-coffee
    handlebars-mode
    helm
    helm-flycheck
    helm-package
    hl-line
    ibuffer
    inf-ruby
    jade-mode
    js2-mode
    json-mode
    less-css-mode
    magit
    markdown-mode
    monokai-theme
    mustache-mode
    neotree
    org
    org-bullets
    py-autopep8
    rainbow-delimiters
    robe
    rspec-mode
    rubocop
    ruby-end
    sass-mode
    scss-mode
    slim-mode
    stylus-mode
    toml-mode
    web-mode
    web-beautify
    yaml-mode
    win-switch
    ) "List of packages that are installed at launch.")

;; check installed packages
(defun packages-installed-p ()
  "Install package if not installed."
  (loop for p in required-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(provide 'setup-packages)
;;; setup-packages.el ends here
