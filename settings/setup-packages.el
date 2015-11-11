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
  '(ujelly-theme
    ace-jump-mode
    adoc-mode
    anaconda-mode
    autopair
    coffee-mode
    browse-kill-ring
    clojure-mode
    company
    company-anaconda
    company-web
    editorconfig
    expand-region
    feature-mode
    fill-column-indicator
    fish-mode
    flycheck
    flycheck-pyflakes
    flymake-coffee
    flymake-php
    flycheck-clojure
    php-mode
    fold-this
    handlebars-mode
    helm
    helm-flycheck
    helm-make
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
    monokai-theme
    mustache-mode
    neotree
    org
    org-bullets
    paradox
    powerline
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
    undo-tree
    web-beautify
    web-mode
    win-switch
    yaml-mode
    keychain-environment
    sqlup-mode
    sql-indent
    ) "List of packages that are installed at launch.")

;; check installed packages
(defun packages-installed-p ()
  "Install package if not installed."
  (loop for p in required-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(provide 'setup-packages)
;;; setup-packages.el ends here
