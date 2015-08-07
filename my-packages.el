;; packages
(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; packages to load
(defvar required-packages
  '(ace-jump-mode
    company
    company-jedi
    company-web
    adoc-mode
    atom-dark-theme
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
    jedi
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
    ) "list of packages that are installed at launch")

;; check installed packages
(defun packages-installed-p ()
  (loop for p in required-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(unless (packages-installed-p)
  ;;; check for new packages, refresh database and install missing if not updated.
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (message "%s" "done.")
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))
