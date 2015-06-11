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
    autopair
    coffee-mode
    atom-dark-theme
    flycheck
    flycheck-pyflakes
    helm
    helm-flycheck
    helm-package
    handlebars-mode
    hl-line
    ibuffer
    inf-ruby
    jedi
    magit
    markdown-mode
    neotree
    py-autopep8
    rainbow-delimiters
    robe
    rspec-mode
    ruby-end
    sass-mode
    scss-mode
    slim-mode
    toml-mode
    yaml-mode
    yasnippet
    feature-mode
    rubocop
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
