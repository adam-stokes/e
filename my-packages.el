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
    magit
    yasnippet
    jedi
    ujelly-theme
    helm
    helm-package
    rainbow-delimiters
    autopair
    py-autopep8
    hl-line
    markdown-mode
    ibuffer
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
