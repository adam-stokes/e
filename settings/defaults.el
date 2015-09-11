;;; defaults.el --- Base defaults for editor
;;; Commentary:
;;;   base config, no external packages/dependencies


;;; Code:

;; paste outside of emacs
(setq x-select-enable-clipboard t)

;; increase mem threshold
(setq gc-cons-threshold 20000000)

;; auto refresh dired
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; show keystrokes
(setq echo-keystrokes 0.1)

;; move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; open compressed
(auto-compression-mode t)

;; Font
(mapc
 (lambda(face)
   (set-face-attribute face nil :weight 'normal :underline nil)
   (set-face-attribute face nil :height 100 :family "Ubuntu Mono"))
 (face-list))

;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; No backup files
(setq make-backup-files nil)

;; No pause on redisplay
(setq redisplay-dont-pause t)

;; kill whole line
(setq kill-whole-line t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; No startup message
(setq inhibit-startup-message t)

;; default browser
(setq browse-url-browser-function 'browse-url-generic)
(defvar browse-url-generic-program)
(setq browse-url-generic-program "google-chrome")

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; Represent undo-history as an actual tree (visualize with C-x u)
(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; 80 chars is a good width.
(set-default 'fill-column 80)

;; truncate lines
(set-default 'truncate-lines t)

;; Prefer utf8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Do not ask for confirmation
(setq confirm-nonexistent-file-or-buffer nil)

;; do not blink cursor
(blink-cursor-mode -1)

;; delete trailing whitespace
(defalias 'dtw 'delete-trailing-whitespace)
;; y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Do not show annying menu-bar tips
(setq suggest-key-bindings nil)

;; Show column number in mode line
(column-number-mode 1)

;; org-mode specific
(defvar org-agenda-files)
(setq org-agenda-files (list "~/Dropbox/Canonical/GTD/openstack-installer.org"
                             "~/Dropbox/Canonical/GTD/subiquity.org"
                             "~/Dropbox/Personal/GTD.org"))
(global-set-key (kbd "<f12>") 'org-agenda)
(setq inhibit-splash-screen t)
(org-agenda-list)
(delete-other-windows)

;; erc
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

;; server
(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'defaults)
;;; defaults.el ends here
