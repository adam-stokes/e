;; base config, no external packages/dependencies

;; Font
;; (add-to-list 'default-frame-alist '(font . "Source Code Pro 11"))
(set-face-attribute 'default nil :height 115 :family "Monaco")
;; No backup files
(setq make-backup-files nil)
;; No pause on redisplay
(setq redisplay-dont-pause t)
;; kill whole line
(setq kill-whole-line t)
;; No startup message
(setq inhibit-startup-message t)
;; default browser
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "firefox")
;; spaces instead of tabs
(setq-default indent-tabs-mode nil)
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
;; cursor color
(add-to-list 'default-frame-alist '(cursor-color . "LawnGreen"))
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

;; Remove scrollbars, menubar, toolbars
(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))



;; movement between windows
(global-set-key (kbd "C-c C-j") 'windmove-left)
(global-set-key (kbd "C-c C-k") 'windmove-down)
(global-set-key (kbd "C-c C-l") 'windmove-up)
(global-set-key (kbd "C-c C-;") 'windmove-right)

;; window manage
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-}") 'next-buffer)
(global-set-key (kbd "M-{") 'previous-buffer)


;; helpful functions
;; unicode bs
(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
        (message "No non-ascii characters."))))

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))
