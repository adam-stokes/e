;;; utils --- utility functions

;;; Commentary:
;; unicode bs


;;; Code:
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



(defun jstidy ()
  "Beautify a region of javascript using the code from jsbeautify.org."
  (interactive)
  (let ((orig-point (point)))
    (unless (mark)
      (mark-defun))
    (shell-command-on-region (point)
                             (mark)
                             (concat "js-beautify -f - ")
                             nil t)
    (goto-char orig-point)))
(global-set-key "\C-cg" 'jstidy)


;;; ERC
(defun my-erc-md-all-but-seteam ()
  "Minimal distraction for all channels except #seteam."
  (interactive)
  (setq erc-track-priority-faces-only
        (remove "#seteam" (my-erc-joined-channels))))

(defun my-erc-joined-channels ()
  "Return all the channels you're in as a list.  This does not include queries."
  (save-excursion
    ;; need to get out of ERC mode so we can have *all* channels returned
    (set-buffer "*scratch*")
    (mapcar #'(lambda (chanbuf)
                (with-current-buffer chanbuf (erc-default-target)))
            (erc-channel-list erc-process))))

(defun reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))
(global-set-key (kbd "C-c r") 'reset-erc-track-mode)


(provide 'utils)

;;; utils.el ends here
