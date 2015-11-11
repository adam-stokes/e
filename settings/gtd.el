;;; gtd.el --- gtd settings
;;; Commentary:
;;;  My org-mode settings

;;; Code:
;; org-mode specific
(defvar org-agenda-files)
(setq org-agenda-files (list "~/Dropbox/Canonical/GTD/openstack-installer.org"
                             "~/Dropbox/Canonical/GTD/subiquity.org"
                             "~/Dropbox/Personal/GTD.org"))
(global-set-key (kbd "<f12>") 'org-agenda)
(setq inhibit-splash-screen t)
(org-agenda-list)
(delete-other-windows)

(setq org-log-done 'note)

(provide 'gtd)
;;; gtd.el ends here
