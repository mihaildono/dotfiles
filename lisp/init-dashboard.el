;;; init-dashboard.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'dashboard)
(require-package 'page-break-lines)
(require-package 'all-the-icons)

(dashboard-setup-startup-hook)

;; show package load time
(setq dashboard-set-init-info t)
;; center content
(setq dashboard-center-content t)
(setq dashboard-set-footer nil)
(setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (agenda . 5)))

;; show latest xkcd comic
(require-package 'xkcd)

;; to update cached xkcd
(with-temp-buffer
  (xkcd)
  (xkcd-kill-buffer))

;; setting dashboard image (png)
(let ((last-xkcd-png (concat xkcd-cache-dir (number-to-string xkcd-latest) ".png")))
  (if (file-exists-p last-xkcd-png)
      (setq dashboard-banner-official-png last-xkcd-png)))

;; set text under image
(setq dashboard-banner-logo-title (xkcd))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
