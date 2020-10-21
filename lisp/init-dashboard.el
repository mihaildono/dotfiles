;;; init-dashboard.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'dashboard)
(require-package 'page-break-lines)
(require-package 'all-the-icons)

(dashboard-setup-startup-hook)

;; Set the title
(setq dashboard-banner-logo-title "Nothing is true, everything is permitted")
;; Set the banner
(setq dashboard-startup-banner 'logo)
;; show package load time
(setq dashboard-set-init-info t)
;; center content
(setq dashboard-center-content t)
(setq dashboard-set-footer nil)
(setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (agenda . 5)))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
