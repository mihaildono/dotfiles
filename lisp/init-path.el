;;; init-path.el --- setup $PATH -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'exec-path-from-shell)
;; suppress init message
(setq exec-path-from-shell-check-startup-files nil)
(exec-path-from-shell-initialize)

(provide 'init-path)
;;; init-path.el ends here
