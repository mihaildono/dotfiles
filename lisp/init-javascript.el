;;; init-javascript.el --- Support for Javascript and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq js-indent-level 2)

(when (maybe-require-package 'add-node-modules-path)
  (dolist (mode '(js-mode js2-mode))
    (add-hook (derived-mode-hook-name mode) 'add-node-modules-path)))

(provide 'init-javascript)
;;; init-javascript.el ends here
