;;; init-javascript.el --- Support for Javascript and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))

(setq js-indent-level 2)

(when (maybe-require-package 'add-node-modules-path)
  (dolist (mode '(js-mode))
    (add-hook (derived-mode-hook-name mode) 'add-node-modules-path)))

(provide 'init-javascript)
;;; init-javascript.el ends here
