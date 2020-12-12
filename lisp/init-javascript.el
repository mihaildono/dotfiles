;;; init-javascript.el --- Support for Javascript and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'js2-mode)

(add-hook 'js-mode-hook 'js2-minor-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))

(setq web-mode-markup-indent-offset 4)
(setq web-mode-code-indent-offset 4)

;; Remove double quotes after the character = in a tag.
(setq web-mode-enable-auto-indentation nil)

;; Remove double quotes after the character = in a tag.
(setq web-mode-enable-auto-quoting nil)

;; highlight closing tag
(setq web-mode-enable-current-element-highlight t)

;; close tag on typing '</'
(setq sgml-quick-keys 'close)

(autoload 'sgml-electric-tag-pair-mode
  "sgml-mode" "Auto edits html tags" t)
(add-hook 'js-mode-hook 'sgml-electric-tag-pair-mode)

;; add delete tag from sgml-mode
(autoload 'sgml-delete-tag
  "sgml-mode" "Delete html tags" t)
(global-set-key (kbd "C-c DEL") 'sgml-delete-tag)

;; add close tag from sgml-mode
(autoload 'sgml-close-tag
  "sgml-mode" "Close html tags" t)
(global-set-key (kbd "C-c /") 'sgml-close-tag)

(setq js2-strict-missing-semi-warning nil)
(setq js-indent-level 2)

(when (maybe-require-package 'add-node-modules-path)
  (dolist (mode '(js-mode js2-mode web-mode))
    (add-hook (derived-mode-hook-name mode) 'add-node-modules-path)))

(provide 'init-javascript)
;;; init-javascript.el ends here
