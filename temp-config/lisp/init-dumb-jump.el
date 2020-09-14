;;; init-dumb-jump.el --- Jump to definition -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; go To Definition
(require-package 'dumb-jump)
(add-hook 'prog-mode-hook 'dumb-jump-mode)

(provide 'init-dumb-jump)
;;; init-dumb-jump.el ends here
