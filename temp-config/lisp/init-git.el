;;; init-git.el --- Git support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'magit)
(global-set-key (kbd "C-c s") 'magit-status)

(provide 'init-git)
;;; init-git.el ends here
