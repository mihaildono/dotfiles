;;; init-tide.el --- Tide -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun setup-tide-mode ()
  (interactive)
  (tide-setup))

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(provide 'init-tide)
;;; init-tide.el ends here
