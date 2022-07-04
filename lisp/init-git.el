;;; init-git.el --- Git support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'magit)
  (global-set-key (kbd "C-c s") 'magit-status)

  ;; performance optimization
  (setq magit-diff-highlight-indentation nil
        magit-diff-highlight-trailing nil
        magit-diff-paint-whitespace nil
        magit-diff-highlight-hunk-body nil
        magit-diff-refine-hunk nil
        magit-refresh-status-buffer nil
        vc-handled-backends nil
        vc-handled-backends (delq 'Git vc-handled-backends)
        magit-auto-revert-mode nil)

  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  (remove-hook 'server-switch-hook 'magit-commit-diff))

;;  (setq magit-git-environment
;;         (append magit-git-environment
;;            (list "OVERCOMMIT_COLOR=0"))))

(provide 'init-git)
;;; init-git.el ends here
