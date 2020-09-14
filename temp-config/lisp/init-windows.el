;;; init-windows.el --- Working with windows within frames -*- lexical-binding: t -*-
;;; Commentary:

;; This is not about the "Windows" OS, but rather Emacs's "windows"
;; concept: these are the panels within an Emacs frame which contain
;; buffers.

;;; Code:
(require-package 'switch-window)

(setq switch-window-increase 4)
(setq switch-window-threshold 3)
(setq switch-window-shortcut-style 'qwerty)
(setq switch-window-qwerty-shortcuts '("a" "s" "d" "f" "j" "k" "l" "i" "o"))
(setq-default switch-window-timeout nil)
(global-set-key (kbd "C-x o") 'switch-window)

(require-package 'golden-ratio)
(golden-ratio-mode 1)

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(provide 'init-windows)
;;; init-windows.el ends here
