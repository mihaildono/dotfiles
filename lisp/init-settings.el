;;; init-settings.el --- Editor Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; General settings
(fset 'yes-or-no-p 'y-or-n-p)
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; change command to be meta key on macOS
(setq mac-command-modifier 'meta)

;; Highlight current line.
(when window-system (global-hl-line-mode))

;; Remove initial scratch message
(setq initial-scratch-message "")

;; Removes tooltips and displays help in echo area
(tooltip-mode -1)

;; Start fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; backup files directory
(setq backup-directory-alist `((".*" . "~/.emacs-saves/")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))

;; Show faster incomplete commands
(setq echo-keystrokes 0.1)

;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-message t)
(setq default-directory "~/")
(setq inhibit-startup-screen t)

;; Change window name to current file
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; Disable the bell ring
(setq ring-bell-function 'ignore)

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
   Does not indent buffer, because it is used for a before-save-hook, and that
   might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(add-hook 'before-save-hook (lambda ()
                              (when (not (eq major-mode 'go-mode))
                                (call-interactively 'cleanup-buffer-safe))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'init-settings)
;;; init-settings.el ends here
