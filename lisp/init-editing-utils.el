;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 locale-coding-system 'utf-8
 blink-cursor-interval 0.4
 whitespace-style '(face trailing lines-tail tabs)
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 column-number-mode t
 case-fold-search t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 truncate-lines nil
 truncate-partial-width-windows nil
 fill-column 80)

;; Navigate trough camelCase words.
(global-subword-mode 1)

;; Always use UTF-8.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; show whitespaces
(global-whitespace-mode t)

;; Delete selection when start writing.
(delete-selection-mode t)

;; Change color of some important words.
(font-lock-add-keywords
 nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|NOTE\\|REFACTOR\\|NOCOMMIT\\|OPTIMIZE\\)"
        1 font-lock-warning-face t)))

;; shows line for current level of indentation
(require-package 'indent-guide)
(indent-guide-global-mode)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

(require-package 'smartparens)
(require 'smartparens-config)
(add-hook 'prog-mode-hook 'smartparens-mode)
;; replace matching parens
(global-set-key (kbd "C-c C-r") 'sp-rewrap-sexp)

(require-package 'idle-highlight-mode)
(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))

(when (maybe-require-package 'beacon)
  (setq-default beacon-lighter "")
  (setq-default beacon-size 20)
  (add-hook 'after-init-hook 'beacon-mode))

;;; Newline behaviour
(global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)

;; Shows next keybindings for incomplete commands
(require-package 'which-key)
(add-hook 'after-init-hook 'which-key-mode)
(setq-default which-key-idle-delay 1)
(with-eval-after-load 'which-key
  (diminish 'which-key-mode))

;; dispaly line numbers
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))


(when (maybe-require-package 'goto-line-preview)
  (global-set-key [remap goto-line] 'goto-line-preview)

  (when (fboundp 'display-line-numbers-mode)
    (defun sanityinc/with-display-line-numbers (f &rest args)
      (let ((display-line-numbers t))
        (apply f args)))
    (advice-add 'goto-line-preview :around #'sanityinc/with-display-line-numbers)))

(when (require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; enable Flyspell mode
(dolist (hook '(prog-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; show git change lines
(require-package 'git-gutter)
(global-git-gutter-mode +1)

;; go to dired
(global-set-key (kbd "C-x C-j") 'dired-jump)

;; Show matching parens
(add-hook 'after-init-hook 'show-paren-mode)

(require-package 'multiple-cursors)
;; multiple-cursors
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
(global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)

(defun sanityinc/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))
(global-set-key (kbd "C-o") 'sanityinc/open-line-with-reindent)

(defun move-line-up ()
  "Move current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))
(global-set-key (kbd "M-P") 'move-line-up)

(defun move-line-down ()
  "Move current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))
(global-set-key (kbd "M-N") 'move-line-down)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the ppbeginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
