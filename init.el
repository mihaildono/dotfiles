;;; package --- Summary
;;; Commentary:
;;; Solenya's config, enjoy :)

;;; Code:

;;;;;;;;;;;;;;;;;;;
;; PACKAGE SETUP ;;
;;;;;;;;;;;;;;;;;;;

(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;

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

(defun add-watchwords ()
  "Change color of some important words."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|NOTE\\|REFACTOR\\|NOCOMMIT\\|OPTIMIZE\\)"
          1 font-lock-warning-face t))))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
   Does not indent buffer, because it is used for a before-save-hook, and that
   might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

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

;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;;

;; =============================================================================
;; Build-in

(use-package less-css-mode)

(use-package midnight) ;; Clean up obsolete buffers automatically.

(use-package dash)

(use-package saveplace ;; Save point position.
  :config
  (progn (setq-default save-place t)
         (setq save-place-file "~/.emacs.d/places")))

(use-package anzu
  :diminish
  :config
  (global-anzu-mode +1))

(use-package ispell
  :config
  (progn
    (when (executable-find ispell-program-name)
      (add-hook 'text-mode-hook 'turn-on-flyspell))))

(use-package flyspell
  :diminish
  :hook (prog-mode-hook . flycheck-mode)
  :config
  (progn
    (define-key flyspell-mode-map (kbd "C-;") nil)))

(use-package re-builder
  :config (define-key reb-mode-map (kbd "C-c C-r") 'reb-query-replace))

(use-package org
  :config
  (progn
    ;; Don't export postambles.
    (setq org-export-html-postamble nil)))

(use-package dired
  :ensure nil
  :config
  (progn
    ;; Always delete and copy recursively.
    (setq dired-recursive-deletes 'top)
    (setq dired-recursive-copies 'always)

    ;; Refresh dired, but be quiet about it.
    (setq global-auto-revert-non-file-buffers t)
    (setq auto-revert-verbose nil)

    ;; Reuse current buffer by opening with 'a'.
    (put 'dired-find-alternate-file 'disabled nil)

    (define-key dired-mode-map (kbd "<S-return>") 'dired-find-file)
    (define-key dired-mode-map (kbd "<return>") 'dired-smart-open-file)
    (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))

    (defun dired-smart-open-file ()
      (interactive)
      (let* (
             (suffixMap
              `(
                ("mp3" . "cvlc")
                ("wav" . "cvlc")
                ("pdf" . "google-chrome-stable")
                ))
             (fname (dired-get-file-for-visit))
             (fext (file-name-extension fname))
             (cmd (cdr (assoc fext suffixMap))))
        (if cmd
            (shell-command (concat cmd " \"" fname "\""))
          (dired-find-alternate-file))))

    (defun dired-start-sxiv ()
      (interactive)
      (shell-command (concat "sxiv -r " "\""
                             (dired-get-file-for-visit) "\""))))
  :bind (("C-x C-j"   . dired-jump)))

(use-package ruby-mode
  :mode (("\\.rake$" . ruby-mode)
         ("\\.thor$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Thorfile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Vagrantfile$" . ruby-mode))
  :config
  (progn
    (setq ruby-insert-encoding-magic-comment nil)
    (setq ruby-indent-level 2)
    (setq ruby-deep-indent-paren nil)))

;; =============================================================================
;; External

;; -----------------------------------------------------------------------------
;; Utilities

(use-package diminish)

(use-package switch-window
  :config
    (setq switch-window-input-style 'minibuffer)
    (setq switch-window-increase 4)
    (setq switch-window-threshold 2)
    (setq switch-window-shortcut-style 'qwerty)
    (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" "i" "o"))
  :bind
    ([remap other-window] . switch-window))

(use-package beacon
  :diminish
  :config (beacon-mode 1))

(use-package pt)

(use-package dumb-jump
  :config (dumb-jump-mode))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package idle-highlight-mode
  :config (add-hook 'prog-mode-hook '(lambda () (idle-highlight-mode t))))

(use-package anaconda-mode
  :hook python-mode-hook)

(use-package git-timemachine)

(use-package company
  :diminish
  :config
  (global-company-mode)
  (add-to-list 'company-backends 'company-jedi)
  (company-mode +1)
  (eldoc-mode +1)
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (global-set-key (kbd "<C-tab>") 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package use-package-ensure-system-package)

(use-package ivy
  :diminish
  :config (ivy-mode 1))

(use-package swiper
  :bind
  ([remap isearch-forward]  . swiper)
  ([remap isearch-backward] . swiper))

(use-package counsel
  :bind
    ("M-x" . counsel-M-x))

(use-package company-jedi
  :diminish
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t))

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(use-package volatile-highlights
  :diminish
  :init (volatile-highlights-mode t))

(use-package golden-ratio
  :diminish
  :init (golden-ratio-mode 1))

(use-package guide-key
  :diminish
  :config (progn (setq guide-key/guide-key-sequence t)
                 (setq guide-key/recursive-key-sequence-flag t)
                 (setq guide-key/popup-window-position 'right))
  :init (guide-key-mode 1))

(use-package flycheck
  :diminish
  :config (flycheck-add-next-checker 'python-flake8 'python-pylint 'ruby-rubocop)
  :init (global-flycheck-mode +1))

(custom-set-variables
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3"))

(use-package expand-region
  :bind ("C-;" . er/expand-region))

(use-package ace-jump-mode
  :bind (("C-c SPC"   . ace-jump-char-mode)))

(use-package ace-jump-buffer
  :bind ("C-c C-SPC" . ace-jump-buffer))

(use-package browse-kill-ring
  :bind (("M-y" . browse-kill-ring)))

(use-package magit
  :bind (("C-c s" . magit-status)))

(use-package projectile
  :diminish
  :init (projectile-mode)
  :config
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package git-gutter
  :diminish
  :init (global-git-gutter-mode +1))

(use-package indent-guide
  :diminish
  :init (indent-guide-mode +1))

(use-package scss-mode
  :config (progn
            (setq scss-compile-at-save nil)
            (setq css-indent-offset 2)))

(use-package smartparens
  :diminish
  :bind ("C-K" . sp-kill-hybrid-sexp)
  :init
  (progn
    (setq sp-base-key-bindings 'paredit)
    (setq sp-hybrid-kill-entire-symbol nil)

    (smartparens-global-mode t)
    (show-smartparens-global-mode t)
    (sp-use-paredit-bindings)))

;; -----------------------------------------------------------------------------
;; Major modes

(use-package js2-mode
  :mode ("\\.jsx?\\'" . js2-jsx-mode)
  :config
  (progn
    (define-key js2-mode-map (kbd "M-j") nil)
    (custom-set-variables
     '(js2-basic-offset 4)
     '(js2-strict-missing-semi-warning nil)
     '(js2-missing-semi-one-line-override t)
     '(sgml-basic-offset 4))))

(use-package markdown-mode
  :mode (("\\.md$" . markdown-mode)
         ("\\.mkd$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)))

(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.blade\\.php\\'" . web-mode)
         ("\\.jsp\\'" . web-mode)
         ("\\.eco\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))
  :init
  (progn
    (add-hook 'web-mode-hook
              '(lambda ()
                 ;; Disable whitespace-mode.
                 (whitespace-mode -1)

                 (setq web-mode-markup-indent-offset 2)
                 (setq web-mode-css-indent-offset 2)
                 (setq web-mode-code-indent-offset 4)
                 (setq web-mode-disable-autocompletion t)

                 (local-set-key (kbd "RET") 'newline-and-indent)))))

(use-package python
  :config (define-key inferior-python-mode-map (kbd "C-l") 'eshell/clear))

;; =============================================================================
;; Hooks

;; Remove whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'prog-mode-hook 'indent-guide-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(add-hook 'before-save-hook (lambda ()
                              (when (not (eq major-mode 'go-mode))
                                (call-interactively 'cleanup-buffer-safe))))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Remove all distractions.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

(setq-default fill-column 80) ; Lines should be 80 chars.

(when window-system (global-hl-line-mode)) ; Highlight current line.

(setq x-select-enable-clipboard t) ; Allow pasting selection outside of Emacs.

(setq save-interprogram-paste-before-kill t) ; Save selection from external programs in kill ring.

(electric-indent-mode +1) ; Automatically re-indentation on some commands.

(delete-selection-mode t) ; Delete selection when start writing.

(global-auto-revert-mode t) ; Auto revert file if it is changed on the disk.

(setq-default indent-tabs-mode nil) ; Don't use tabs in indentation.

(setq tab-width 2) ; Set tabs to be 2 spaces

;; Set 4 spaces tab stops.
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                      64 68 72 76 80 84 88 92 96 100 104 108 112))

(defalias 'yes-or-no-p 'y-or-n-p) ; Enable y or n answers.

(setq gc-cons-threshold 100000000) ; Increase cache

;; Always use UTF-8.
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(global-subword-mode 1) ; Navigate trough camel cased words.

(when window-system
  ;; Display help text in the echo area.
  (tooltip-mode -1)

  ;; Enable scrolling using mouse wheel.
  (mouse-wheel-mode t)

  ;; Disable blink cursor.
  (blink-cursor-mode -1)

  ;; Prettify frame title.
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(setq inhibit-startup-screen t) ; No start up screen.

(setq ring-bell-function 'ignore) ; Disable the bell ring

(setq color-theme-is-global t) ; Use one color theme for all the frames.

;; White space mode settings.
(setq whitespace-style '(face trailing lines-tail tabs))
(setq whitespace-line-column 80)
(global-whitespace-mode t)

(show-paren-mode 1) ; Show matching parens.

(global-visual-line-mode t) ; Operate on visual lines instead of logical lines.

(set-default 'indicate-empty-lines t) ; Show me empty lines after buffer end.

;; Always display file size, line and column numbers.
(size-indication-mode t)
(setq line-number-mode t)
(setq column-number-mode t)

(setenv "PAGER" (executable-find "cat")) ; Eshell has troubles with more.

(setq default-directory (getenv "HOME")) ; Use home as the default directory.

(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; Start fullscreen

(setq initial-scratch-message "") ; Remove initial scratch message

(global-linum-mode 1) ; Add line numbers

;; Save all backup files in /backups.
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; Hippie-expand preferences
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(setq js-indent-level 4) ; Fix indentation in js-mode.

(setq ac-max-width 0.5) ; Fix popup menus width

(setq echo-keystrokes 0.1) ; Show faster incomplete commands

;; nice scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)
(global-set-key (kbd "M-P") 'move-line-up)
(global-set-key (kbd "M-N") 'move-line-down)

;; Paragraph commands are by far more useful than the sentence commands.
(global-set-key (kbd "M-e") 'forward-paragraph)
(global-set-key (kbd "M-a") 'backward-paragraph)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;;;;;;;;;;;;
;; THEMES ;;
;;;;;;;;;;;;

(use-package zenburn-theme
  :config (load-theme 'zenburn t))

;;; init.el ends here
