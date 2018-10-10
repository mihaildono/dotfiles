;;; package --- Summary
;;; Commentary:
;;; my config, enjoy :)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;


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


;;;;;;;;;;;;;;
;; PACKAGES ;;
;;;;;;;;;;;;;;


(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defun packages-install (my-packages)
  (dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p))))

(defun init-install-packages ()
  (packages-install
   '(use-package)))

(condition-case nil
    (init-install-packages)
  (error
   (package-refresh-contents)
   (init-install-packages)))


;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;;


;; =============================================================================
;; Build-in

(use-package less-css-mode)

(use-package midnight) ;; Clean up obsolete buffers automatically.

(use-package dash
  :ensure t)

(use-package dired-x   ;; Load some advanced dired functions.
  :bind (("C-x C-j"   . dired-jump)))

(use-package uniquify  ;; When several buffers visit identically-named files.
  :config (setq uniquify-buffer-name-style 'forward))

(use-package saveplace ;; Save point position.
  :config
  (progn (setq-default save-place t)
         (setq save-place-file "~/.emacs.d/places")))

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :config
  (global-anzu-mode +1))

(use-package ispell
  :config
  (progn
    (when (executable-find ispell-program-name)
      (add-hook 'text-mode-hook 'turn-on-flyspell))))

(use-package flyspell
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
                             (dired-get-file-for-visit) "\"")))))

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
  :init (add-hook 'ruby-mode-hook 'rubocop-mode)
  :config
  (progn
    (setq ruby-insert-encoding-magic-comment nil)
    (setq ruby-indent-level 2)
    (setq ruby-deep-indent-paren nil)))

;; =============================================================================
;; External

;; -----------------------------------------------------------------------------
;; Utilities

(use-package diminish
  :ensure t)

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package idle-highlight-mode
       :ensure t
       :config (add-hook 'prog-mode-hook '(lambda () (idle-highlight-mode t))))

(use-package anaconda-mode
  :ensure t
  :hook python-mode-hook)

(use-package git-timemachine
  :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (eldoc-mode +1)
  (company-mode +1)
  (tide-hl-identifier-mode +1))

(use-package tide
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'setup-tide-mode)
  (add-hook 'js2-jsx-mode-hook #'setup-tide-mode))

(use-package company
  :ensure t)

(use-package use-package-ensure-system-package
  :ensure t)

(use-package ivy
  :ensure t
  :config (ivy-mode 1))

(use-package swiper
  :ensure t
  :bind
  ([remap isearch-forward]  . swiper)
  ([remap isearch-backward] . swiper))

(use-package counsel
  :ensure t
  :bind
    ("M-x" . counsel-M-x))

(use-package jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t
        jedi:use-shortcuts t))

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package volatile-highlights
  :diminish volatile-highlights
  :ensure t
  :init (volatile-highlights-mode t))

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :init (golden-ratio-mode 1))

(use-package guide-key
  :ensure t
  :config (progn (setq guide-key/guide-key-sequence t)
                 (setq guide-key/recursive-key-sequence-flag t)
                 (setq guide-key/popup-window-position 'right))
  :init (guide-key-mode 1))

(use-package flycheck
  :ensure t
  :ensure-system-package
  ((pylint . "pip install pylint")
   (flake8 . "pip install flake8"))
  :config (flycheck-add-next-checker 'python-flake8 'python-pylint)
  :init (global-flycheck-mode +1))

(use-package expand-region
  :ensure t
  :bind ("C-;" . er/expand-region))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC"   . ace-jump-mode)))

(use-package ace-jump-buffer
  :ensure t
  :bind ("C-c C-SPC" . ace-jump-buffer))

(use-package browse-kill-ring
  :ensure t
  :bind (("M-y" . browse-kill-ring)))

(use-package magit
  :ensure t
  :bind (("C-c s" . magit-status)))

(use-package projectile
  :ensure t
  :bind (("C-c C-f" . projectile-find-file)
         ("C-c C-d" . projectile-find-dir))
  :init (projectile-mode))

(use-package rbenv
  :ensure t
  :config (global-rbenv-mode))

(use-package git-gutter
  :ensure t
  :init (global-git-gutter-mode +1)
  :diminish git-gutter-mode)

(use-package indent-guide
  :diminish indent-guide-mode
  :ensure t
  :init (indent-guide-mode +1))

(use-package scss-mode
  :ensure t
  :config (progn
            (setq scss-compile-at-save nil)
            (setq css-indent-offset 2)))

(use-package smartparens
  :ensure t
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
  :ensure t
  :mode ("\\.jsx?\\'" . js2-mode)
  :config
  (progn
    (define-key js2-mode-map (kbd "M-j") nil)
    (custom-set-variables
     '(js2-basic-offset 2)
     '(js2-strict-missing-semi-warning nil)
     '(js2-missing-semi-one-line-override t))))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md$" . markdown-mode)
         ("\\.mkd$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)))

(use-package web-mode
  :ensure t
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

;; Lines should be 80 chars.
(setq-default fill-column 80)

;; Highlight current line.
(when window-system (global-hl-line-mode))

;; Allow pasting selection outside of Emacs.
(setq x-select-enable-clipboard t)

;; Save selection from external programs in kill ring.
(setq save-interprogram-paste-before-kill t)

;; Automatically re-indentation on some commands.
(electric-indent-mode +1)

;; Delete selection when start writing.
(delete-selection-mode t)

;; Auto revert file if it is changed on the disk.
(global-auto-revert-mode t)

;; Don't use tabs in indentation.
(setq-default indent-tabs-mode nil)

;; Set tabs to be 2 spaces
(setq tab-width 2)

;; Set 4 spaces tab stops.
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                      64 68 72 76 80 84 88 92 96 100 104 108 112))

;; Enable y or n answers.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Increase cache
(setq gc-cons-threshold 50000000)

;; Always use UTF-8.
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Navigate trough camel cased words.
(global-subword-mode 1)

(when window-system
  ;; Display help text in the echo area.
  (tooltip-mode -1)

  ;; Enable scrolling using mouse wheel.
  (mouse-wheel-mode t)

  ;; Disable blink cursor.
  (blink-cursor-mode -1)

  ;; Prettify frame title.
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; No start up screen.
(setq inhibit-startup-message t)

;; Try to flash the frame to represent a bell instead
;; of destroying my ears.
(setq visible-bell t)

;; Use one color theme for all the frames.
(setq color-theme-is-global t)

;; White space mode settings.
(setq whitespace-style '(face trailing lines-tail tabs))
(setq whitespace-line-column 80)
(global-whitespace-mode t)

;; Show matching parens.
(show-paren-mode 1)

;; Operate on visual lines instead of logical lines.
(global-visual-line-mode t)

;; Show me empty lines after buffer end.
(set-default 'indicate-empty-lines t)

;; Always display file size, line and column numbers.
(size-indication-mode t)
(setq line-number-mode t)
(setq column-number-mode t)

;; Eshell has troubles with more.
(setenv "PAGER" (executable-find "cat"))

;; Use home as the default directory.
(setq default-directory (getenv "HOME"))

;; Start fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Remove initial scratch message
(setq initial-scratch-message "")

;; Add line numbers
(global-linum-mode 1)

;; Save all backup files in /backups.
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
;; Save current session
(desktop-save-mode t)

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

;; Fix indentation in js-mode.
(setq js-indent-level 2)

;; fix popup menus width
(setq ac-max-width 0.5)

(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)
(global-set-key (kbd "M-P") 'move-line-up)
(global-set-key (kbd "M-N") 'move-line-down)

;;;;;;;;;;;;
;; THEMES ;;
;;;;;;;;;;;;


(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

;;; init.el ends here
