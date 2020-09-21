;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; MihailDono's Configuration
;; Uses Steve Purcell's config as base
;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages

(require 'init-path)
;;----------------------------------------------------------------------------
;; Packages
;;----------------------------------------------------------------------------

(require 'init-flycheck)
(require 'init-smex)
(require 'init-ivy)
(require 'init-projectile)
(require 'init-company)
(require 'init-eglot)
(require 'init-grep)
(require 'init-git)
(require 'init-dumb-jump)

(require 'init-javascript)
(require 'init-web)
(require 'init-css)
(require 'init-markdown)

(require 'init-editing-utils)
(require 'init-themes)
(require 'init-windows)
(require 'init-settings)
(require 'init-modeline)

(provide 'init)

;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
