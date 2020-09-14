;;; init-eglot.el --- LSP intellisense  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'eglot)
  ;; server-programs becomes void if this line is missing
  (require 'eglot)
  (add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio")))
  (add-hook 'js-mode-hook 'eglot-ensure)
  ;; disabled flymake, so you can only use flycheck
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1))))

(provide 'init-eglot)
;;; init-eglot.el ends here
