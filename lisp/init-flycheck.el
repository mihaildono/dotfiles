;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)
  ;; display error messages in the echo area
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

;; config file
;; (custom-set-variables
;;  '(flycheck-typescript-tslint-config "~/tslint.json"))

;; user-created-config
;; (custom-set-variables)
;; '(flycheck-typescript-tslint-rulesdir "~/tslint.json")

(provide 'init-flycheck)
;;; init-flycheck.el ends here
