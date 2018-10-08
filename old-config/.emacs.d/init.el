;; Set up load path.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Free personal key bindings space.
(global-unset-key (kbd "M-j"))


(require 'setup-settings)
(require 'setup-packages)
(require 'setup-keybindings)

(require 'setup-themes)
(require 'setup-defuns)

;; Always keep init.el buffer.
(find-file "~/.emacs.d/init.el")

;; Don't need the *scratch* buffer.
(kill-buffer "*scratch*")

;; Start server.
(require 'server)
(unless (server-running-p)
  (server-start))
