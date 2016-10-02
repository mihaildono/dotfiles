(global-linum-mode t)

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

(prelude-require-packages '(rainbow-delimiters
			    auto-complete
			    git-gutter
                            multiple-cursors))

(add-to-list 'load-path "/home/stein/.emacs.d/personal/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(ac-config-default)

(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
