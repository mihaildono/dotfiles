;;; init-grep.el --- File Search -*- lexical-binding: t -*-
;;; Commentary:
;;; use ag for search instead of grep
;;; it is faster and more intuitive to use
;;; Code:

(when (and (executable-find "ag")
           (maybe-require-package 'ag)))

(provide 'init-grep)

;;; init-grep.el ends here
