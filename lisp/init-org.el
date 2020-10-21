;;; init-org.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Agenda

(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-agenda-files '("~/.emacs.d/org/notes/"))
(setq org-agenda-todo t)

;; Capturing
(global-set-key (kbd "C-c c") 'org-capture)

;; Save file as <name>-<time>.org
(defun my/generate-org-note-name ()
  (setq my-org-note--name (read-string "Name: "))
  (setq my-org-note--time (format-time-string "%d-%m-%Y"))
  (expand-file-name (format "%s-%s.org" my-org-note--time my-org-note--name) "~/.emacs.d/org/notes/"))

;; Add TODO keyword and add to agenda for scheduled time
(setq org-capture-templates
  '(("n" "note" plain
     (file my/generate-org-note-name)
     "%(format \"* TODO %s\nSCHEDULED:%(org-insert-time-stamp (org-read-date nil t) nil nil nil nil)\n\" my-org-note--name)")))

;;; To-do settings
(setq org-todo-keywords
      '((sequence "TODO(t)" "CANCELLED(n)" "|" "DONE(d!/!)")))

(setq org-todo-keyword-faces
      (quote (("TODO" :inherit warning))))

;; Various preferences
(setq org-log-done t
      org-tags-column 80
      org-directory "~/.emacs.d/org")

;;; Archiving

(setq org-archive-location "%s_archive::* Archive")

;; Pomodoro
(require-package 'org-pomodoro)
(setq org-pomodoro-keep-killed-pomodoro-time t)
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))

(provide 'init-org)
;;; init-org.el ends here
