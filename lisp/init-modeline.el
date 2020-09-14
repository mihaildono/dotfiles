;;; init-modeline.el --- Custom Emacs modeline -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(define-minor-mode minor-mode-blackout-mode
  "Hides minor modes from the mode line."
  t)

(catch 'done
  (mapc (lambda (x)
          (when (and (consp x)
                     (equal (cadr x) '("" minor-mode-alist)))
            (let ((original (copy-sequence x)))
              (setcar x 'minor-mode-blackout-mode)
              (setcdr x (list "" original)))
            (throw 'done t)))
        mode-line-modes))


(provide 'init-modeline)
;;; init-modeline.el ends here
