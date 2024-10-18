;;; flycheck-correct.el --- Emacs Minor Mode for correcting at point with flycheck

;; Copyright (C) 2021  mattiasdrp


;; Author: mattiasdrp
;; Maintainer: mattiasdrp <https://github.com/mattiasdrp>
;; Created: 17 august 2022
;; Version: 0.1.0
;; Licence: MIT
;; Keywords: emacs, tools
;; URL: https://github.com/mattiasdrp/pokemacs
;; Copyright (c) 2022 mattiasdrp and contributors.

;;; Commentary:
;;
;; Flycheck correct mode.

;;; Code:

(require 'flycheck)

(defun pokemacs-error-at-point (pos)
  "Correct the first error encountered at POS.

  This method replace the word at POS by the first suggestion coming from
  flycheck, if any."
  (let ((error (car-safe (flycheck-overlay-errors-at pos))))
    (when error
      (message "Error: %S" error))))

(defun pokemacs-correct-or-newline (pos)
  "Doc POS."
  (interactive "d")
  (unless (pokemacs-error-at-point pos)
    (newline)))

;;;###autoload
(define-minor-mode flycheck-correct-mode
  "Flycheck correct mode."
  :init-value nil
  :lighter " fly-c")

(provide 'flycheck-correct)

;;; flycheck-correct.el ends here
