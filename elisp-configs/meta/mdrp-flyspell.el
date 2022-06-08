;;; mdrp-flyspell.el --- -*- lexical-binding: t -*-

;; Copyright (c) 2020-2020 mdrp and contributors.

;; Author: mdrp
;; Maintainer: mdrp <https://github.com/mattiasdrp>
;; Version: 1.0
;; Licence: GPL2+
;; Keywords: convenience, configuration

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(use-package flyspell
    :ensure t
    :hook (
           (
           (org-mode-hook
            markdown-mode-hook
            TeX-mode-hook
            rst-mode-hook
            mu4e-compose-mode-hook
            message-mode-hook
            git-commit-mode-hook) . flyspell-mode)
           (prog-mode . flyspell-prog-mode))

    :config
    (provide 'ispell) ; forcibly load ispell configs

    (setq flyspell-issue-welcome-flag nil
          ;; Significantly speeds up flyspell, which would otherwise print
          ;; messages for every word when checking the entire buffer
          flyspell-issue-message-flag nil)

    (add-hook 'flyspell-mode-hook
              (defun +spell-inhibit-duplicate-detection-maybe-h ()
                "Don't mark duplicates when style/grammar linters are present.
e.g. proselint and langtool."
                (and (or (and (bound-and-true-p flycheck-mode)
                              (executable-find "proselint"))
                         (featurep 'langtool))
                     (setq-local flyspell-mark-duplications-flag nil))))
    (defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
  (let* ((rlt ad-return-value)
         (begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\|example\\|quote\\)")
         (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\|example\\|quote\\)")
         (case-fold-search t)
         b e)
    (when ad-return-value
      (save-excursion
        (setq b (re-search-backward begin-regexp nil t))
        (if b (setq e (re-search-forward end-regexp nil t))))
      (if (and b e (< (point) e)) (setq rlt nil)))
    (setq ad-return-value rlt)))
    )

(defun mdrp/english-dict ()
  "Change dictionary to english."
  (interactive)
  (setq ispell-local-dictionary "english")
  (flyspell-mode 1)
  (flyspell-buffer))

(defun mdrp/french-dict ()
  "Change dictionary to french."
  (interactive)
  (setq ispell-local-dictionary "french")
  (flyspell-mode 1)
  (flyspell-buffer))

(use-package flyspell-correct
  :commands flyspell-correct-previous
  :config
  (require 'flyspell-correct-ivy)
  (require 'flyspell-correct-popup)
  (setq flyspell-popup-correct-delay 0.8)
  (define-key popup-menu-keymap [escape] #'keyboard-quit))


(provide 'mdrp-flyspell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-flyspell.el ends here
