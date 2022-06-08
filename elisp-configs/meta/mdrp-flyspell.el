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
  :init

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

  (defun mdrp/flyspell-on-for-buffer-type ()
    "Enable Flyspell appropriately for the major mode of the current buffer.
Uses `flyspell-prog-mode' for modes derived from `prog-mode', so only strings
and comments get checked.  All other buffers get `flyspell-mode' to check
all text.  If flyspell is already enabled, does nothing."
    (interactive)
    (if (not (symbol-value flyspell-mode)) ; if not already on
	(progn
	  (if (derived-mode-p 'prog-mode)
	      (progn
	        (message "Flyspell on (code)")
	        (flyspell-prog-mode))
	    ;; else
	    (progn
	      (message "Flyspell on (text)")
	      (flyspell-mode 1)))
	  )
      (flyspell-buffer)))

  (defun mdrp/flyspell-toggle ()
    "Turn Flyspell on if it is off, or off if it is on.  When turning on,
it uses `flyspell-on-for-buffer-type' so code-vs-text is handled appropriately."
    (interactive)
    (if (symbol-value flyspell-mode)
	(progn ; flyspell is on, turn it off
	  (message "Flyspell off")
	  (flyspell-mode -1))
                                        ; else - flyspell is off, turn it on
      (mdrp/flyspell-on-for-buffer-type)))

  :ensure t
  :hook (find-file . mdrp/flyspell-on-for-buffer-type)
  :bind-keymap ("M-f" . mdrp-flyspell-map)
  ("C-f" . mdrp-flyspell-map)
  :bind (
         (:map mdrp-flyspell-map
               ("t" . mdrp/flyspell-toggle)
               ("f" . mdrp/french-dict)
               ("e" . mdrp/english-dict)
               )
         )
  :config
  (provide 'ispell) ; forcibly load ispell configs
  (define-prefix-command 'mdrp-flyspell-map nil "Flyspell-")

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

(use-package flyspell-correct
  :after flyspell
  :general
  (popup-menu-keymap
   "<return>" 'popup-select)
  :bind (
         (:map mdrp-flyspell-map
               ("C-f" . flyspell-correct-wrapper)
               ))
  )

(use-package flyspell-correct-ivy
  :after (flyspell-correct)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package flyspell-correct-popup
  :after (flyspell-correct-ivy)
  :config
  (define-key popup-menu-keymap [escape] #'keyboard-quit)
  )

(provide 'mdrp-flyspell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-flyspell.el ends here
