;;; mdrp-god-mode.el --- -*- lexical-binding: t -*-
;;
;; Description: Initialize God-Mode
;; Author: Mattias
;; Copyright (C) 2020 Mattias
;; Version: 1.0
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes god-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package god-mode
  :init
  (setq god-mode-enable-function-key-translation nil)
  :config
  (defun my-god-mode-update-modeline ()
    (cond (god-local-mode (progn
                            (set-face-background 'mode-line "#e9e2cb")
                            (set-face-foreground 'mode-line "black")
                            (set-face-background 'mode-line-inactive "#e9e2cb")
                            (set-face-foreground 'mode-line-inactive "black")
                            ))
          (t (progn
               (set-face-background 'mode-line "#0a2832")
               (set-face-foreground 'mode-line "white")
               (set-face-background 'mode-line-inactive "#0a2832")
               (set-face-foreground 'mode-line-inactive "white")
               ))))
  (add-hook 'god-mode-enabled-hook #'my-god-mode-update-modeline)
  (add-hook 'god-mode-disabled-hook #'my-god-mode-update-modeline)
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)
  :bind (
         ("<escape>"                . god-mode-all)
         ("²"                       . god-mode-all)
         ("C-²"                     . god-mode-all)
         )
  )

(provide 'mdrp-god-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-god-mode.el ends here
