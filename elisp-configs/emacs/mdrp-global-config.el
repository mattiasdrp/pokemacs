;;; mdrp-global-config.el --- -*- lexical-binding: t -*-

;; Copyright (c) 2020-2020 Mattias and contributors.

;; Author: Mattias
;; Maintainer: Mattias <mattias@email.com>
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

;; DefBindings
;; Unbind unneeded keys
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "M-m") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "M-/") nil)
;; Truncate lines
(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)
;; Adjust font size like web browsers
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
;; Move up/down paragraph
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)

(define-key input-decode-map [?\C-m] [C-m])
(define-key input-decode-map [?\C-i] [C-i])

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; -DefBindings

;; Loading custom-file containing all the custom variables and faces
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; (set-fontset-font t '(#xe3d0 . #xe3d4) "Material Icons")

(unless *sys/win32*
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
;; -UTF8Coding

;; Delete trailing whitespaces when saving:
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Wraps automatically too long lines:
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq frame-title-format '(buffer-file-name "%b (%f)" "%b"))

;; Asks y/n instead of yes/no (faster):
(fset 'yes-or-no-p 'y-or-n-p)

;; Replace selection on insert
(delete-selection-mode 1)

;; When buffer is closed, saves the cursor location
(save-place-mode 1)

;; SmallConfigs
;; Move the backup fies to user-emacs-directory/.backup
(setq backup-directory-alist `(("." . ,(expand-file-name ".backup" user-emacs-directory))))

;; Turn Off Cursor Alarms
(setq ring-bell-function 'ignore)

;; Show Keystrokes in Progress Instantly
(setq echo-keystrokes 0.1)

;; Don't Lock Files
(setq-default create-lockfiles nil)

;; Better Compilation
(setq-default compilation-always-kill t) ; kill compilation process before starting another

(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'

(setq-default compilation-scroll-output t)

;; So Long mitigates slowness due to extremely long lines.
;; Currently available in Emacs master branch *only*!
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;; Default .args, .in, .out files to text-mode
(add-to-list 'auto-mode-alist '("\\.in\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.args\\'" . text-mode))
;; -SmallConfigs

;; Adapting the compilation directory matcher for french:
;; (setq compilation-directory-matcher '("\\(?:Entering\\|Leavin\\(g\\)\\|\\) directory [`']\\(.+\\)'$" (2 . 1)))
;; (setq compilation-directory-matcher '("\\(?:on entre dans le\\|on quitte l\\(e\\)\\|\\) répertoire « \\(.+\\) »$" (2 . 1)))
;; (setq compilation-page-delimiter "\\(?:on entre dans le\\|on quitte l\\(e\\)\\|\\) répertoire « \\(.+\\) »$")

(provide 'mdrp-global-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-global-config.el ends here
