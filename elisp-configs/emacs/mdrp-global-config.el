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

;; Please oh please emacs, stop creating files everywhere, just put them in one place
(require 'no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "backups/") t)))

;; Loading custom-file containing all the custom variables and faces
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Auto-save the visited file, don't create a #file#
(auto-save-visited-mode 1)
(setq auto-save-default nil)
(setq auto-save-timeout 600)

(set-fontset-font t '(#xe3d0 . #xe3d4) "Material Icons")

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
(setq-default compilation-context-lines 2)
(setq-default compilation-error-screen-columns nil)
(setq-default compilation-scroll-output nil)
(setq-default compilation-window-height 12)
(setq line-move-visual t)
(setq next-error-highlight t)
(setq next-error-highlight-no-select t)


;; So Long mitigates slowness due to extremely long lines.
;; Currently available in Emacs master branch *only*!
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)
;; (setq save-interprogram-paste-before-kill t)

;; Default .args, .in, .out files to text-mode
(add-to-list 'auto-mode-alist '("\\.in\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.args\\'" . text-mode))
;; -SmallConfigs

;;; SET VARIABLES

(setq blink-matching-paren t)
(setq blink-matching-paren-dont-ignore-comments t)
(setq blink-matching-paren-on-screen t)

(setq column-number-mode t)
(setq comment-style 'indent)

(setq ansi-color-names-vector
      ["#424242" "#EF9A9A" "#C5E1A5" "#FFEE58" "#64B5F6" "#E1BEE7" "#80DEEA" "#E0E0E0"])

(setq enable-recursive-minibuffers t)
(setq electric-indent-mode nil)
(setq-default font-lock-global-modes t)
(setq-default global-auto-revert-mode t)
(global-prettify-symbols-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default inhibit-startup-screen t)
(setq-default initial-major-mode 'text-mode)
(save-place-mode 1)
(setq scroll-bar-mode nil)
(setq sentence-end-double-space nil)
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(setq-default show-trailing-whitespace t)
(setq tool-bar-mode nil)
(setq truncate-lines nil)
(setq visible-bell t)

;; Adapting the compilation directory matcher for french:
;; (setq compilation-directory-matcher '("\\(?:Entering\\|Leavin\\(g\\)\\|\\) directory [`']\\(.+\\)'$" (2 . 1)))
;; (setq compilation-directory-matcher '("\\(?:on entre dans le\\|on quitte l\\(e\\)\\|\\) répertoire « \\(.+\\) »$" (2 . 1)))
;; (setq compilation-page-delimiter "\\(?:on entre dans le\\|on quitte l\\(e\\)\\|\\) répertoire « \\(.+\\) »$")

(provide 'mdrp-global-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-global-config.el ends here
