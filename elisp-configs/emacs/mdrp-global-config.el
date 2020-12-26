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

(require 'server)
(unless (server-running-p) (server-start))

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

(when (and (eq system-type 'gnu/linux)
           (string-match
            "Linux.*Microsoft.*Linux"
            (shell-command-to-string "uname -a")))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))

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

;; SmallConfigs
;; Move the backup fies to user-emacs-directory/.backup

;; Don't Lock Files
(setq-default
 create-lockfiles nil
 compilation-always-kill t ; kill compilation process before starting another
 compilation-ask-about-save nil ; save all buffers on `compile'
 compilation-context-lines 2
 compilation-error-screen-columns nil
 compilation-scroll-output nil
 compilation-window-height 12
 initial-scratch-message ""
 select-enable-clipboard t
 font-lock-global-modes t
 global-auto-revert-mode t
 indent-tabs-mode nil
 inhibit-startup-screen t
 initial-major-mode 'text-mode
 )

;;; SET VARIABLES

(setq
 line-move-visual t
 next-error-highlight t
 next-error-highlight-no-select t
 ;; Add a newline automatically at the end of the file upon save.
 require-final-newline t
 blink-matching-paren t
 blink-matching-paren-dont-ignore-comments t
 blink-matching-paren-on-screen t
 column-number-mode t
 comment-style 'indent
 ansi-color-names-vector ["#424242" "#EF9A9A" "#C5E1A5" "#FFEE58" "#64B5F6" "#E1BEE7" "#80DEEA" "#E0E0E0"]
 enable-recursive-minibuffers t
 electric-indent-mode nil
 scroll-bar-mode nil
 sentence-end-double-space nil
 show-paren-delay 0
 show-paren-style 'expression
 tool-bar-mode nil
 truncate-lines nil
 visible-bell t
 prescient-persist-mode t
 backup-directory-alist `(("." . ,(expand-file-name ".backup" user-emacs-directory)))
;; Turn Off Cursor Alarms
 ring-bell-function 'ignore
;; Show Keystrokes in Progress Instantly
 echo-keystrokes 0.1)

;;; GLOBAL MODES

(global-prettify-symbols-mode 1)
(save-place-mode 1)
(show-paren-mode 1)
;; Replace selection on insert
(delete-selection-mode 1)
;; When buffer is closed, saves the cursor location
(save-place-mode 1)

;; So Long mitigates slowness due to extremely long lines.
;; Currently available in Emacs master branch *only*!
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; (setq save-interprogram-paste-before-kill t)

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
