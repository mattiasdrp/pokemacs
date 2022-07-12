;;; init.el --- -*- lexical-binding: t -*-

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

;;;; A BIG BUNCH OF CUSTOM OPTIONS

;; Get rid of the cl is deprecated warning
(setq byte-compile-warnings '(cl-functions))
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;;;; From MatthewZMD

;; See https://github.com/MatthewZMD/.emacs.d for the following options
;; CheckVer
(cond ((version< emacs-version "26.1")
       (warn "M-EMACS requires Emacs 26.1 and above!"))
      ((let* ((early-init-f (expand-file-name "early-init.el" user-emacs-directory))
              (early-init-do-not-edit-d (expand-file-name "early-init-do-not-edit/" user-emacs-directory))
              (early-init-do-not-edit-f (expand-file-name "early-init.el" early-init-do-not-edit-d)))
         (and (version< emacs-version "27")
              (or (not (file-exists-p early-init-do-not-edit-f))
                  (file-newer-than-file-p early-init-f early-init-do-not-edit-f)))
         (make-directory early-init-do-not-edit-d t)
         (copy-file early-init-f early-init-do-not-edit-f t t t t)
         (add-to-list 'load-path early-init-do-not-edit-d)
         (require 'early-init))))
;; -CheckVer

;; BetterGC
(defvar better-gc-cons-threshold 67108864 ; 64mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))
;; -BetterGC

;; AutoGC
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))
;; -AutoGC

;; LoadPath
(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))

(update-to-load-path (expand-file-name "elisp-configs" user-emacs-directory))

;; -LoadPath

;; END MATTHEWZMD

(require 'cl-lib)

;;;; EMACS RELATED
;; Constants, global keybindings, functions, helpers for navigating etc

(update-to-load-path (expand-file-name "elisp-configs/emacs" user-emacs-directory))

;; Useful constants
(require 'mdrp-constants)

;; Package Management
(require 'mdrp-packages)

(require 'mdrp-secrets)

;; Global Functionalities
(require 'mdrp-global-config)

(require 'mdrp-ws-butler)

;; Global functions
(require 'mdrp-functions)

(require 'mdrp-crux)

(require 'mdrp-winner)

;; KEYBINDINGS

(require 'mdrp-keybindings)

(require 'mdrp-which-key)

(require 'mdrp-hydra)

(require 'mdrp-selected)

(require 'mdrp-god-mode)

;;- KEYBINDINGS

(require 'mdrp-whitespace)

;;;; UI

(update-to-load-path (expand-file-name "elisp-configs/ui" user-emacs-directory))

;; (require 'mdrp-tab-bar)

;; The bar below with some infos
(require 'mdrp-doom-modeline)

(require 'mdrp-minions)

;; Navigating through any file
(require 'mdrp-outline)

(require 'mdrp-ligatures)

;;;;; Themes, colors and other small things


(require 'mdrp-ansi-color)

(require 'mdrp-rainbow-mode)

(require 'mdrp-rainbow-delimiters)

(require 'mdrp-theme)

(require 'mdrp-all-the-icons)

(require 'mdrp-nlinum)

(require 'mdrp-highlight)

(require 'mdrp-ace-window)

(require 'mdrp-spotify)

(require 'mdrp-visual-fill)

;;;; Completion
(update-to-load-path (expand-file-name "elisp-configs/completion" user-emacs-directory))

;; Minibuffer completion
(require 'mdrp-ivy-projectile)

(require 'mdrp-company)

;;;; META PROGRAMMING:

(update-to-load-path (expand-file-name "elisp-configs/meta" user-emacs-directory))

(require 'mdrp-separedit)

(require 'mdrp-conf-mode)

(require 'mdrp-flycheck)

(require 'mdrp-dumb-jump)               ; Jumps to definition

(require 'mdrp-hideshow)

(require 'mdrp-multiple-cursors)

(require 'mdrp-delete-block)

(require 'mdrp-flyspell)

(require 'mdrp-git)

(require 'mdrp-pdf)

;;;; ORG MODE

(update-to-load-path (expand-file-name "elisp-configs/org" user-emacs-directory))

(require 'mdrp-org)

;;;; LANGUAGE SPECIFIC PACKAGES:

(update-to-load-path (expand-file-name "elisp-configs/lang" user-emacs-directory))

(require 'mdrp-latex)

(require 'mdrp-rust)

(require 'mdrp-ocaml)

(require 'mdrp-fsharp)

(require 'mdrp-md-pandoc)

(require 'mdrp-web-modes)

(require 'mdrp-lsp)

(require 'mdrp-apheleia)

(require 'mdrp-smt)

(require 'mdrp-yaml)

(if (file-exists-p "custom/usuba-mode.el")
  (require 'mdrp-usuba)
  )

;;;; Footer

;; End:
(provide 'init)

;;; init.el ends here
