;;; init.el --- -*- lexical-binding: t -*-

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

;;;; A BIG BUNCH OF CUSTOM OPTIONS

;; Get rid of the cl is deprecated warning
(setq byte-compile-warnings '(cl-functions))

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

;; Packages

;; Package Management
(require 'mdrp-packages)

;; Global Functionalities
(require 'mdrp-global-config)

;; Global functions
(require 'mdrp-functions)

(require 'mdrp-crux)

(require 'mdrp-winner)

(require 'mdrp-keybindings)

(require 'mdrp-which-key)

;;;; UI

(update-to-load-path (expand-file-name "elisp-configs/ui" user-emacs-directory))

;; The bar below with some infos
(require 'mdrp-doom-modeline)

(require 'mdrp-minions)

;; Navigating through any file
(require 'mdrp-outline)

(require 'mdrp-ligatures)

;;;;; Themes, colors and other small things

(require 'mdrp-rainbow-mode)

(require 'mdrp-rainbow-delimiters)

(require 'mdrp-ansi-color)

(require 'mdrp-apropospriate-theme)

(require 'mdrp-all-the-icons)

(require 'mdrp-nlinum)

(require 'mdrp-highlight)

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

;;;; LANGUAGE SPECIFIC PACKAGES:

(update-to-load-path (expand-file-name "elisp-configs/lang" user-emacs-directory))

;;;;; LaTeX:

(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              ;; (rainbow-delimiters-mode)
              ;; (pretty-outlines-add-bullets)
              (company-mode)
              ;; (smartparens-mode)
              (turn-on-reftex)
              (setq reftex-plug-into-AUCTeX t)
              (reftex-isearch-minor-mode)
              (setq TeX-PDF-mode t)
              (setq TeX-source-correlate-method 'synctex)
              (setq TeX-source-correlate-start-server t)))
  )

(use-package LaTeX-math-mode
  :hook tex-site
  )

;;;;; Cubicle:

(use-package cubicle-mode
  :mode "\\.cub$"
  )

;;;;; Why3:

(use-package why3-mode
  :load-path "custom/"
  :mode "\\.\\(\\(mlw\\)\\|\\(why\\)\\)$"
  )

;;;;; Dune:

(use-package dune-mode
  :mode ("dune" "dune-project")
  )

;;;;; Rust:

(use-package rust-mode
  :mode "\\.rs'"
  :bind ("C-M-;" . rust-doc-comment-dwim-following)
  :bind ("C-M-," . rust-doc-comment-dwim-enclosing)
  ;; :hook (rust-mode . my/rust-mode-outline-regexp-setup)
  :config
  (setq rust-format-on-save t)
  ;; (defun my/rust-mode-outline-regexp-setup ()
  ;;   (setq-local outline-regexp "///[;]\\{1,8\\}[^ \t]"))
  (defun rust-doc-comment-dwim (c)
    "Comment or uncomment the current line or text selection."
    (interactive)

    ;; If there's no text selection, comment or uncomment the line
    ;; depending whether the WHOLE line is a comment. If there is a text
    ;; selection, using the first line to determine whether to
    ;; comment/uncomment.
    (let (p1 p2)
      (if (use-region-p)
          (save-excursion
            (setq p1 (region-beginning) p2 (region-end))
            (goto-char p1)
            (if (wholeLineIsCmt-p c)
                (my-uncomment-region p1 p2 c)
              (my-comment-region p1 p2 c)
              ))
        (progn
          (if (wholeLineIsCmt-p c)
              (my-uncomment-current-line c)
            (my-comment-current-line c)
            )) )))

  (defun wholeLineIsCmt-p (c)
    (save-excursion
      (beginning-of-line 1)
      (looking-at (concat "[ \t]*//" c))
      ))

  (defun my-comment-current-line (c)
    (interactive)
    (beginning-of-line 1)
    (insert (concat "//" c))
    )

  (defun my-uncomment-current-line (c)
    "Remove “//c” (if any) in the beginning of current line."
    (interactive)
    (when (wholeLineIsCmt-p c)
      (beginning-of-line 1)
      (search-forward (concat "//" c))
      (delete-backward-char 4)
      ))

  (defun my-comment-region (p1 p2 c)
    "Add “//c” to the beginning of each line of selected text."
    (interactive "r")
    (let ((deactivate-mark nil))
      (save-excursion
        (goto-char p2)
        (while (>= (point) p1)
          (my-comment-current-line c)
          (previous-line)
          ))))

  (defun my-uncomment-region (p1 p2 c)
    "Remove “//c” (if any) in the beginning of each line of selected text."
    (interactive "r")
    (let ((deactivate-mark nil))
      (save-excursion
        (goto-char p2)
        (while (>= (point) p1)
          (my-uncomment-current-line c)
          (previous-line) )) ))

  (defun rust-doc-comment-dwim-following ()
    (interactive)
    (rust-doc-comment-dwim "/ "))
  (defun rust-doc-comment-dwim-enclosing ()
    (interactive)
    (rust-doc-comment-dwim "! "))
  )

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package racer
  :hook (rust-mode . racer-mode)
  :bind ("C-c C-t" . 'racer-find-definition))

(use-package eldoc
  :hook (racer-mode . eldoc-mode))

(use-package toml-mode
  :hook cargo)

(use-package flycheck-rust
  :hook (rust-mode . flycheck-rust-setup))

;;;;; OCaml:

(require 'mdrp-ocaml)

;;;;; Fsharp

(use-package fsharp-mode
  :config (require 'eglot-fsharp)
  :defer t
  :ensure t)

(use-package eglot
  :ensure t
  :commands (eglot eglot-ensure)
  :hook ((fsharp-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         )
  :config
  (setq eglot-confirm-server-initiated-edits nil)
  )

;;;;; Markdown:

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . gfm-mode)))

(use-package pandoc-mode
  :hook ((markdown-mode . pandoc-mode)
         (pandoc-mode . pandoc-load-default-settings)))

;;;;; Web:

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  :bind (:map web-mode-map
              ([backtab] . company-complete))
  )

;;;;; CSS:

(use-package css-mode
  :ensure nil
  :mode "\\.css\\'")

;;;;; JSON:

(use-package json-mode
  :mode (("\\.bowerrc$"     . json-mode)
         ("\\.jshintrc$"    . json-mode)
         ("\\.json_schema$" . json-mode)
         ("\\.json\\'" . json-mode))
  :bind (:package json-mode-map
                  :map json-mode-map
                  ("C-c <tab>" . json-mode-beautify))
  :config
  (make-local-variable 'js-indent-level))

;;;; Footer

;; End:
(provide 'init)

;;; init.el ends here
