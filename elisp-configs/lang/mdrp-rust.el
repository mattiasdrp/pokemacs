;;; mdrp-rust.el --- -*- lexical-binding: t -*-

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


(use-package rust-mode
  :mode "\\.rs'"
  ;; :hook (rust-mode . my/rust-mode-outline-regexp-setup)
  :bind (
         :map rust-mode-map
              ("C-M-;"                   . rust-doc-comment-dwim-following)
              ("C-M-,"                   . rust-doc-comment-dwim-enclosing)
              ("C-c C-t"                 . racer-find-definition)
              )

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
  :config
  (setq racer-rust-src-path (concat (getenv "HOME") "/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library"))
)

(use-package eldoc
  :hook (racer-mode . eldoc-mode))

(use-package toml-mode
  :hook cargo)

(use-package flycheck-rust
  :hook (rust-mode . flycheck-rust-setup))

(provide 'mdrp-rust)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-rust.el ends here
