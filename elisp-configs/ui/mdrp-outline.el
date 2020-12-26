;;; mdrp-outline.el --- -*- lexical-binding: t -*-

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

(use-package outline
  :hook ((prog-mode . outline-minor-mode))
  :config
  (define-prefix-command 'cm-map nil "Outline-")
  (set-display-table-slot standard-display-table
                          'selective-display
                          (string-to-vector "+++"))
  :bind-keymap ("C-o" . cm-map)
  :bind (:map cm-map
              ;; HIDE
              ("q" . outline-hide-sublevels)    ; Hide everything but the top-level headings
              ("t" . outline-hide-body)         ; Hide everything but headings (all body lines)
              ("o" . outline-hide-other)        ; Hide other branches
              ("c" . outline-hide-entry)        ; Hide this entry's body
              ("l" . outline-hide-leaves)       ; Hide body lines in this entry and sub-entries
              ("d" . outline-hide-subtree)      ; Hide everything in this entry and sub-entries
              ;; SHOW
              ("a" . outline-show-all)          ; Show (expand) everything
              ("e" . outline-show-entry)        ; Show this heading's body
              ("i" . outline-show-children)     ; Show this heading's immediate child sub-headings
              ("k" . outline-show-branches)     ; Show all sub-headings under this heading
              ("s" . outline-show-subtree)      ; Show (expand) everything in this heading & below
              ;; MOVE
              ("u" . outline-up-heading)                ; Up
              ("n" . outline-next-visible-heading)      ; Next
              ("p" . outline-previous-visible-heading)  ; Previous
              ("f" . outline-forward-same-level)        ; Forward - same level
              ("b" . outline-backward-same-level)       ; Backward - same level
              )
  )

(use-package outline-ivy
  :load-path "custom/"
  :after (outline ivy)
  :bind (:map outline-minor-mode-map
              ("M-j"                     . oi-jump)
              )
  )

(use-package outshine
  :hook (outline-minor-mode . outshine-mode)
  :config
  (setq outshine-preserve-delimiter-whitespace nil)
  )

(use-package pretty-outlines
  :defer t
  :load-path "custom/"
  :hook ((outline-mode . pretty-outlines-set-display-table)
         (outline-minor-mode . pretty-outlines-set-display-table)
         (emacs-lisp-mode . pretty-outlines-add-bullets)
         )
  )

(provide 'mdrp-outline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-outline.el ends here
