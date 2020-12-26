;;; mdrp-selected.el --- -*- lexical-binding: t -*-
;;
;; Description: Initialize Selected
;; Author: Mattias
;; Copyright (C) 2020 Mattias
;; Version: 1.0
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes selected
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

(use-package selected
  :hook (after-init . selected-global-mode)
  :bind (:map selected-keymap
              ("C-?"                     . hydra-selected/body)
              ("<"                       . mc/mark-previous-like-this)
              (">"                       . mc/mark-next-like-this)
              ("C-<"                     . mc/unmark-previous-like-this)
              ("C->"                     . mc/unmark-next-like-this)
              ("M-<"                     . mc/skip-to-previous-like-this)
              ("M->"                     . mc/skip-to-next-like-this)
              ("C-c >"                   . mc/edit-lines)
              ("M-r c"                   . capitalize-region)
              ("M-r l"                   . downcase-region)
              ("M-r u"                   . upcase-region)
              ("C-f"                     . fill-region)
              ("C-q"                     . selected-off)
              ("C-s r"                   . reverse-region)
              ("C-s s"                   . sort-lines)
              ("C-s w"                   . mdrp/sort-words)
              )
  )

(provide 'mdrp-selected)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-selected.el ends here
