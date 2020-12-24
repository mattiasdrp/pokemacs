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
  )

(use-package outline-ivy
  :load-path "custom/"
  :after (outline ivy)
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
