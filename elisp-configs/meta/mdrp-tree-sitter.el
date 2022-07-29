;;; mdrp-tree-sitter.el --- -*- lexical-binding: t -*-

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

(use-package tree-sitter-langs
  :ensure t)

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  ;; :hook
  ;; (tuareg-mode . tree-sitter-mode)
  ;; (python-mode . tree-sitter-mode)
)

;; (use-package tree-sitter-fold
;;   :load-path "custom/tree-sitter-fold/"
;;   :hook
;;   (tuareg-mode . tree-sitter-fold-mode)
;;   (python-mode . tree-sitter-fold-mode)
;;   )

(use-package ts-fold
  :load-path "custom/ts-fold/"
  :hook
  (tuareg-mode . ts-fold-mode)
  (c++-mode    . ts-fold-mode)
  (python-mode . ts-fold-mode)
  )

(use-package ts-fold-indicators
  :load-path "custom/ts-fold/"
  :config
  (setq ts-fold-indicators-fringe 'left-fringe)
  (setq ts-fold-indicators-priority 100)
  :hook
  (tree-sitter-after-on . ts-fold-indicators-mode)
  )

(provide 'mdrp-tree-sitter)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-tree-sitter.el ends here
