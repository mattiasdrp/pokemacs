;;; mdrp-visual-fill.el --- -*- lexical-binding: t -*-

;; Copyright (c) 2020-2020 mdrp and contributors.

;; Author: mdrp
;; Maintainer: mdrp <https://github.com/mattiasdrp>
;; Version: 1.0
;; Licence: MIT
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
;; along with this program.  If not, see <http://www.gnu.visual-fill/licenses/>.

;;; Commentary:

;;; Code:

(use-package visual-fill-column
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t)
  :hook ((org-mode text-mode) . visual-fill-column-mode)
  )

(provide 'mdrp-visual-fill)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-visual-fill.el ends here
