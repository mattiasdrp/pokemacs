;;; mdrp-latex.el --- -*- lexical-binding: t -*-

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (reftex-isearch-minor-mode)
  (setq TeX-PDF-mode t)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server t)
  )

(use-package auctex-latexmk
  :after tex-site
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  )

(use-package LaTeX-math-mode
  :hook auctex
  )

(provide 'mdrp-latex)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-latex.el ends here
