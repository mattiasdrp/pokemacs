;;; mdrp-flycheck.el --- -*- lexical-binding: t -*-
;;
;; Description: Initialize Flycheck
;; Author: mdrp
;; Copyright (C) 2020 mdrp
;; Version: 1.0
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes flycheck
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

;; Enabled when in prog mode
(use-package flycheck
  :hook ((prog-mode markdown-mode) . flycheck-mode)
  )

;; Quick-peek:

;; Will be used to allow seeing the inline flycheck in a stylised way
(use-package quick-peek
  :ensure t
  )

;; Flycheck inline mode:

;; Enabled when Flycheck is enabled
(use-package flycheck-inline
  :hook (flycheck-mode . flycheck-inline-mode)
  :config
  (setq flycheck-inline-display-function
        (lambda (msg pos err)
          (let* ((ov (quick-peek-overlay-ensure-at pos))
                 (contents (quick-peek-overlay-contents ov)))
            (setf (quick-peek-overlay-contents ov)
                  (concat contents (when contents "\n") msg))
            (quick-peek-update ov)))
        flycheck-inline-clear-function #'quick-peek-hide)
  )

(provide 'mdrp-flycheck)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-flycheck.el ends here
