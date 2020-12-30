;;; mdrp-hydra.el --- -*- lexical-binding: t -*-
;;
;; Description: Initialize Hydra
;; Author: mdrp
;; Copyright (C) 2020 mdrp
;; Version: 1.0
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes hydra
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

(use-package hydra
  :custom
  (hydra-default-hint nil)
  )

(defhydra hydra-dates (:color teal)
  (concat "\n " (mdrp/hydra-heading "Dates" "Insert" "Insert with Time")
          "
 _q_ quit              _d_ short             _D_ short             ^^
 ^^                    _i_ iso               _I_ iso               ^^
 ^^                    _l_ long              _L_ long              ^^
")
  ("q" nil)
  ("d" mdrp/date-short)
  ("D" mdrp/date-short-with-time)
  ("i" mdrp/date-iso)
  ("I" mdrp/date-iso-with-time)
  ("l" mdrp/date-long)
  ("L" mdrp/date-long-with-time))

(provide 'mdrp-hydra)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-hydra.el ends here
