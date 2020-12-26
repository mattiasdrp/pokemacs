;;; mdrp-crux.el --- -*- lexical-binding: t -*-

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

(use-package crux
  :init
  (define-prefix-command 'mdrp-crux-map nil "Crux-")
  :bind-keymap ("M-m" . mdrp-crux-map)
  :bind (
         ("C-a" . crux-move-beginning-of-line)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-x K" . crux-kill-other-buffers)
         ("C-k" . crux-smart-kill-line)
         ("M-u" . crux-upcase-region)
         ("M-d" . crux-downcase-region)
         ("M-c" . crux-capitalize-region)
         (:map mdrp-crux-map
               ("w" . crux-view-url)                ; Open a new buffer containing the contents of URL.
               ("o" . crux-open-with)               ; Open visited file in default external program.
               ("e" . crux-sudo-edit)               ; Edit currently visited file as root.
               ("i" . crux-insert-date)             ; Insert a timestamp according to locale's date and time format.
               ("t" . crux-transpose-windows)       ; Transpose the buffers shown in two windows.
               ("j" . crux-top-join-line)           ; Join the current line with the line beneath it.
               ("u" . crux-upcase-region)           ; `upcase-region' when `transient-mark-mode' is on and region is active.
               ("d" . crux-downcase-region)         ; `downcase-region' when `transient-mark-mode' is on and region is active.
               ("c" . crux-capitalize-region)       ; `capitalize-region' when `transient-mark-mode' is on and region is active.
               ("r" . crux-recompile-init)          ; Byte-compile all your dotfiles again.
               ("k" . crux-smart-kill-line)         ; Kill to the end of the line and kill whole line on the next call.
               ("M-k" . crux-kill-line-backwards)   ; Kill line backwards and adjust the indentation.
               ("a" . crux-move-beginning-of-line)  ; Move point back to indentation/beginning (toggle) of line.
               ("s" . crux-ispell-word-then-abbrev) ; Call `ispell-word', then create an abbrev for it.
               )
         )
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(provide 'mdrp-crux)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-crux.el ends here
