;;; dune-minor.el --- Emacs Minor Mode for Dune, Rust's Package Manager.

;; Copyright (C) 2021  mattiasdrp

;; Author: Mattias <5543639+mattiasdrp@users.noreply.github.com>
;; Version  : 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Dune Minor mode.
;; Provides a number of key combinations and functions for managing Dune.
;; Current supported Dune Key Combinations:
;;  * C-c C-c C-e - dune-process-bench
;;  * C-c C-c C-b - dune-process-build
;;  * C-c C-c C-l - dune-process-clean
;;  * C-c C-c C-d - dune-process-doc
;;  * C-c C-c C-v - dune-process-doc-open
;;  * C-c C-c C-n - dune-process-new
;;  * C-c C-c C-i - dune-process-init
;;  * C-c C-c C-r - dune-process-run
;;  * C-c C-c C-x - dune-process-run-example
;;  * C-c C-c C-s - dune-process-search
;;  * C-c C-c C-t - dune-process-test
;;  * C-c C-c C-u - dune-process-update
;;  * C-c C-c C-c - dune-process-repeat
;;  * C-c C-c C-f - dune-process-current-test
;;  * C-c C-c C-o - dune-process-current-file-tests
;;  * C-c C-c C-O - dune-process-outdated
;;  * C-c C-c C-m - dune-process-fmt
;;  * C-c C-c C-k - dune-process-check
;;  * C-c C-c C-K - dune-process-clippy
;;  * C-c C-c C-a - dune-process-add
;;  * C-c C-c C-D - dune-process-rm
;;  * C-c C-c C-U - dune-process-upgrade
;;  * C-c C-c C-A - dune-process-audit

;;
;;; Code:

(require 'dune-process)

(defgroup dune-cc nil
  "Dune group."
  :prefix "dune-cc-"
  :group 'tools)

(defvar dune-minor-mode-command-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "b") 'dune-process-build)
    (define-key km (kbd "l") 'dune-process-clean)
    (define-key km (kbd "r") 'dune-process-exec-name)
    (define-key km (kbd "i") 'dune-process-init)
    km)
  "Keymap for Dune mode commands after prefix.")
(fset 'dune-minor-mode-command-map dune-minor-mode-command-map)

(defvar dune-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'dune-minor-mode-command-map)
    map)
  "Keymap for Dune mode.")

;; (defvar dune-minor-mode nil)

;;;###autoload
(define-minor-mode dune-minor-mode
  "Dune minor mode. Used to hold keybindings for dune-mode.

\\{dune-minor-mode-command-map}"
  :init-value nil
  :lighter " dune-cc"
  :keymap dune-minor-mode-map)

(provide 'dune-minor)
;;; dune-minor.el ends here
