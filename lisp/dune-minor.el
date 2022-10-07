;;; dune-minor.el --- Emacs Minor Mode for Dune, OCaml's Package Manager.

;; Copyright (C) 2021  mattiasdrp


;; Author: mattiasdrp
;; Maintainer: mattiasdrp <https://github.com/mattiasdrp>
;; Created: 17 august 2022
;; Version: 0.1.0
;; Licence: MIT
;; Keywords: emacs, tools
;; URL: https://github.com/mattiasdrp/pokemacs
;; Copyright (c) 2022 mattiasdrp and contributors.

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
