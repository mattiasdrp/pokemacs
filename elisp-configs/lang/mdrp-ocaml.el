;;; mdrp-ocaml.el --- -*- lexical-binding: t -*-

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

(use-package tuareg
  :mode ("\\.ml\\'" . tuareg-mode)
  :custom
  (tuareg-other-file-alist
  '(("\\.\\(?:pp\\.\\)?mli\\'" (".ml" ".mll" ".mly" ".pp.ml"))
    ("_intf\\.ml\\'" (".ml"))
    ("\\.\\(?:pp\\.\\)?ml\\'" (".mli" "_intf.ml"))
    ("\\.mll\\'" (".mli"))
    ("\\.mly\\'" (".mli"))
    ("\\.eliomi\\'" (".eliom"))
    ("\\.eliom\\'" (".eliomi"))))
  :bind (:map tuareg-mode-map
              ("C-c C-t" . nil)
              ("C-c C-w" . nil)
              ("C-c C-l" . nil))
  :config
  ;; Use opam to set environment
  (setq tuareg-opam-insinuate t)
  (setq tuareg-electric-indent t)

  (tuareg-opam-update-env (tuareg-opam-current-compiler))
  (defun update-opam-env (&rest _)
    (when (derived-mode-p 'tuareg-mode)
      (tuareg-opam-update-env nil)
      )
    )

  (defun update-load-path-opam (&rest _)
    (when (derived-mode-p 'tuareg-mode)
      (let ((opam-share
             (let ((reply (opam-shell-command-to-string "opam var share --safe")))
               (when reply (substring reply 0 -1)))))
        (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
      )
      )
    )
  (message "------ tuareg")
  (if (boundp 'window-buffer-change-functions)
      (progn
        (add-hook 'window-buffer-change-functions 'update-opam-env)
        (add-hook 'window-buffer-change-functions 'update-load-path-opam)
        )
    (progn
      (add-hook 'post-command-hook 'update-opam-env)
      (add-hook 'post-command-hook 'update-load-path-opam)
      ))

  :hook
  (tuareg-mode .
               (lambda ()
                 ;; Commented symbols are actually prettier with ligatures or just ugly
                 (setq prettify-symbols-alist
                       '(
                         ("sqrt" . ?√)
                         ("&&" . ?⋀)        ; 'N-ARY LOGICAL AND' (U+22C0)
                         ("||" . ?⋁)        ; 'N-ARY LOGICAL OR' (U+22C1)
                         ("<>" . ?≠)
                         ;; Some greek letters for type parameters.
                         ("'a" . ?α)
                         ("'b" . ?β)
                         ("'c" . ?γ)
                         ("'d" . ?δ)
                         ("'e" . ?ε)
                         ("'f" . ?φ)
                         ("'i" . ?ι)
                         ("'k" . ?κ)
                         ("'m" . ?μ)
                         ("'n" . ?ν)
                         ("'o" . ?ω)
                         ("'p" . ?π)
                         ("'r" . ?ρ)
                         ("'s" . ?σ)
                         ("'t" . ?τ)
                         ("'x" . ?ξ)
                         ("fun" . ?λ)
                         ("not" . ?¬)
                         (":=" . ?⇐)
                         )
                       )
                 )
               )
  )

(use-package tuareg-menhir
  :ensure nil ;; already present in tuareg
  :mode ("\\.mly'" . tuareg-menhir-mode)
  )

(use-package dune-minor
  :load-path "custom/"
  :hook (tuareg-mode . dune-minor-mode))

(use-package dune-mode
  :ensure nil ;; already present in tuareg
  :mode ("^dune$" "^dune-project$")
  )

(provide 'mdrp-ocaml)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-ocaml.el ends here
