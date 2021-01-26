;;; mdrp-ocaml.el --- -*- lexical-binding: t -*-

;; Copyright (c) 2020-2020 mdrp and contributors.

;; Author: mdrp
;; Maintainer: mdrp <https://github.com/MonsieurPi>
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

(use-package tuareg
  :custom
  (tuareg-other-file-alist
   (quote
    (("\\.mli\\'" (".ml" ".mll" ".mly"))
     ("_intf.ml\\'" (".ml"))
     ("\\.ml\\'" ("_intf.ml" ".mli"))
     ("\\.mll\\'" (".mli"))
     ("\\.mly\\'" (".mli"))
     ("\\.eliomi\\'" (".eliom"))
     ("\\.eliom\\'" (".eliomi")))))
  :config
  ;; tuareg-mode has the prettify symbols itself
  ;; (ligature-set-ligatures 'tuareg-mode '(tuareg-prettify-symbols-basic-alist))
  ;; (ligature-set-ligatures 'tuareg-mode '(tuareg-prettify-symbols-extra-alist))
  ;; harmless if `prettify-symbols-mode' isn't active
  ;; (setq tuareg-prettify-symbols-full t)
  (defun opam-shell-command-to-string (command)
    "Similar to shell-command-to-string, but returns nil unless the process
  returned 0, and ignores stderr (shell-command-to-string ignores return value)"
    (let* ((return-value 0)
           (return-string
            (with-output-to-string
              (setq return-value
                    (with-current-buffer standard-output
                      (process-file shell-file-name nil '(t nil) nil
                                    shell-command-switch command))))))
      (if (= return-value 0) return-string nil)))
  (defvar opam-share
    (let ((reply (opam-shell-command-to-string "opam config var share --safe")))
      (when reply (substring reply 0 -1))))

  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

  ;; Use opam to set environment
  (setq tuareg-opam-insinuate t)
  (setq tuareg-electric-indent t)

  (tuareg-opam-update-env (tuareg-opam-current-compiler))
  (defun update-opam-env (&rest _)
    (when (derived-mode-p 'tuareg-mode)
      (message "update")
      (tuareg-opam-update-env nil)
      )
    )
  (if (boundp 'window-buffer-change-functions)
      (add-hook 'window-buffer-change-functions 'update-opam-env)
    (add-hook 'post-command-hook 'update-opam-env))
  :hook
  (tuareg-mode .
               (lambda ()
                 ;; Commented symbols are actually prettier with ligatures or just ugly
                 (setq prettify-symbols-alist
                       '(
                         ("sqrt" . ?√)
                         ("&&" . ?⋀)        ; 'N-ARY LOGICAL AND' (U+22C0)
                         ("||" . ?⋁)        ; 'N-ARY LOGICAL OR' (U+22C1)
                         ;; ("+." . ?∔)        ;DOT PLUS (U+2214)
                         ;; ("-." . ?∸)        ;DOT MINUS (U+2238)
                         ;; ("*." . ?×)
                         ;; ("*." . ?•)   ; BULLET OPERATOR
                         ;; ("/." . ?÷)
                         ;; ("<-" . ?←)
                         ;; ("<=" . ?≤)
                         ;; (">=" . ?≥)
                         ("<>" . ?≠)
                         ;; ("==" . ?≡)
                         ;; ("!=" . ?≢)
                         ;; ("<=>" . ?⇔)
                         ;; ("infinity" . ?∞)
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
                         ;; ("[|" . ?〚)        ;; 〚
                         ;;  ("|]" . ?⟭)        ;; 〛
                         ;; ("->" . ?→)
                         (":=" . ?⇐)
                         ;; ("::" . ?∷))
                         )
                       )
                 )
               )
  )

(use-package dune-minor
  :load-path "custom/"
  :hook (tuareg-mode . dune-minor-mode))

(use-package ocamlformat
  :load-path
  (lambda ()
    (concat
     ;; Never use "/" or "\" since this is not portable (opam-user-setup does this though)
     ;; Always use file-name-as-directory since this will append the correct separator if needed
     ;; (or use a package that does it well like https://github.com/rejeep/f.el
     ;; This is the verbose and not package depending version:
     (file-name-as-directory
      ;; Couldn't find an option to remove the newline so a substring is needed
      (substring (shell-command-to-string "opam config var share --switch=ocamlformat --safe") 0 -1))
     (file-name-as-directory "emacs")
     (file-name-as-directory "site-lisp")))
  :custom
  (ocamlformat-enable 'enable-outside-detected-project)
  (ocamlformat-command
   (concat
    (file-name-as-directory
     (substring (shell-command-to-string "opam config var bin --switch=ocamlformat --safe") 0 -1))
    "ocamlformat"))
  :hook
  (tuareg-mode . (lambda () (add-hook 'before-save-hook 'ocamlformat-before-save nil 'local)))
  )

(use-package merlin
  :hook ((tuareg-mode . merlin-mode)
         (merlin-mode . company-mode))
  :custom
  (merlin-error-after-save nil)
  (merlin-completion-with-doc t)
  :config
  (add-to-list 'company-backends 'merlin-company-backend)
  (message "merlin")
  )

(use-package flycheck-ocaml
  :hook (merlin-mode . +ocaml-init-flycheck-h)
  :config
  (defun +ocaml-init-flycheck-h ()
    "Activate `flycheck-ocaml`"
    ;; Enable Flycheck checker
    (flycheck-ocaml-setup)))

(use-package merlin-eldoc
  :hook (merlin-mode . merlin-eldoc-setup)
  :custom
  (eldoc-echo-area-use-multiline-p t) ; use multiple lines when necessary
  (merlin-eldoc-max-lines 8)          ; but not more than 8)
  )

(use-package merlin-imenu
  :hook (merlin-mode . merlin-use-merlin-imenu))

(use-package ocp-indent
  ;; must be careful to always defer this, it has autoloads that adds hooks
  ;; which we do not want if the executable can't be found
  :init
  (defcustom ocp-indent-before-save nil
    "*Non nil means execute ocp-indent when saving a buffer."
    :group 'ocp-indent
    :type '(bool))
  ;; :custom
  ;; (ocp-indent-before-save t)
  :hook
  (tuareg-mode . mdrp/ocaml-init-ocp-indent-h)
  :config
  (defun mdrp/ocaml-init-ocp-indent-h ()
    "Run `ocp-setup-indent', so long as the ocp-indent binary exists."
    (when (executable-find "ocp-indent")
      (ocp-setup-indent)))
  (if ocp-indent-before-save
      (lambda () (add-hook 'tuareg-mode-hook
                      (lambda ()
                        (add-hook 'before-save-hook 'ocp-indent-buffer nil 'local))))
    )
  )

(use-package dune-mode
  :mode ("^dune$" "^dune-project$")
  )

(provide 'mdrp-ocaml)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-ocaml.el ends here
