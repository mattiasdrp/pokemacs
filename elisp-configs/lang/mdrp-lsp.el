;;; mdrp-lsp.el --- -*- lexical-binding: t -*-

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

(use-package lsp-mode
  :hook (
         (tuareg-mode . lsp-deferred)
         (caml-mode . lsp-deferred)
         (elm-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (fsharp-mode . lsp-deferred)
         )
  :custom
  (lsp-log-io t)
  ;; (lsp-headerline-breadcrumb-enable nil)

  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(project file symbols))
  (lsp-keymap-prefix "M-l")
  (lsp-prefer-capf t)
  (lsp-lens-enable nil)
  (lsp-diagnostics-provider :capf)
  (lsp-completion-provider :capf)
  (lsp-completion-enable t)
  (lsp-enable-imenu t)
  (lsp-disabled-clients '((python-mode . pyls)))
  :commands lsp
  :config
  (which-key-add-keymap-based-replacements lsp-command-map "u" "UI")
  (lsp-enable-which-key-integration t)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     '("opam" "exec" "--" "ocamllsp"))
    :major-modes '(caml-mode tuareg-mode)
    :server-id 'ocamllsp))
  :bind-keymap ("M-l" . lsp-command-map)
  :bind
  (:map lsp-command-map
        ("d" . lsp-find-definition)
        ("r" . lsp-find-references)
        ("n" . lsp-ui-find-next-reference)
        ("p" . lsp-ui-find-prev-reference)
        ("i" . counsel-imenu)
        ("R" . lsp-rename)
        ("tr" . lsp-treemacs-references)
        ("ts" . lsp-treemacs-symbols)
        ("te" . lsp-treemacs-error-list)
        ("f" . my-lsp-fix-buffer)
        )
  :bind (
   ("C-c n" . flycheck-next-error)
   ("C-c C-t" . lsp-describe-thing-at-point)
   ("C-c C-l" . lsp-find-definition)
   ("C-c &"   . pop-global-mark)
   )
  )

;; Useful link : https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-delay 0.9)
  (lsp-ui-doc-position 'at-point)
  ;; Whether or not to enable the header which displays the symbol string.
  (lsp-ui-doc-header t)
  ;; Whether or not to include the object signature/type in the frame.
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse nil)
  ;; Border color of the frame
  (lsp-ui-doc-border (face-foreground 'default))
  ;; Whether or not to enable lsp-ui-sideline
  (lsp-ui-sideline-enable nil)
  ;; Ignore duplicates when there is a same symbol with same contents
  ;; (lsp-ui-sideline-ignore-duplicate t)
  ;; Whether to show code actions in sideline.
  ;; (lsp-ui-sideline-show-code-actions nil)
  :bind
  ("C-M-d" . lsp-ui-doc-show)
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        )
  (:map lsp-command-map
        ("u f" . lsp-ui-doc-focus-frame)
        ("u i" . lsp-ui-imenu)
        )
  )

(use-package lsp-treemacs
  :after lsp
  )

(use-package lsp-ivy
  :after lsp
  )

(use-package lsp-rust
  :custom
  (lsp-rust-server 'rust-analyzer)
  )

(provide 'mdrp-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-lsp.el ends here
