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
         (elm-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (fsharp-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         )
  :custom
  (lsp-log-io t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-keymap-prefix "M-l")
  (lsp-prefer-capf t)
  (lsp-completion-provider :capf)
  (lsp-completion-enable t)
  (lsp-enable-imenu t)
  :commands lsp
  :config
  (which-key-add-keymap-based-replacements lsp-command-map "u" "UI")

  :bind
  )

;; Useful link : https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-header t)                           ;; Whether or not to enable the header which displays the symbol string.
  (lsp-ui-doc-include-signature t)                ;; Whether or not to include the object signature/type in the frame.
  (lsp-ui-doc-border (face-foreground 'default))  ;; Border color of the frame
  (lsp-ui-sideline-enable nil)                    ;; Whether or not to enable lsp-ui-sideline
  ;; (lsp-ui-sideline-ignore-duplicate t)            ;; Ignore duplicates when there is a same symbol with same contents
  ;; (lsp-ui-sideline-show-code-actions nil)         ;; Whether to show code actions in sideline.
  :bind
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
