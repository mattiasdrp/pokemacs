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
         (python-mode . lsp-deferred))
  :custom
  (lsp-log-io nil)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-keymap-prefix "M-l")
  (lsp-prefer-capf t)
  (lsp-lens-enable nil)
  (lsp-completion-provider :capf)
  (lsp-completion-enable t)
  (lsp-enable-imenu t)
  (lsp-disabled-clients '((python-mode . pyls)))
  :commands lsp

  :config

  ;; Temporary solution until https://github.com/emacs-lsp/lsp-mode/pull/3637 is merged
  (defcustom lsp-cut-signature 'space
    "If non-nil, signatures returned on hover will not be split on newline."
    :group 'lsp-ocaml
    :type '(choice (symbol :tag "Default behaviour" 'cut)
                   (symbol :tag "Display all the lines with spaces" 'space)))

  (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql ocamllsp)) &optional storable)
    "Extract a representative line from OCaml's CONTENTS, to show in the echo area.
This function splits the content between the signature
and the documentation to display the signature
and truncate it if it's too wide.
The STORABLE argument is used if you want to use this
function to get the type and, for example, kill and yank it."
    (let ((type (s-trim (lsp--render-element (lsp-make-marked-string
                                              :language "ocaml"
                                              :value (car (s-split "---" (lsp--render-element contents))))))))
      (if (equal nil storable)
          (if (eq lsp-cut-signature 'cut)
              (car (s-lines type))
            ;; else lsp-cut-signature is 'space
            (let ((ntype (s-replace "\n" " " type)))
              (if (>= (length ntype) (frame-width))
                  (concat (substring ntype 0 (- (frame-width) 4)) "...")
                ntype)))
        type)))

  ;;- end of temporary solution

  (defvar mdrp/type-map
    (let ((keymap (make-sparse-keymap)))
      (define-key keymap (kbd "C-w") #'mdrp/lsp-get-type-and-kill)
      keymap)
    "The local map to navigate type enclosing.")

  (defun mdrp/set-type-map (&rest r)
    (set-transient-map mdrp/type-map)
    )

  (advice-add 'lsp-describe-thing-at-point :after #'mdrp/set-type-map)

  (defun mdrp/lsp-get-type-and-kill ()
    (interactive)
    (let ((contents (-some->> (lsp--text-document-position-params)
                      (lsp--make-request "textDocument/hover")
                      (lsp--send-request)
                      (lsp:hover-contents))))
      (let ((contents (and contents
                           (lsp--render-on-hover-content
                            contents
                            t))))
        (let ((contents
               (pcase (lsp-workspaces)
                 (`(,workspace)
                  (lsp-clients-extract-signature-on-hover
                   contents
                   (lsp--workspace-server-id workspace)
                   t))
                 (lsp-clients-extract-signature-on-hover
                  contents
                  nil)
                 )))
          (message "Copied %s to kill-ring" contents)
          (kill-new contents)
          ))))

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
         ("C-c n"   . flycheck-next-error)
         ("C-c C-t" . lsp-describe-thing-at-point)
         ("C-c C-w" . mdrp/lsp-get-type-and-kill)
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
  ("C-c i" . lsp-ui-imenu)
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
