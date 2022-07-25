;;; package --- Customization for emacs
;;; Commentary:
;;; Global customization should be made with M-x customize-variable/face
;;; so everything can be found in this file
;;;
;;; If there is any question about what these variables/faces do just
;;; M-x customize-variable/face <ret> name_of_the_variable/face and see the doc
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(describe-char-unidata-list
   '(name old-name general-category decomposition decimal-digit-value digit-value numeric-value))
 '(org-export-backends '(ascii html icalendar latex md odt pandoc))
 '(package-selected-packages
   '(tree-sitter-langs tree-sitter cheatsheet keycast git-timemachine pulsar auto-package-update restart-emacs diff-hl magit-todos lsp-docker hide-mode-line async ghub anzu command-log-mode python-mode yapfify pyvenv lsp-pyright pyenv-mode dap-mode apheleia org-pdftools org-appear xr org-inline-pdf pandoc ox-gfm markdown-preview-mode dune-format abbrev ace-window all-the-icons all-the-icons-dired all-the-icons-ivy all-the-icons-ivy-rich amx auctex auctex-latexmk auto-complete auto-complete-auctex calfw calfw-org caml-debug caml-mode cargo cdlatex company-auctex company-box company-math company-prescient company-quickhelp company-racer company-web counsel counsel-projectile crux cubicle-mode dash dash-functional delight dictionary diminish discover-my-major doom-modeline doom-themes dumb-jump dune dune-mode elm-mode flycheck flycheck-inline flycheck-rust flyspell-correct flyspell-correct-ivy flyspell-correct-popup fsharp-mode general git-commit git-messenger gitignore-mode god-mode helpful highlight-indent-guides ivy-avy ivy-bibtex ivy-posframe ivy-prescient ivy-rich json-mode LaTeX-math-mode lsp-ivy lsp-mode lsp-treemacs lsp-ui magit merlin-mode minions multiple-cursors nlinum no-littering ob-rust org-bullets org-make-toc org-plus-contrib org-present org-protocol org-ref org-super-agenda outline-minor-faces outshine ox-pandoc pandoc-mode pdf-tools php-mode pretty-outlines projectile quick-peek racer rainbow-delimiters rainbow-mode ripgrep rotate run-ocaml rust-mode saveplace-pdf-view selected separedit smex sort-words tex-site toml-mode treemacs treemacs-all-the-icons treemacs-icons-dired treemacs-magit treemacs-projectile undo-tree unfill use-package use-package-ensure-system-package utop visual-fill-column vlf web-mode wgrep which-key why3-mode ws-butler yaml-mode yasnippet z3-mode zzz-to-char))
 '(warning-suppress-log-types '((emacs) (emacs) (comp)))
 '(warning-suppress-types '((emacs) (comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :slant normal :weight normal :height 136 :width normal :foundry "CTDB"))))
 '(fixed-pitch ((t (:family "Fira Code" :slant normal :weight normal :height 136 :width normal :foundry "CTDB"))))
 '(lsp-lens-face ((t (:family "Fira Code" :foundry "CTDB" :inherit lsp-details-face))))
 '(menu ((t (:inherit mode-line))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:inherit variable-pitch :height 1.4 :weight bold :foreground "#c678dd"))))
 '(org-level-1 ((t (:inherit variable-pitch :height 1.7 :weight bold :foreground "#51afef"))))
 '(org-level-2 ((t (:inherit variable-pitch :height 1.4 :weight bold :foreground "#c678dd"))))
 '(org-level-3 ((t (:inherit variable-pitch :height 1.2 :weight bold :foreground "#a9a1e1"))))
 '(org-level-4 ((t (:inherit variable-pitch :height 1.1 :weight bold :foreground "#7cc3f3"))))
 '(org-level-5 ((t (:inherit variable-pitch :height 1.0 :weight bold))))
 '(org-level-6 ((t (:inherit variable-pitch :height 1.0 :weight bold))))
 '(org-level-7 ((t (:inherit variable-pitch :height 1.0 :weight bold))))
 '(org-level-8 ((t (:inherit variable-pitch :height 1.0 :weight bold))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(show-paren-match ((t (:background "sienna" :weight normal))))
 '(variable-pitch ((t (:family "Ubuntu" :height 136 :weight thin))))
;; tree-sitter-hl-face:attribute
;; tree-sitter-hl-face:comment
;; tree-sitter-hl-face:constant
;; tree-sitter-hl-face:constant.builtin
;; tree-sitter-hl-face:constructor
;; tree-sitter-hl-face:doc
;; tree-sitter-hl-face:embedded
;; tree-sitter-hl-face:escape
;; tree-sitter-hl-face:function
;; tree-sitter-hl-face:function.builtin
;; tree-sitter-hl-face:function.call
;; tree-sitter-hl-face:function.macro
;; tree-sitter-hl-face:function.special
;; tree-sitter-hl-face:keyword
;; tree-sitter-hl-face:label
;; tree-sitter-hl-face:method
;; tree-sitter-hl-face:method.call
;; tree-sitter-hl-face:number
;; tree-sitter-hl-face:operator
;; tree-sitter-hl-face:property
;; tree-sitter-hl-face:property.definition
;; tree-sitter-hl-face:punctuation
;; tree-sitter-hl-face:punctuation.bracket
;; tree-sitter-hl-face:punctuation.delimiter
;; tree-sitter-hl-face:punctuation.special
;; tree-sitter-hl-face:string
;; tree-sitter-hl-face:string.special
;; tree-sitter-hl-face:tag
;; tree-sitter-hl-face:type
;; tree-sitter-hl-face:type.argument
;; tree-sitter-hl-face:type.builtin
;; tree-sitter-hl-face:type.parameter
;; tree-sitter-hl-face:type.super
;; tree-sitter-hl-face:variable
;; tree-sitter-hl-face:variable.builtin
;; tree-sitter-hl-face:variable.parameter
;; tree-sitter-hl-face:variable.special


;;  tuareg-font-double-colon-face
;; tuareg-font-lock-attribute-face
;; tuareg-font-lock-constructor-face
;; tuareg-font-lock-error-face
;; tuareg-font-lock-extension-node-face
;; tuareg-font-lock-governing-face
;; tuareg-font-lock-infix-extension-node-face
;; tuareg-font-lock-interactive-directive-face
;; tuareg-font-lock-interactive-error-face
;; tuareg-font-lock-interactive-output-face
;; tuareg-font-lock-label-face
;; tuareg-font-lock-line-number-face
;; tuareg-font-lock-module-face
;; tuareg-font-lock-multistage-face
;; tuareg-font-lock-operator-face
 )

(provide 'custom)
;;; custom.el ends here
