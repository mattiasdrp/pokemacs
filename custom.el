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
 '(package-selected-packages
   '(fsharp-mode yaml-mode json-navigator ox-pandoc org-make-toc git-commit z3-mode company-quickhelp saveplace-pdf-view org-ref pdf-tools org-super-agenda org-gcal ivy-bibtex calfw-org org-protocol calfw doom-themes elm-mode lsp-ivy lsp-treemacs lsp-ui lsp-mode selected god-mode general company-prescient cdlatex ws-butler treemacs-icons-dired treemacs-all-the-icons treemacs-magit treemacs-projectile treemacs ob-rust all-the-icons-dired visual-fill-column highlight-indent-guides utop ripgrep no-littering flyspell-correct-popup flyspell-correct-ivy flyspell-correct ace-window company-box ivy-prescient ivy-avy ivy-posframe wgrep yasnippet counsel-projectile doom-modeline all-the-icons-ivy-rich ivy-rich rainbow-delimiters helpful projectile dash zzz-to-char auctex-latexmk auto-complete-auctex company-auctex merlin-mode tuareg dumb-jump flycheck discover-my-major amx abbrev rust-mode use-package-ensure-system-package crux outline-minor-faces pretty-outlines dash-functional outshine json-mode run-ocaml dune-mode why3-mode LaTeX-math-mode tex-site gitignore-mode org-present diminish delight org-bullets rainbow-mode all-the-icons all-the-icons-ivy auctex auto-complete caml-debug caml-mode cargo company-math company-racer company-web counsel cubicle-mode dictionary dune flycheck-inline flycheck-ocaml flycheck-rust git-messenger magit merlin-eldoc minions multiple-cursors nlinum org-plus-contrib pandoc-mode php-mode quick-peek racer rotate separedit smex sort-words toml-mode undo-tree unfill use-package vlf web-mode which-key)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :slant normal :weight normal :height 136 :width normal :foundry "CTDB"))))
 '(fixed-pitch ((t (:family "Fira Code" :slant normal :weight normal :height 136 :width normal :foundry "CTDB"))))
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
 '(variable-pitch ((t (:family "Ubuntu" :height 136 :weight thin)))))

(provide 'custom)
;;; custom.el ends here
