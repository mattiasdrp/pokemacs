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
 '(package-selected-packages
   '(calfw-org org-protocol calfw doom-themes elm-mode lsp-ivy lsp-treemacs lsp-ui lsp-mode selected god-mode general company-prescient cdlatex ws-butler treemacs-icons-dired treemacs-all-the-icons treemacs-magit treemacs-projectile treemacs ob-rust all-the-icons-dired visual-fill-column highlight-indent-guides utop ripgrep no-littering saveplace-pdf-view flyspell-correct-popup flyspell-correct-ivy flyspell-correct ace-window company-box ivy-prescient ivy-avy ivy-posframe wgrep yasnippet counsel-projectile doom-modeline all-the-icons-ivy-rich ivy-rich rainbow-delimiters helpful projectile dash zzz-to-char auctex-latexmk auto-complete-auctex company-auctex merlin-mode tuareg dumb-jump flycheck discover-my-major amx abbrev rust-mode use-package-ensure-system-package crux outline-minor-faces pretty-outlines dash-functional outshine json-mode run-ocaml dune-mode why3-mode LaTeX-math-mode tex-site gitignore-mode org-present diminish delight org-bullets rainbow-mode all-the-icons all-the-icons-ivy auctex auto-complete caml-debug caml-mode cargo company-math company-racer company-web counsel cubicle-mode dictionary dune flycheck-inline flycheck-ocaml flycheck-rust git-messenger magit merlin-eldoc minions multiple-cursors nlinum org-plus-contrib pandoc-mode php-mode quick-peek racer rotate separedit smex sort-words toml-mode undo-tree unfill use-package vlf web-mode which-key)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:slant normal :weight normal :height 136 :width normal :foundry "CTDB" :family "Fira Code"))))
 '(show-paren-match ((t (:background "sienna" :weight normal)))))

(provide 'custom)
;;; custom.el ends here
