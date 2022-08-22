;;; package --- Customization for emacs
;;; Commentary:
;; Global customization should be made with M-x customize-variable/face
;; so everything can be found in this file
;;;
;; If there is any question about what these variables/faces do just
;; M-x customize-variable/face <ret> name_of_the_variable/face and see the doc
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(auth-sources '("~/.authinfo"))
 '(completion-ignored-extensions (remove ".git/" completion-ignored-extensions))
 '(describe-char-unidata-list
   '(name old-name general-category decomposition decimal-digit-value digit-value numeric-value))
 '(doom-modeline-bar-width 4)
 '(doom-modeline-height 25)
 '(doom-theme 'doom-one)
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(electric-indent-mode t)
 '(hl-todo-keyword-faces
   '(("MDRPTODO" . "red")
     ("HOLD" . "#d0bf8f")
     ("TODO" . "#cc9393")
     ("NEXT" . "#dca3a3")
     ("THEM" . "#dc8cc3")
     ("PROG" . "#7cb8bb")
     ("OKAY" . "#7cb8bb")
     ("DONT" . "#5f7f5f")
     ("FAIL" . "#8c5353")
     ("DONE" . "#afd8af")
     ("NOTE" . "#d0bf8f")
     ("KLUDGE" . "#d0bf8f")
     ("HACK" . "#d0bf8f")
     ("TEMP" . "#d0bf8f")
     ("FIXME" . "#cc9393")
     ("XXXX*" . "#cc9393")))
 '(org-export-backends '(ascii html icalendar latex md odt pandoc))
 '(package-selected-packages
   '(ocp-indent tuareg calfw-org yasnippet lsp-ui web-mode flycheck-rust cargo rustic reason-mode lsp-pyright pyvenv saveplace-pdf-view pandoc-mode markdown-toc kotlin-mode fsharp-mode toml-mode tree-sitter-langs tree-sitter org-make-toc org-roam org-appear org-super-agenda calfw org-inline-pdf org-bullets org-ref which-key vertico use-package-ensure-system-package separedit selected rainbow-mode rainbow-delimiters pulsar projectile outshine orderless ob-rust no-littering minions marginalia magit-todos ligature keycast highlight-symbol git-timemachine git-modes git-messenger general fringe-helper flycheck embark-consult easy-kill dumb-jump doom-themes doom-modeline discover-my-major diff-hl dap-mode crux company-web company-quickhelp company-prescient company-box auto-package-update apheleia anzu all-the-icons-dired all-the-icons-completion))
 '(show-paren-style 'expression)
 '(use-company t)
 '(use-fsharp t)
 '(use-god nil)
 '(use-kotlin t)
 '(use-latex nil)
 '(use-magit-todos t)
 '(use-markdown t)
 '(use-maximize t)
 '(use-ocaml t)
 '(use-org-roam t)
 '(use-pandoc t)
 '(use-python t)
 '(use-reason t)
 '(use-rust t)
 '(use-spotify nil)
 '(use-treemacs t)
 '(use-visual-fill t)
 '(use-web t)
 '(warning-suppress-types '((comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :slant normal :weight normal :height 136 :width normal :foundry "CTDB"))))
 '(fixed-pitch ((t (:family "Fira Code" :slant normal :weight normal :height 136 :width normal :foundry "CTDB"))))
 '(lsp-lens-face ((t (:family "Fira Code" :foundry "CTDB" :inherit lsp-details-face))))
 '(menu ((t (:inherit mode-line))))
 '(mode-line ((nil :family "Fira Code" :height 140)))
 '(mode-line-inactive ((nil :family "Fira Code" :height 140)))
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
