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
 '(blink-cursor-mode t)
 '(calendar-month-abbrev-array
   ["Jan" "Fév" "Mar" "Avr" "Mai" "Jun" "Jul" "Aoû" "Sep" "Oct" "Nov" "Déc"])
 '(completion-ignored-extensions (remove ".git/" completion-ignored-extensions))
 '(cursor-intangible-mode t t)
 '(cursor-type t)
 '(describe-char-unidata-list
   '(name old-name general-category decomposition decimal-digit-value digit-value numeric-value))
 '(doom-modeline-height 12)
 '(doom-theme 'doom-solarized-dark)
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(eaf-browser-blank-page-url "https://duckduckgo.com/")
 '(electric-indent-mode t)
 '(fill-column 80)
 '(flycheck-languagetool-server-jar
   (concat
    (getenv "HOME")
    "/.emacs.d/LanguageTool-5.9-stable/languagetool-server.jar"))
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
 '(lsp-auto-guess-root t)
 '(org-agenda-files
   '("/home/mattias/org/academie_djoukui_roux.org" "/home/mattias/org/agenda.org" "/home/mattias/org/agenda.sync-conflict-20221017-103511-SKAKFUN.org" "/home/mattias/org/applis_pc.org" "/home/mattias/org/bd.org" "/home/mattias/org/bebe.org" "/home/mattias/org/calendar_company.org" "/home/mattias/org/calendar_user.org" "/home/mattias/org/cduce.org" "/home/mattias/org/conseil_decole_17_10_2022.org" "/home/mattias/org/garde.org" "/home/mattias/org/ikea.org" "/home/mattias/org/informatique.org" "/home/mattias/org/notes.org" "/home/mattias/org/orgzly.org" "/home/mattias/org/usuba.org" "/home/mattias/org/usuba.sync-conflict-20221017-103511-SKAKFUN.org") nil nil "Customized with use-package org")
 '(org-export-backends '(ascii html icalendar latex md odt pandoc))
 '(package-selected-packages
   '(page-break-lines dashboard auctex-latexmk mixed-pitch svg-tag-mode org-auto-tangle flycheck-languagetool solaire-mode multiple-cursors auctex pdf-tools kurecolor consult-flycheck consult-yasnippet flyspell-correct fontify-face dune iedit tree-sitter-langs tree-sitter hide-mode-line ocp-indent tuareg calfw-org yasnippet lsp-ui web-mode flycheck-rust cargo rustic reason-mode lsp-pyright pyvenv saveplace-pdf-view pandoc-mode markdown-toc kotlin-mode fsharp-mode org-make-toc org-roam org-appear org-super-agenda calfw org-inline-pdf org-bullets org-ref which-key vertico use-package-ensure-system-package separedit selected rainbow-mode rainbow-delimiters pulsar projectile outshine orderless ob-rust no-littering minions marginalia magit-todos ligature keycast highlight-symbol git-timemachine git-modes git-messenger general fringe-helper flycheck embark-consult easy-kill dumb-jump doom-themes doom-modeline discover-my-major diff-hl dap-mode crux company-web company-quickhelp company-prescient company-box auto-package-update apheleia anzu all-the-icons-dired all-the-icons-completion))
 '(safe-local-variable-values
   '((projectile-project-root-functions . (projectile-root-top-down projectile-root-bottom-up projectile-root-top-down-recurring))))
 '(show-paren-style 'expression)
 '(use-company t)
 '(use-eaf t)
 '(use-fsharp t)
 '(use-god nil)
 '(use-kotlin t)
 '(use-latex t)
 '(use-magit-todos t)
 '(use-markdown t)
 '(use-maximize t)
 '(use-michelson nil)
 '(use-ocaml t)
 '(use-org-agenda-startup nil)
 '(use-org-roam t)
 '(use-pandoc t)
 '(use-python t)
 '(use-rainbow t)
 '(use-reason t)
 '(use-rust t)
 '(use-solaire t)
 '(use-spotify nil)
 '(use-treemacs t)
 '(use-visual-fill t)
 '(use-web t)
 '(use-window-purpose nil)
 '(vertico-multiform-categories
   '((imenu buffer)
     (file
      (vertico-sort-function . sort-directories-first))
     (company
      (vertico-sort-function . vertico-sort-alpha))
     (symbol
      (vertico-sort-function . vertico-sort-history-length-alpha))))
 '(vertico-multiform-commands
   '((consult-imenu buffer)
     (consult-line buffer)
     (execute-extended-command mouse)
     (find-file
      (vertico-sort-function . sort-directories-first))
     (insert-char
      (vertico-sort-function . sort-characters))
     (describe-symbol
      (vertico-sort-override-function . vertico-sort-alpha))))
 '(warning-suppress-types '((comp)))
 '(x-stretch-cursor nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :slant normal :weight normal :height 118 :width normal :foundry "CTDB"))))
 '(fixed-pitch ((t (:family "Fira Code" :slant normal :weight normal :height 118 :width normal :foundry "CTDB"))))
 '(lsp-lens-face ((t (:family "Fira Code" :foundry "CTDB" :inherit lsp-details-face))))
 '(menu ((t (:inherit mode-line))))
 '(mode-line ((t :inherit fixed-pitch)))
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
 '(region ((t (:extend t :background "#93a1a1" :foreground "#073642" :inverse-video t))))
 '(secondary-selection  ((t (:inherit region))))
 '(show-paren-match ((t (:inherit highlight :foreground unspecified :weight normal))))
 '(show-paren-mismatch ((t (:foreground unspecified :weight normal :background "#7D3131"))))
 '(variable-pitch ((t (:family "Ubuntu" :height 118 :weight regular)))))

(provide 'custom)
;;; custom.el ends here
