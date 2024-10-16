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
   '(name old-name general-category decomposition decimal-digit-value digit-value
          numeric-value))
 '(doom-modeline-height 12)
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(eaf-browser-blank-page-url "https://duckduckgo.com/")
 '(electric-indent-mode t)
 '(fill-column 80)
 '(flycheck-languagetool-server-jar
   (concat (getenv "HOME") "/.emacs.d/LanguageTool-6.1/languagetool-server.jar"))
 '(hl-todo-keyword-faces
   '(("TODO" warning bold) ("FIXME" error bold)
     ("REVIEW" font-lock-keyword-face bold)
     ("DEPRECATED" font-lock-doc-face bold) ("NOTE" success bold)
     ("XXXX*" font-lock-constant-face bold)))
 '(lsp-auto-guess-root nil)
 '(lsp-java-completion-lazy-resolve-text-edit-enabled t)
 '(lsp-java-format-enabled nil)
 '(magit-diff-refine-hunk 'all)
 '(org-export-backends '(ascii html icalendar latex md odt pandoc))
 '(package-selected-packages nil)
 '(pokemacs-dict "en-GB")
 '(pokemacs-repeat-timeout 0.5)
 '(pokemacs-theme 'doom-solarized-dark t)
 '(pokemacs-mono-font "Fira Code" t)
 '(pokemacs-variable-font "Iosevka Aile" t)
 '(safe-local-variable-values
   '((projectile-project-root-functions projectile-root-top-down
                                        projectile-root-bottom-up
                                        projectile-root-top-down-recurring)))
 '(show-paren-style 'expression)
 '(use-all-the-icons nil)
 '(use-clojure t)
 '(use-dashboard t)
 '(use-eaf nil)
 '(use-elm t)
 '(use-fsharp t)
 '(use-gcal nil)
 '(use-god nil)
 '(use-header-line t)
 '(use-java t)
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
 '(use-posframe nil)
 '(use-python t)
 '(use-racket t)
 '(use-rainbow t)
 '(use-reason nil)
 '(use-rust t)
 '(use-sicp t)
 '(use-solaire t)
 '(use-spotify nil)
 '(use-treemacs nil)
 '(use-visual-fill t)
 '(use-web t)
 '(use-window-purpose nil)
 '(tuple-mono-font
   (if (window-system)
       (cond
        ((x-list-fonts pokemacs-mono-font)         `(:font ,pokemacs-mono-font))
        ((x-list-fonts "Fira Code")                '(:font "Fira Code"))
        ((x-list-fonts "Inconsolata")              '(:font "Inconsolata"))
        ((x-family-fonts "DejaVu")                 '(:family "DejaVu"))
        (nil (warn "Cannot find a monospaced font.")))
     '(:family "Monospace"))
   t)
 '(tuple-variable-font
   (if (window-system)
       (cond
        ((x-list-fonts pokemacs-variable-font)     `(:font ,pokemacs-variable-font))
        ((x-list-fonts "Iosevka Aile")             '(:font "Iosevka Aile"))
        ((x-list-fonts "ETBembo")                  '(:font "ETBembo"))
        ((x-list-fonts "Source Sans Pro")          '(:font "Source Sans Pro"))
        ((x-list-fonts "Lucida Grande")            '(:font "Lucida Grande"))
        ((x-list-fonts "Verdana")                  '(:font "Verdana"))
        ((x-family-fonts "Sans Serif")             '(:family "Sans Serif"))
        (nil (warn "Cannot find a Sans Serif Font.")))
     '(:family "Monospace"))
   t)
 '(warning-suppress-types '((comp)))
 '(x-stretch-cursor nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 `(default ((t (,@tuple-mono-font :slant normal :weight normal :height 118 :width normal :foundry "CTDB"))))
 `(fixed-pitch ((t (,@tuple-mono-font :slant normal :weight normal :height 118 :width normal :foundry "CTDB"))))
 `(variable-pitch ((t (,@tuple-variable-font :weight light :height 1.3))))
 `(lsp-lens-face ((t (,@tuple-mono-font :foundry "CTDB" :inherit lsp-details-face))))
 '(menu ((t (:inherit mode-line))))
 '(mode-line ((t :inherit fixed-pitch)))
 `(mode-line-inactive ((nil ,@tuple-mono-font)))
 '(org-document-title ((t (:inherit variable-pitch :height 1.4 :weight bold :foreground "#c678dd"))))
 '(org-level-1 ((t (:inherit variable-pitch :height 1.7 :weight bold :foreground "#51afef"))))
 '(org-level-2 ((t (:inherit variable-pitch :height 1.4 :weight bold :foreground "#c678dd"))))
 '(org-level-3 ((t (:inherit variable-pitch :height 1.2 :weight bold :foreground "#a9a1e1"))))
 '(org-level-4 ((t (:inherit variable-pitch :height 1.1 :weight bold :foreground "#7cc3f3"))))
 '(org-level-5  ((t (:inherit variable-pitch :height 1.0 :weight bold))))
 '(org-level-6  ((t (:inherit variable-pitch :height 1.0 :weight bold))))
 '(org-level-7  ((t (:inherit variable-pitch :height 1.0 :weight bold))))
 '(org-level-8  ((t (:inherit variable-pitch :height 1.0 :weight bold))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 ;; '(org-block             ((t (:inherit fixed-pitch))))
 ;; '(org-block-begin-line  ((t (:inherit fixed-pitch))))
 ;; '(org-block-end-line    ((t (:inherit fixed-pitch))))
 '(org-code              ((t (:inherit (shadow fixed-pitch)))))
 '(org-date              ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info     ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-drawer            ((t (:inherit (shadow fixed-pitch)))))
 '(org-ellipsis          ((t (:inherit fixed-pitch :underline nil))))
 '(org-indent            ((t (:inherit (org-hide fixed-pitch)))))
 `(org-link              ((t (:inherit fixed-pitch :underline t))))
 '(org-meta-line         ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-modern-statistics ((t (:height 1.6))))
 '(org-properties        ((t (:inherit fixed-pitch))))
 '(org-property-value    ((t (:inherit fixed-pitch))))
 '(org-src               ((t (:inherit fixed-pitch))))
 '(org-property-value    ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword   ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table             ((t (:inherit fixed-pitch))))
 '(org-tag               ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim          ((t (:inherit (shadow fixed-pitch)))))
 '(region                ((t (:extend t :background "#93a1a1" :foreground "#073642" :inverse-video t))))
 '(secondary-selection   ((t (:inherit region))))
 '(show-paren-match      ((t (:inherit highlight :foreground unspecified :weight normal))))
 '(show-paren-mismatch   ((t (:foreground unspecified :weight normal :background "#7D3131")))))

(provide 'custom)
;;; custom.el ends here
