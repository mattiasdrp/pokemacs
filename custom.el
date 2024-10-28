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
 '(pokemacs-mono-font "Fira Code" t)
 '(pokemacs-repeat-timeout 0.5)
 '(pokemacs-dark-theme 'doom-solarized-dark t)
 '(pokemacs-light-theme 'doom-solarized-light t)
 '(pokemacs-mode-line-right-align 'right-fringe t)
 '(pokemacs-variable-font "Iosevka Aile" t)
 '(safe-local-variable-values
   '((projectile-project-root-functions projectile-root-top-down
                                        projectile-root-bottom-up
                                        projectile-root-top-down-recurring)))
 '(show-paren-priority -50)
 '(show-paren-style 'expression)
 '(tuple-mono-font
   (if (window-system)
       (cond ((x-list-fonts pokemacs-mono-font) `(:font ,pokemacs-mono-font))
             ((x-list-fonts "Fira Code") '(:font "Fira Code"))
             ((x-list-fonts "Inconsolata") '(:font "Inconsolata"))
             ((x-family-fonts "DejaVu") '(:family "DejaVu"))
             (nil (warn "Cannot find a monospaced font.")))
     '(:family "Monospace")) t)
 '(tuple-variable-font
   (if (window-system)
       (cond
        ((x-list-fonts pokemacs-variable-font) `(:font ,pokemacs-variable-font))
        ((x-list-fonts "Iosevka Aile") '(:font "Iosevka Aile"))
        ((x-list-fonts "ETBembo") '(:font "ETBembo"))
        ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
        ((x-list-fonts "Lucida Grande") '(:font "Lucida Grande"))
        ((x-list-fonts "Verdana") '(:font "Verdana"))
        ((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
        (nil (warn "Cannot find a Sans Serif Font.")))
     '(:family "Monospace")) t)
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
 '(use-latex nil)
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
 '(warning-suppress-types '((flycheck syntax-checker) (treesit) (comp)))
 '(x-stretch-cursor nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 `(default ((t (,@tuple-mono-font :slant normal :weight normal :height 118 :width normal :foundry "CTDB"))))
 `(fixed-pitch ((t (,@tuple-mono-font :slant normal :weight normal :height 118 :width normal :foundry "CTDB"))))
 `(lsp-lens-face ((t (,@tuple-mono-font :foundry "CTDB" :inherit lsp-details-face))))
 '(menu ((t (:inherit mode-line))))
 '(mode-line ((t :inherit fixed-pitch)))
 `(mode-line-inactive ((nil ,@tuple-mono-font)))
 `(variable-pitch ((t (,@tuple-variable-font :weight light :height 1.3)))))

(provide 'custom)
;;; custom.el ends here
