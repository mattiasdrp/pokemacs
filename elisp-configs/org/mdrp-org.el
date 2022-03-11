;;; mdrp-org.el --- -*- lexical-binding: t -*-

;; Copyright (c) 2020-2020 mdrp and contributors.

;; Author: mdrp
;; Maintainer: mdrp <https://github.com/MonsieurPi>
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

(require 'org-protocol)

(use-package org
  :ensure t
  ;; :hook (org-mode . variable-pitch-mode)
  :init
  (setq org-list-allow-alphabetical t)
  ;; If you don't want the agenda in french you can comment the following
  ;; expression. You can even set it to your preferred language
  ;; https://www.emacswiki.org/emacs/CalendarLocalization#toc16
  (setq calendar-week-start-day 1
        calendar-day-name-array ["Dimanche" "Lundi" "Mardi" "Mercredi"
                                 "Jeudi" "Vendredi" "Samedi"]
        calendar-month-name-array ["Janvier" "Février" "Mars" "Avril" "Mai"
                                   "Juin" "Juillet" "Août" "Septembre"
                                   "Octobre" "Novembre" "Décembre"])
  :custom
  (org-agenda-files `(,org-directory))
  (org-ellipsis " ▾")
  (org-footnote-auto-adjust t)
  (org-cycle-separator-lines -1)
  (org-startup-truncated nil)
  (org-adapt-indentation nil)
  (org-hide-emphasis-markers t)
  (org-fontify-done-headline t)
  (org-hide-leading-stars t)
  (org-pretty-entities t)
  (org-support-shift-select 'always)
  (org-agenda-start-with-log-mode t)
  (org-agenda-start-day "1d")
  (org-agenda-span 'week)
  (org-agenda-start-on-weekday nil)
  (org-log-done 'time)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-tag-persistent-alist
   '((:startgroup . nil)
     ("Maison" . ?h)
     ("Boulot" . ?b)
     ("Plaisir" . ?p)
     (:endgroup . nil)
     (:startgroup . nil)
     ("Irmin" . ?i)
     ("Ocaml" . ?o)
     ("Reste" . ?r)
     (:endgroup . nil)
     (:startgroup . nil)
     ("Facile" . ?f)
     ("Moyen" . ?m)
     ("Difficile" . ?d)
     (:endgroup . nil)
     )
   )
  (org-tag-faces
   '(
     ("Maison" . (:foreground "GoldenRod" :weight bold))
     ("Boulot" . (:foreground "GoldenRod" :weight bold))
     ("Irmin" . (:foreground "IndianRed1" :weight bold))
     ("OCaml" . (:foreground "IndianRed1" :weight bold))
     ("Reste" . (:foreground "IndianRed1" :weight bold))
     ("Facile" . (:foreground "OrangeRed" :weight bold))
     ("Moyen" . (:foreground "OrangeRed" :weight bold))
     ("Difficile" . (:foreground "OrangeRed" :weight bold))
     )
   )
  :bind-keymap ("M-o" . mdrp-org-map)
  :bind (
         (:map mdrp-org-map
               ("l"                       . org-store-link)
               ("a"                       . org-agenda)
               ("c"                       . org-capture)
               )
         (:map org-mode-map
               ("M-j"                     . org-goto)
               )
         )

  :config
  (setq org-image-actual-width nil)
  (define-prefix-command 'mdrp-org-map nil "Org-")
  (defun org-mode-<>-syntax-fix (start end)
    "Change syntax of characters ?< and ?> to symbol within source code blocks."
    (let ((case-fold-search t))
      (when (eq major-mode 'org-mode)
        (save-excursion
          (goto-char start)
          (while (re-search-forward "<\\|>" end t)
            (when (save-excursion
                    (and
                     (re-search-backward "[[:space:]]*#\\+\\(begin\\|end\\)_src\\_>" nil t)
                     (string-equal (downcase (match-string 1)) "begin")))
              ;; This is a < or > in an org-src block
              (put-text-property (point) (1- (point))
                                 'syntax-table (string-to-syntax "_"))))))))

  (defun org-setup-<>-syntax-fix ()
    "Setup for characters ?< and ?> in source code blocks.
Add this function to `org-mode-hook'."
    (setq syntax-propertize-function 'org-mode-<>-syntax-fix)
    (syntax-propertize (point-max)))

  (add-hook 'org-mod-hook #'org-setup-<>-syntax-fix)

  (setq org-agenda-custom-commands
        '(("r" "Rendez-vous" agenda* "Rendez-vous du mois"
           ((org-agenda-span 'month)
            (org-agenda-show-all-dates nil)
            ))))
  (calendar-set-date-style 'iso)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((rust . t)
     (ocaml . t)
     (latex . t)
     ))
  (add-hook 'org-mode-hook
            (lambda ()
              (push '("- [ ]" . "") prettify-symbols-alist)
              (push '("+ [ ]" . "") prettify-symbols-alist)
              (push '("* [ ]" . "") prettify-symbols-alist)
              (push '("- [X]" . "") prettify-symbols-alist)
              (push '("+ [X]" . "") prettify-symbols-alist)
              (push '("* [X]" . "") prettify-symbols-alist)
              (push '("- [-]" . "") prettify-symbols-alist)
              (push '("+ [-]" . "") prettify-symbols-alist)
              (push '("* [-]" . "") prettify-symbols-alist)
              (prettify-symbols-mode)
              ))
  (custom-theme-set-faces
   'user
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
  ;; (let ((re "\\}\\(+\\|-\\) "))
  ;;   (font-lock-add-keywords
  ;;     'org-mode
  ;;     `((,(concat "^[[:space:]]\\{" (number-to-string (+ 0 org-list-indent-offset)) re)
  ;;        (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;;    (font-lock-add-keywords
  ;;     'org-mode
  ;;     `((,(concat "^[[:space:]]\\{" (number-to-string (+ 2 org-list-indent-offset)) re)
  ;;        (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◆"))))))

  ;;    (font-lock-add-keywords
  ;;     'org-mode
  ;;     `((,(concat "^[[:space:]]\\{" (number-to-string
  ;;                                    (* 2 (+ 2 org-list-indent-offset))) re)
  ;;        (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◇"))))))
  ;;    (font-lock-add-keywords
  ;;     'org-mode
  ;;     `((,(concat "^[[:space:]]\\{" (number-to-string
  ;;                                    (* 3 (+ 2 org-list-indent-offset))) re)
  ;;        (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◼"))))))
  ;;    )
  )
  )

(use-package ivy-bibtex
  :defer t)

(use-package org-ref
  :after org
  :init
  (setq
   org-ref-completion-library 'org-ref-ivy-cite
   )
  :custom
  (org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
  )



(use-package org-tempo ;; part of org-mode
  :after (org)
  :config
  (add-to-list 'org-structure-template-alist '("smt" . "src smt-lib"))
  (add-to-list 'org-structure-template-alist '("oc" . "src ocaml"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  )

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  ;; :custom
  ;; (org-bullets-bullet-list '("" "" "" "" "" "" ""))
)

(use-package org-inline-pdf
  :hook (org-mode . org-inline-pdf-mode)
  )

(use-package calfw
  :config
  (setq cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓)
)

(use-package calfw-org
  :after calfw
  :config
  (define-prefix-command 'mdrp-calfw-map nil "Cal-")
  (defun cfw:org-capture-day ()
    (with-current-buffer  (get-buffer-create cfw:calendar-buffer-name)
      (let ((pos (cfw:cursor-to-nearest-date)))
        (concat "<"
                (format-time-string  "%Y-%m-%d %a 09:00"
                                     (encode-time 0 0 0
                                                  (calendar-extract-day pos)
                                                  (calendar-extract-month pos)
                                                  (calendar-extract-year pos)))
                ">"))))
  :bind-keymap ("M-c" . mdrp-calfw-map)
  :bind (
         (:map mdrp-calfw-map
               ("c" . cfw:open-calendar-buffer)
               ("o" . cfw:open-org-calendar)
               )
         )
  :custom
  (cfw:org-capture-template
   `("c" "calfw2org" entry (file+headline ,(concat org-directory "agenda.org") "Calendrier")
     "* %?\nSCHEDULED: %(cfw:org-capture-day)" :empty-lines 1)
   )
  )

;; The request can be wrong depending on Google updates, evaluate this:
;; (concat org-gcal-auth-url
;;         "?client_id=" (url-hexify-string org-gcal-client-id)
;;         "&response_type=code"
;;         "&redirect_uri=" (url-hexify-string "urn:ietf:wg:oauth:2.0:oob")
;;         "&scope=" (url-hexify-string org-gcal-resource-url))
;; (use-package org-gcal
;;   :custom
;;   (org-gcal-client-id (get-secrets-config-value 'org-gcal-client-id))
;;   (org-gcal-client-secret (get-secrets-config-value 'org-gcal-client-secret))
;;   (org-gcal-fetch-file-alist
;;    `(
;;      (,(get-secrets-config-value 'calendar-company) . "~/org/calendar_company.org")
;;      (,(get-secrets-config-value 'calendar-user) . "~/org/calendar_user.org")
;;      )
;;    )
;;   )

(use-package org-super-agenda
  :ensure t
  :config
  (setq org-super-agenda-groups
        '(;; Each group has an implicit Boolean OR operator between its selectors.
          (:name "ASDSD"  ; Optionally specify section name
                 :time-grid t  ; Items that appear on the time grid
                 )
          ;; After the last group, the agenda will display items that didn't
          ;; match any of these groups, with the default order position of 99
          ))
  (org-super-agenda-mode)
  ;; (org-agenda nil "a")
  ;; (setq org-agenda-log-mode 1)
  )

(provide 'mdrp-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-org.el ends here
