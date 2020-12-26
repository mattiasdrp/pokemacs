;;; mdrp-org.el --- -*- lexical-binding: t -*-

;; Copyright (c) 2020-2020 Mattias and contributors.

;; Author: Mattias
;; Maintainer: Mattias <mattias@email.com>
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

(use-package org
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
  (org-directory "~/org/")
  (org-agenda-files `(,(concat org-directory "agenda.org")))
  (org-ellipsis " ▾")
  (org-startup-truncated nil)
  (org-adapt-indentation nil)
  (org-support-shift-select 'always)
  (org-agenda-start-with-log-mode t)
  (org-agenda-start-day "1d")
  (org-agenda-span 'week)
  (org-agenda-start-on-weekday nil)
  (org-log-done 'time)
  (org-tag-persistent-alist
   '((:startgroup . nil)
     ("Maison" . ?m)
     ("Boulot" . ?b)
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
  (org-capture-templates
   `(("t" "Todo" entry (file+headline ,(concat org-directory "agenda.org") "A Faire")
      "* TODO %?\n  %i\n  %a")
     ("r" "Rdv" entry (file+headline ,(concat org-directory "agenda.org") "Rendez-vous")
      "* RDV %?\n  %i\n  %a")
     ;; ("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
     ;;  "* %? [[%:link][%:description]] \nCaptured On: %U")
     ("p" "Protocol" entry (file+headline ,(concat org-directory "agenda.org") "Citations")
      "* %^{Title}\nSource: %:link\nCaptured On: %U\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
     ("L" "Protocol Link" entry (file+headline ,(concat org-directory "agenda.org") "Liens")
      "* %? [[%:link][%:description]] \nCaptured On: %U")
     )
   )
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  :bind-keymap ("M-o" . mdrp-org-map)
  :bind (
         (:map mdrp-org-map
               ("l"                       . org-store-link)
               ("a"                       . org-agenda)
               )
         (:map org-mode-map
               ("M-j"                     . org-goto)
               )
         )
  :config
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

  (add-hook 'org-mode-hook #'org-setup-<>-syntax-fix)

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
     ))
  (let ((re "\\}\\(+\\|*\\|-\\) "))
    (font-lock-add-keywords
      'org-mode
      `((,(concat "^[[:space:]]\\{" (number-to-string (+ 0 org-list-indent-offset)) "\\}\\(+\\|-\\) ")
         (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◈"))))))

     (font-lock-add-keywords
      'org-mode
      `((,(concat "^[[:space:]]\\{" (number-to-string (+ 2 org-list-indent-offset)) re)
         (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◆"))))))

     (font-lock-add-keywords
      'org-mode
      `((,(concat "^[[:space:]]\\{" (number-to-string
                                     (* 2 (+ 2 org-list-indent-offset))) re)
         (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◇"))))))
     (font-lock-add-keywords
      'org-mode
      `((,(concat "^[[:space:]]\\{" (number-to-string
                                     (* 3 (+ 2 org-list-indent-offset))) re)
         (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◼"))))))
     )
  )

(use-package org-tempo ;; part of org-mode
  :after (org)
  :config
  (add-to-list 'org-structure-template-alist '("smt" . "src smt-lib"))
  (add-to-list 'org-structure-template-alist '("oc" . "src ocaml"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  )

(use-package org-protocol
  :after (org)
)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  ;; :custom
  ;; (org-bullets-bullet-list '("" "" "" "" "" "" ""))
)

(use-package visual-fill-column
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t)
  :hook (org-mode . visual-fill-column-mode))

(provide 'mdrp-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-org.el ends here
