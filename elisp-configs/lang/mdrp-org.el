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
  :custom
  (org-ellipsis " ▾")
  (org-startup-truncated nil)
  (org-support-shift-select 'always)
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-agenda-files
   '("~/org/afaire.org"
     "~/org/rdv.org"))
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
   '(("t" "Todo" entry (file+headline "~/org/afaire.org" "A Faire")
      "* TODO %?\n  %i\n  %a")
     ("r" "Rdv" entry (file+headline "~/org/rdv.org" "Rendez-vous")
      "* RDV %?\n  %i\n  %a")))
  (font-lock-add-keywords
   'org-mode
   '(("^\\(-\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (font-lock-add-keywords
   'org-mode
   `((,(concat "^[[:space:]]\\{" (number-to-string (+ 2 org-list-indent-offset)) "\\}\\(-\\) ")
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

  (font-lock-add-keywords
   'org-mode
   `((,(concat "^[[:space:]]\\{" (number-to-string
                                  (* 2 (+ 2 org-list-indent-offset))) "\\}\\(-\\) ")
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◆")))))
)
  (font-lock-add-keywords
   'org-mode
   `((,(concat "^[[:space:]]\\{" (number-to-string
                                  (* 3 (+ 2 org-list-indent-offset))) "\\}\\(-\\) ")
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◇"))))))
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  :config
  )

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("" "" "" "" "" "" ""))
)

(use-package visual-fill-column
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t)
  :hook (org-mode . visual-fill-column-mode))

(provide 'mdrp-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-org.el ends here
