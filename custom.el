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
 '(ac-use-fuzzy nil)
 '(backup-directory-alist '(("." . "~/.local/share/emacs/backups")))
 '(blink-matching-paren t)
 '(blink-matching-paren-dont-ignore-comments t)
 '(blink-matching-paren-on-screen t)
 '(column-number-mode t)
 '(comment-style 'indent)
 '(company-idle-delay nil)
 '(company-minimum-prefix-length 1)
 '(company-tooltip-align-annotations t)
 '(compilation-context-lines 2)
 '(compilation-error-screen-columns nil)
 '(compilation-scroll-output t)
 '(compilation-search-path '(nil "src"))
 '(compilation-window-height 12)
 '(delete-selection-mode t)
 '(electric-indent-mode nil)
 '(enable-recursive-minibuffers t)
 '(font-lock-global-modes t)
 '(global-auto-revert-mode t)
 '(global-nlinum-mode t)
 '(global-prettify-symbols-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ivy-count-format "%d/%d ")
 '(ivy-height 10)
 '(ivy-use-selectable-prompt t)
 '(ivy-use-virtual-buffers t)
 '(line-move-visual t)
 '(minions-mode t)
 '(minions-mode-line-lighter "☰")
 '(next-error-highlight t)
 '(next-error-highlight-no-select t)
 '(next-line-add-newlines nil)
 '(org-startup-truncated nil)
 '(org-support-shift-select 'always)
 '(package-selected-packages
   '(merlin-mode tuareg outline-minor-faces pretty-outlines dash-functional outshine json-mode run-ocaml dune-mode why3-mode LaTeX-math-mode tex-site gitignore-mode org-present diminish delight org-bullets rainbow-mode aggressive-indent all-the-icons all-the-icons-ivy apropospriate-theme auctex auto-complete caml-debug caml-mode cargo company-math company-racer company-web counsel cubicle-mode dictionary dune flycheck-inline flycheck-ocaml flycheck-rust git-messenger magit merlin-eldoc minions multiple-cursors nlinum org-plus-contrib pandoc-mode php-mode quick-peek racer rotate separedit smartparens smex sort-words toml-mode undo-tree unfill use-package vlf web-mode which-key))
 '(prettify-symbols-unprettify-at-point nil)
 '(require-final-newline t)
 '(save-abbrevs 'silently)
 '(save-interprogram-paste-before-kill t)
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-paren-style 'expression)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil)
 '(truncate-lines nil)
 '(tuareg-electric-indent t)
 '(tuareg-prettify-symbols-full t)
 '(use-package-verbose t)
 '(visible-bell t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#2e3436" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 136 :width normal :foundry "CTDB" :family "Fira Code"))))
 '(ivy-current-match ((t (:extend t :background "dark olive green"))))
 '(outshine-level-1 ((t (:inherit outline-1 :foreground "sienna" :underline t))))
 '(show-paren-match ((t (:background "indian red" :underline nil))))
 '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin)))))
(provide 'custom)
;;; custom.el ends here
