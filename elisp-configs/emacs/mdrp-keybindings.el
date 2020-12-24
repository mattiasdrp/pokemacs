;;; mdrp-keybindings.el --- -*- lexical-binding: t -*-
;;
;; Description: Initialize Keybindings
;; Author: Mattias
;; Copyright (C) 2020 Mattias
;; Version: 1.0
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes keybindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package general
  :ensure t)

(use-package god-mode
  :init
  (setq god-mode-enable-function-key-translation nil)
  :config
  (defun my-god-mode-update-modeline ()
    (let ((limited-colors-p (> 257 (length (defined-colors)))))
      (cond (god-local-mode (progn
                              (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
                              (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
            (t (progn
                 (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
                 (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832")))))))

  (add-hook 'god-mode-enabled-hook #'my-god-mode-update-modeline)
  (add-hook 'god-mode-disabled-hook #'my-god-mode-update-modeline)
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)
  )

(define-key input-decode-map [?\C-m] [C-m])
(define-key input-decode-map [?\C-i] [C-i])

(general-unbind
  "C-o"
  "C-z"
  "M-z"
  "M-m"
  "C-x C-z"
  "M-/"
  )

;;; Remappings

(general-define-key
 [remap ispell-word]                  'flyspell-correct-at-point
 [remap switch-to-buffer]             'ivy-switch-buffer
 [remap apropos]                      'counsel-apropos
 [remap bookmark-jump]                'counsel-bookmark
 [remap compile]                      'counsel-compile
 [remap describe-bindings]            'counsel-descbinds
 [remap describe-face]                'counsel-faces
 [remap describe-function]            'counsel-describe-function
 [remap describe-variable]            'counsel-describe-variable
 [remap evil-ex-registers]            'counsel-evil-registers
 [remap evil-show-marks]              'counsel-mark-ring
 [remap execute-extended-command]     'counsel-M-x
 [remap find-file]                    'counsel-find-file
 [remap find-library]                 'counsel-find-library
 [remap imenu]                        'counsel-imenu
 [remap info-lookup-symbol]           'counsel-info-lookup-symbol
 [remap load-theme]                   'counsel-load-theme
 [remap locate]                       'counsel-locate
 [remap org-goto]                     'counsel-org-goto
 [remap org-set-tags-command]         'counsel-org-tag
 [remap recentf-open-files]           'counsel-recentf
 [remap set-variable]                 'counsel-set-variable
 [remap swiper]                       'counsel-grep-or-swiper
 [remap unicode-chars-list-chars]     'counsel-unicode-char
 [remap yank-pop]                     'counsel-yank-pop
 [remap projectile-find-dir]          'counsel-projectile-find-dir
 [remap projectile-switch-to-buffer]  'counsel-projectile-switch-to-buffer
 [remap projectile-grep]              'counsel-projectile-grep
 [remap projectile-ag]                'counsel-projectile-ag
 [remap projectile-switch-project]    'counsel-projectile-switch-project
 [remap describe-function]            'counsel-describe-function
 [remap describe-command]             'helpful-command
 [remap describe-variable]            'counsel-describe-variable
 [remap describe-key]                 'helpful-key
 )

(general-define-key
 ;; Prefixed by C

 "C-x C-l"                 'toggle-truncate-lines
 "C-="                     'text-scale-increase
 "C-+"                     'text-scale-increase
 "C--"                     'text-scale-decrease
 ;; Make ESC quit prompts
 "C-c h b"                 'describe-personal-keybindings
 ;; Create new line contextualised by the previous one
 ;; (will add a comment if in comment mode for example)
 "C-<return>"              'default-indent-new-line
 ;; emacs autocompletion (not like company)
 "C-<tab>"                 'dabbrev-expand
;; Setup shorcuts for window resize width and height
 "C-n"                     'next-error
 "C-p"                     'previous-error
;; windmove
 "C-x <left>"              'windmove-left
 "C-x <right>"             'windmove-right
 "C-x <up>"                'windmove-up
 "C-x <down>"              'windmove-down

 "C-c v"                   'ivy-push-view
 "C-c V"                   'ivy-switch-view

 "C-x C-o"                 'ace-window

 ;; rotate buffers and window arrangements
 "C-c r w"                 'rotate-window
 "C-c r l"                 'rotate-layout

 ;; Tab bar mode
 "C-S-n"                   'tab-new
 "C-S-q"                   'tab-close

 "C-d"                     'delete-block-forward
 "C-<backspace>"           'delete-block-backward

 "C-c C-e"                 'separedit

 ;; Prefixed by M

 "M-J"                     (lambda () (interactive) (mdrp/resize-window t 5))
 "M-L"                     (lambda () (interactive) (mdrp/resize-window t -5))
 "M-I"                     (lambda () (interactive) (mdrp/resize-window nil 5))
 "M-K"                     (lambda () (interactive) (mdrp/resize-window nil -5))
 ;; Custom comment overwriting comment-dwim key binding
 "M-;"                     'mdrp/comment-eclipse
 "M-n"                     'forward-paragraph
 "M-p"                     'backward-paragraph
 "M-<f1>"                  'kill-this-buffer
 "M-g"                     'goto-line
 "M-Q"                     'unfill-paragraph

 "M-q"                     'tab-next
 "M-d"                     'tab-previous

 "M-<backspace>"           'delete-block-backward
 "M-DEL"                   'delete-block-backward

 "M-+"                     'hs-toggle-hiding
 "M-*"                     'hs-show-all
 ;; Function keys

 [f3]                      'next-match
 [(shift f3)]              'prev-match
 [f4]                      'goto-line
 [f5]                      'compile
 [f6]                      'recompile
 [f7]                      'next-error
 [f8]                      'normal-mode

 ;; Mouse
 [mouse-4]                 'down-slightly
 [mouse-5]                 'up-slightly

 "<escape>"                'god-mode-all
 "²"                       'god-mode-all
 "C-²"                     'god-mode-all
)


;;; Prefixed keys

(general-define-key
 :prefix "M-z"
 ;; Setup shorcuts for window resize width and height
 "w"                       'mdrp/resize-window-width
 "h"                       'mdrp/resize-window-height
)

(general-define-key
 :prefix "M-o"
 "l"                       'org-store-link
 "a"                       'org-agenda
)

(general-define-key
 :prefix "M-m"
 "g"                       'magit-status
 "G"                       'git-messenger:popup-message
 "M-g"                     'magit-dispatch
 )

(general-define-key
 :keymap 'cm-map
 :prefix "C-o"
 ;; HIDE
 ;; Hide everything but the top-level headings
 "q"                       'outline-hide-sublevels
 ;; Hide everything but headings (all body lines)
 "t"                       'outline-hide-body
 ;; Hide other branches
 "o"                       'outline-hide-other
 ;; Hide this entry's body
 "c"                       'outline-hide-entry
 ;; Hide body lines in this entry and sub-entries
 "l"                       'outline-hide-leaves
 ;; Hide everything in this entry and sub-entries
 "d"                       'outline-hide-subtree
 ;; SHOW
 ;; Show (expand) everything
 "a"                       'outline-show-all
 ;; Show this heading's body
 "e"                       'outline-show-entry
 ;; Show this heading's immediate child sub-headings
 "i"                       'outline-show-children
 ;; Show all sub-headings under this heading
 "k"                       'outline-show-branches
 ;; Show (expand) everything in this heading & below
 "s"                       'outline-show-subtree
 ;; MOVE
 ;; Up
 "u"                       'outline-up-heading
 ;; Next
 "n"                       'outline-next-visible-heading
 ;; Previous
 "p"                       'outline-previous-visible-heading
 ;; Forward - same level
 "f"                       'outline-forward-same-level
 ;; Backward - same level
 "b"                       'outline-backward-same-level
 )

;;; Local maps

;; emacs autocompletion in the minibuffer (search, search file, M-x etc)
(general-def minibuffer-local-map
  "C-<tab>" 'dabbrev-expand)

(general-def org-mode-map
  "M-j"                     'org-goto
  )

(general-def flyspell-mouse-map
  "RET"                     'flyspell-correct-at-point
  [return]                  'flyspell-correct-at-point)

(general-def outline-minor-mode-map
  "M-j"                     'oi-jump
  )

(general-def rust-mode-map
  "C-M-;"                   'rust-doc-comment-dwim-following
  "C-M-,"                   'rust-doc-comment-dwim-enclosing
  "C-c C-t"                 'racer-find-definition
  )

(general-def json-mode-map
  "C-c <tab>"               'json-mode-beautify
)

(provide 'mdrp-keybindings)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-keybindings.el ends here
