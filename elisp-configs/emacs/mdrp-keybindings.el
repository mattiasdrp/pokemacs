;;; mdrp-keybindings.el --- -*- lexical-binding: t -*-
;;
;; Description: Initialize Keybindings
;; Author: mdrp
;; Copyright (C) 2020 mdrp
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

;; These keys can be defined with no package
;; Package specific keys are defined inside the packages

(define-key input-decode-map [?\C-m] [C-m])
(define-key input-decode-map [?\C-i] [C-i])

(general-unbind
  "C-o"
  "C-f"
  "C-z"
  "C-x C-z"
  "M-z"
  "M-m"
  "M-h"
  "M-/"
  "M-l"
  "M-f"
  )

;;; Remappings

(general-define-key
 [remap kill-buffer]                  'kill-this-buffer
 [remap ispell-word]                  'flyspell-correct-at-point
 )

(general-define-key
 ;; Prefixed by C
 "C-x C-1"                 'delete-other-windows
 "C-x C-2"                 'split-window-below
 "C-x C-3"                 'split-window-right
 "C-x C-0"                 'delete-window

 "C-x &"                   'delete-other-windows
 "C-x é"                   'split-window-below
 "C-x \""                  'split-window-right
 "C-x à"                   'delete-window

 "C-x C-l"                 'toggle-truncate-lines
 "C-="                     'text-scale-increase
 "C-+"                     'text-scale-increase
 "C--"                     'text-scale-decrease
 "C-c h b"                 'describe-personal-keybindings
 ;; Create new line contextualised by the previous one
 ;; (will add a comment if in comment mode for example)
 "C-<return>"              'default-indent-new-line
 ;; emacs autocompletion (not like company)
 "C-<tab>"                 'dabbrev-expand
 "C-n"                     'next-error
 "C-p"                     'previous-error
 ;; windmove
 "C-x <left>"              'windmove-left
 "C-x <right>"             'windmove-right
 "C-x <up>"                'windmove-up
 "C-x <down>"              'windmove-down
 "C-x C-o"                 'ace-window

 ;; rotate buffers and window arrangements
 "C-c r w"                 'rotate-window
 "C-c r l"                 'rotate-layout

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
)

;;; Prefixed keys (local maps or not)

(general-define-key
 :prefix "M-z"
 ;; Setup shorcuts for window resize width and height
 "w"                       'mdrp/resize-window-width
 "h"                       'mdrp/resize-window-height
)

(general-define-key
 :prefix "M-h"
 "d"                       'hydra-dates/body
 ;; "a"                       'hydra-applications/body
 ;; "e"                       'hydra-eyebrowse/body
 ;; "f"                       'hydra-flycheck/body
 ;; "g"                       'hydra-git/body
 ;; "i"                       'hydra-ivy/body
 ;; "o"                       'me/hydra-super-maybe
 ;; "p"                       'hydra-projectile/body
 ;; "s"                       'hydra-system/body
 ;; "u"                       'hydra-ui/body
 ;; "w"                       'hydra-windows/body
 ;; "x"                       'hydra-x/body
 )

;;; Local maps

;; emacs autocompletion in the minibuffer (search, search file, M-x etc)
(general-def minibuffer-local-map
  "C-<tab>" 'dabbrev-expand)

(general-def flyspell-mouse-map
  "RET"                     'flyspell-correct-at-point
  [return]                  'flyspell-correct-at-point
  )

(provide 'mdrp-keybindings)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-keybindings.el ends here
