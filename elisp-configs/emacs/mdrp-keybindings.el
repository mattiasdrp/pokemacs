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

;; DefBindings
;; Unbind unneeded keys
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "M-m") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "M-/") nil)
;; Truncate lines
(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)
;; Adjust font size like web browsers
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
;; Move up/down paragraph
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)

(define-key input-decode-map [?\C-m] [C-m])
(define-key input-decode-map [?\C-i] [C-i])

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; -DefBindings

;; Setup shorcuts for window resize width and height
(global-set-key (kbd "C-z w") #'mdrp/resize-window-width)
(global-set-key (kbd "C-z h") #'mdrp/resize-window-height)

;; Setup shorcuts for window resize width and height
(global-set-key (kbd "M-J") (lambda () (interactive) (mdrp/resize-window t 5)))
(global-set-key (kbd "M-L") (lambda () (interactive) (mdrp/resize-window t -5)))

(global-set-key (kbd "M-I") (lambda () (interactive) (mdrp/resize-window nil 5)))
(global-set-key (kbd "M-K") (lambda () (interactive) (mdrp/resize-window nil -5)))

(global-set-key (kbd "C-c h b") 'describe-personal-keybindings)

;; Custom comment overwriting comment-dwim key binding
(global-set-key (kbd "M-;") 'mdrp/comment-eclipse)
;; Create new line contextualised by the previous one
;; (will add a comment if in comment mode for example)
(global-set-key (kbd "C-<return>") 'default-indent-new-line)
;; emacs autocompletion (not like company)
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
;; emacs autocompletion in the minibuffer (search, search file, M-x etc)
(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)

;; Shortcuts used for compilation and other bound to function keys
(global-set-key [f3] 'next-match)
(defun prev-match () (interactive nil) (next-match -1))
(global-set-key [(shift f3)] 'prev-match)
(global-set-key [f4]   'goto-line)
(global-set-key [f5]   'compile)
(global-set-key [f6]   'recompile)
(global-set-key [f7]   'next-error)
(global-set-key [f8]   'normal-mode)

(global-set-key (kbd "C-n") 'next-error)
(global-set-key (kbd "C-p") 'previous-error)

(global-set-key (kbd "M-<f1>") 'kill-this-buffer)
(global-set-key (kbd "M-g") 'goto-line)

;; Rewriting scroll up and down
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))

(global-set-key [mouse-4]   'down-slightly)
(global-set-key [mouse-5]   'up-slightly)

;; enable toggling paragraph un-fill
(define-key global-map (kbd "M-Q") 'unfill-paragraph)
;; *****************************************************************************

;;;;; Window management (move):

;; windmove
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)

;; Store and recall window layouts (views!)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-switch-view)

;; use ace-window for navigating windows
(global-set-key (kbd "C-x C-o") 'ace-window)
(with-eval-after-load "ace-window"
  (setq aw-dispatch-always t)
  (set-face-attribute 'aw-leading-char-face nil :height 2.5))

;; rotate buffers and window arrangements
(global-set-key (kbd "C-c r w") 'rotate-window)
(global-set-key (kbd "C-c r l") 'rotate-layout)
;; *****************************************************************************

;; Org keys
(global-set-key (kbd "M-o l") 'org-store-link)
(global-set-key (kbd "M-o a") 'org-agenda)
(global-set-key (kbd "M-j") 'org-goto)

;; Tab bar mode
(global-set-key (kbd "M-q") 'tab-next)
(global-set-key (kbd "M-d") 'tab-previous)
(global-set-key (kbd "C-S-n") 'tab-new)
(global-set-key (kbd "C-S-q") 'tab-close)

;; Delete block
(global-set-key (kbd "C-d") 'delete-block-forward)
(global-set-key (kbd "C-<backspace>") 'delete-block-backward)
(global-set-key (kbd "M-<backspace>") 'delete-block-backward)
(global-set-key (kbd "M-DEL") 'delete-block-backward)

(provide 'mdrp-keybindings)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-keybindings.el ends here
