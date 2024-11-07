;;; pokemacs-functions.el --- -*- lexical-binding: t -*-

;; Copyright (c) 2022 mattiasdrp and contributors.

;; Author: mattiasdrp
;; Maintainer: mattiasdrp <https://github.com/mattiasdrp>
;; Created: 17 august 2022
;; Version: 1.0
;; Licence: MIT
;; Keywords: emacs, init, convenience, configuration
;; URL: https://github.com/mattiasdrp/pokemacs

;;; Commentary:

;; Useful functions for pokemacs

;;; Code:

;; Custom comment function a bit more clever
;; https://www.emacswiki.org/emacs/CommentingCode
(defun pokemacs-comment-eclipse (&optional arg)
  "Replacement for the `comment-dwim' command.
If no region is selected and current line is not blank and we are not at the
end of the line, then comment current line.
Replaces default behaviour of `comment-dwim', when it inserts comment at the
end of the line. Provides the optional ARG used by `comment-dwim'"
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))


;; Resizes the window width based on the input
(defun pokemacs-resize-window-width (w)
  "Resizes the window width based on W."
  (interactive (list (if (> (count-windows) 1)
                         (read-number "Set the current window width in [1~9]x10%: ")
                       (error "You need more than 1 window to execute this function!"))))
  (window-resize nil (- (truncate (* (/ w 10.0) (frame-width))) (window-total-width)) t))

;; Resizes the window height based on the input
(defun pokemacs-resize-window-height (h)
  "Resizes the window height based on H."
  (interactive (list (if (> (count-windows) 1)
                         (read-number "Set the current window height in [1~9]x10%: ")
                       (error "You need more than 1 window to execute this function!"))))
  (window-resize nil (- (truncate (* (/ h 10.0) (frame-height))) (window-total-height)) nil))

(defun pokemacs-resize-window (width delta)
  "Resize the current window's size.  If WIDTH is non-nil, resize width by some DELTA."
  (if (> (count-windows) 1)
      (window-resize nil delta width)
    (error "You need more than 1 window to execute this function!")))

(defun prev-match () (interactive nil) (next-match -1))
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))

(defun pokemacs-sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (let ((temp-table (copy-syntax-table text-mode-syntax-table)))
    (with-syntax-table temp-table
      (modify-syntax-entry ?- "w" temp-table)
      (modify-syntax-entry ?_ "w" temp-table)
      (sort-regexp-fields reverse "\\w+" "\\&" beg end))))

(defvar-local pokemacs-hydra-super-body nil)

(defun pokemacs-update-other-buffer ()
  (interactive)
  (other-window 1)
  (revert-buffer nil t)
  (other-window -1))

;;; FROM DOOM EMACS

(defun pokemacs-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

;;;###autoload
(defmacro plist-put! (plist &rest rest)
  "Set each PROP VALUE pair in REST to PLIST in-place."
  `(cl-loop for (prop value)
            on (list ,@rest) by #'cddr
            do ,(if (symbolp plist)
                    `(setq ,plist (plist-put ,plist prop value))
                  `(plist-put ,plist prop value))))

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (pokemacs-enlist ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defmacro pokemacs-appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

(defun pokemacs-partially-apply-interactively (cmd arg)
  (lambda ()
    (interactive)
    (minibuffer-with-setup-hook
        (lambda ()
          (insert arg)
          (add-hook 'post-command-hook #'exit-minibuffer nil t))
      (call-interactively cmd))))

(defun pokemacs-customize-my-custom-variable (variable)
  (interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        (insert variable)
        (add-hook 'post-command-hook #'exit-minibuffer nil t))
    (call-interactively #'customize-set-variable)))

(defun pokemacs-restore-session (&optional columns)
  "Restore a session by creating the proper buffers."
  (interactive "P")
  (when use-visual-fill (visual-fill-column-mode -1))
  (setq columns-number (or columns pokemacs-columns))
  (setq current-prefix-arg nil)
  (setq middle-window (selected-window))
  (dotimes (_ (- columns-number 2))
    (setq middle-window (split-window-right))
    (select-window middle-window)
    (balance-windows))
  (select-window middle-window)
  ;; Third window, start with magit
  (setq magit-window (split-window-right))
  (select-window magit-window)
  (with-selected-window magit-window
    (defvar magit-display-buffer-function)
    (let ((magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))
      (magit-status-quick))
    ;; Lock after creating the magit buffer otherwise it's created in another window
    (locked-window-buffer-mode))
  ;; Fourth window below magit is the compilation window,
  ;; create a buffer without compiling
  (setq compilation-window (split-window-below))
  (select-window compilation-window)
  (set-window-buffer (selected-window) (get-buffer-create "*compilation*"))
  (with-selected-window compilation-window
    (locked-window-buffer-mode))
  ;; Fifth window below compilation is the lsp-help window,
  ;; create a buffer without calling lsp
  (setq lsp-window (split-window-below))
  (select-window lsp-window)
  (set-window-buffer (selected-window) (get-buffer-create "*lsp-help*"))
  (with-selected-window lsp-window
    (locked-window-buffer-mode))
  (select-window middle-window)
  (balance-windows))

(provide 'pokemacs-functions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pokemacs-functions.el ends here
