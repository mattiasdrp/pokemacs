;;; post-init.el --- Emacs customization -*- lexical-binding: t -*-

;; Copyright (c) 2022 mattiasdrp and contributors.

;; Author: mattiasdrp
;; Maintainer: mattiasdrp <https://github.com/mattiasdrp>
;; Created: 17 august 2022
;; Version: 1.0
;; Licence: MIT
;; Keywords: emacs, init, convenience, configuration
;; URL: https://github.com/mattiasdrp/pokemacs

  ;;; Commentary:
;; This file will be loaded when emacs has finished initializing everything
;; and allows to override some bindings and behaviours that are not
;; controlled by custom.el

;;; Code:

(when use-mu4e
  (use-package smtpmail
    :ensure nil
    :ensure-system-package msmtp))

;; (load-file (expand-file-name "~/mu4e/mu4e.el"))

(defgroup tezos nil
  "Tezos values."
  :group 'pokemacs
  :tag "ꜩ Tezos")

(defcustom use-tezos-utilities t
"If non-nil, uses tezos utilities."
  :group 'tezos
  :type 'boolean
  :tag "ꜩ Tezos utilities")

(defcustom tezos-organisation-name "Nomadic Labs"
"If non-nil, uses tezos utilities."
  :group 'tezos
  :type 'boolean
  :tag "ꜩ Tezos organisation name")

(defcustom tezos-organisation-email "contact@nomadic-labs.com"
"If non-nil, uses tezos utilities."
  :group 'tezos
  :type 'boolean
  :tag "ꜩ Tezos organisation email")

(when use-tezos-utilities
  (defun pokemacs-concat-with-newlines (&rest strings)
    "Concatenate STRINGS with a newline between each."
    (apply 'concat (mapcar (lambda (s) (concat s "\n")) strings)))

  (defun pokemacs-insert-mit-spdx-header ()
    (let ((year (format-time-string "%Y")))
      (insert
       (cond

        ;; OCaml file
        ((derived-mode-p 'tuareg-mode)
         (pokemacs-concat-with-newlines
          "(*****************************************************************************)"
          (format "(*%77s" "*)")
          (format "(* SPDX-License-Identifier: MIT%48s" "*)")
          (concat "(* SPDX-FileCopyrightText: "
                  year
                  " "
                  (concat
                   tezos-organisation-name
                   ". <"
                   tezos-organisation-email
                   (format "> %6s" "*)")))
          "(*                                                                           *)"
          "(*****************************************************************************)"))

        ;; Rust file
        ((derived-mode-p 'rust-mode)
         (pokemacs-concat-with-newlines
          "//"
          "// SPDX-License-Identifier: MIT"
          (concat
           "// SPDX-FileCopyrightText: "
           year
           " "
           tezos-organisation-name
           " "
           tezos-organisation-email)
          "//"))))))

  (defun auto-insert ()
    "Insert default contents into new files if variable `auto-insert' is non-nil.
Matches the visited file name against the elements of `auto-insert-alist'."
    (interactive)
    (and (not buffer-read-only)
         (or (eq this-command 'auto-insert)
             (and auto-insert
                  (bobp) (eobp)))
         (let* ((case-fold-search nil)
                (desc nil)
                (_ (message "here %S" this-command))
                ;; Find first matching alist entry.
                (action
                 (seq-some
                  (pcase-lambda (`(,cond . ,action))
                    (if (atom cond)
                        (setq desc cond)
                      ;; if `cond' is a predicate, don't split it but set `desc' to a custom string
                      (if (and (consp cond) (equal (car cond) 'predicate))
                          (setq desc "predicate")
                        (setq desc (cdr cond)
                              cond (car cond))))
                    (when (cond
                           ;; `cond' should be a major-mode variable
                           ((symbolp cond)
                            (derived-mode-p cond))

                           ;; `cond' should be a predicate that takes no argument
                           ((and (consp cond) (equal (car cond) 'predicate))
                            (funcall (cadr cond)))

                           ;; cond should be a regexp
                           (t
                            (and buffer-file-name
                                 (string-match cond buffer-file-name))))
                      action))
                  auto-insert-alist)))
           (goto-char 1)
           ;; Now, if we found something, do it
           (and action
                (or (not (stringp action))
                    (file-readable-p (expand-file-name
                                      action auto-insert-directory)))
                (or (not auto-insert-query)
                    (if (eq auto-insert-query 'function)
                        (eq this-command 'auto-insert))
                    (y-or-n-p (format auto-insert-prompt desc)))
                (mapc
                 (lambda (action)
                   (if (stringp action)
                       (if (file-readable-p
                            (setq action (expand-file-name
                                          action auto-insert-directory)))
                           (insert-file-contents action))
                     (save-window-excursion
                       ;; make buffer visible before skeleton or function
                       ;; which might ask the user for something
                       (switch-to-buffer (current-buffer))
                       (if (and (consp action)
                                (not (functionp action)))
                           (skeleton-insert action)
                         (funcall action)))))
                 (if (vectorp action)
                     action
                   (vector action))))
           (and (buffer-modified-p)
                (not (eq this-command 'auto-insert))
                (set-buffer-modified-p (eq auto-insert t)))))
    ;; Return nil so that it could be used in
    ;; `find-file-not-found-functions', though that's probably inadvisable.
    nil)

  (defun pokemacs-get-git-repo-url ()
    "Return the URL of the Git repository for the current project."
    (interactive)
    (when-let* ((git-root (vc-git-root default-directory))
                (remotes (with-temp-buffer
                           (let ((default-directory git-root))
                             (call-process "git" nil t nil "remote")
                             (split-string (buffer-string) "\n" t))))
                (remote (car remotes)))
      (vc-git-repository-url default-directory remote)))

  (defun pokemacs-split-git-url (url)
    "Split a GIT URL into protocol, user and repo."
    (let ((regex "^\\(?1:https?\\|ssh\\|git\\|ftps?\\)\\(://\\|@\\)\\(\\([^/@]+\\)@\\)?\\([^/:]+\\)[/:]\\(?2:[^/:]+\\)/\\(\\(?3:[^.]+\\)\\(.git\\)?/?\\)$"))
      (let* ((_ (string-match regex url))
             (protocol (match-string 1 url))
             (org (match-string 2 url))
             (repo (match-string 3 url)))
        (list protocol org repo))))

  (defun pokemacs-tezos-repo? ()
    (when-let* ((git-url (pokemacs-get-git-repo-url))
                (url-destructed (pokemacs-split-git-url git-url)))
      (cl-destructuring-bind (_ org repo) url-destructed
        (and (string-equal "tezos" org) (string-equal "tezos" repo)))))

  (use-package autoinsert
    :ensure nil
    ;; :after projectile
    :demand t
    :init
    (auto-insert-mode t)
    :config
    (add-to-list 'auto-insert-alist
                 '(pokemacs-tezos-repo? . pokemacs-insert-mit-spdx-header))
    (message "`auto-insert-mode' loaded")))

;; (general-unbind
;;   "C-o"
;;   )

;; (general-unbind
;;   :keymaps 'tuareg-mode-map
;;   "C-c TAB"
;;   )

;; (general-define-key
;;  "C-x 1"                 'delete-other-windows
;;  )

;; (general-define-key
;;  :prefix "M-z"
;;  "w"                       'mdrp/resize-window-width
;;  "h"                       'mdrp/resize-window-height)

;; (general-define-key
;;  :keymaps 'tuareg-mode-map
;;  "C-x M-1"                 'delete-other-windows
;;  )

(provide 'post-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; post-init.el ends here
