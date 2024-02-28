;;; post-custom.el --- Emacs customization -*- lexical-binding: t -*-

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

(use-package smtpmail
  :ensure nil
  :ensure-system-package msmtp)

(load-file (expand-file-name "~/mu4e/mu4e.el"))

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

(provide 'post-custom)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; post-custom.el ends here
