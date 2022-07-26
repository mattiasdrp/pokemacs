;;; mdrp-packages.el --- -*- lexical-binding: t -*-

;; Copyright (c) 2020-2020 mdrp and contributors.

;; Author: mdrp
;; Maintainer: mdrp <https://github.com/mattiasdrp>
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

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("elpa" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; ConfigurePackageManager
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key)
  )

;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)
;; -ConfigureUsePackage

;; Will be used to download non-emacs packages needed by emacs packages
(use-package use-package-ensure-system-package
  :ensure t)

(use-package discover-my-major
  :defer t
  :bind ("C-h <C-m>" . discover-my-major))

(setq use-package-verbose t)

(use-package auto-package-update
  :custom
  (auto-package-update-show-preview t)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-version t)
  :config
  (auto-package-update-maybe)
  )

(defgroup mdrp-packages nil
  "Pokemacs packages."
  :group 'package
  :tag "Packages options group")

(defcustom use-spotify nil
  "If non-nil, uses the spotify packages"
  :group 'mdrp-packages
  :type 'boolean)

(defcustom use-latex nil
  "If non-nil, uses the LaTeX packages"
  :group 'mdrp-packages
  :type 'boolean)

(defcustom use-org nil
  "If non-nil, uses the Org packages"
  :group 'mdrp-packages
  :type 'boolean)

(defcustom use-smt nil
  "If non-nil, uses the SMT packages"
  :group 'mdrp-packages
  :type 'boolean)

(defcustom use-rust nil
  "If non-nil, uses the rust packages"
  :group 'mdrp-packages
  :type 'boolean)

(defcustom use-ocaml t
  "If non-nil, uses the OCaml packages"
  :group 'mdrp-packages
  :type 'boolean)

(defcustom use-reason t
  "If non-nil, uses the Reason packages"
  :group 'mdrp-packages
  :type 'boolean)

(defcustom use-usuba nil
  "If non-nil, uses the Usuba packages"
  :group 'mdrp-packages
  :type 'boolean)

(defcustom use-python nil
  "If non-nil, uses the Python packages"
  :group 'mdrp-packages
  :type 'boolean)

(defcustom use-fsharp nil
  "If non-nil, uses the F# packages"
  :group 'mdrp-packages
  :type 'boolean)

(defcustom use-web nil
  "If non-nil, uses the web packages"
  :group 'mdrp-packages
  :type 'boolean)

(defcustom use-markdown t
  "If non-nil, uses the markdown packages"
  :group 'mdrp-packages
  :type 'boolean)

(defcustom use-company t
  "If non-nil, uses the company packages"
  :group 'mdrp-packages
  :type 'boolean)

(defcustom use-treemacs t
  "If non-nil, uses the treemacs packages"
  :group 'mdrp-packages
  :type 'boolean)

(defcustom use-god nil
  "If non-nil, uses the god (mode) packages"
  :group 'mdrp-packages
  :type 'boolean)

(defcustom use-rainbow nil
  "If non-nil, uses the rainbow packages"
  :group 'mdrp-packages
  :type 'boolean)

(defcustom use-visual-fill nil
  "If non-nil, fill the frame when there's only one"
  :group 'mdrp-packages
  :type 'boolean)

(defcustom use-vertical-split nil
  "If non-nil, split window vertically when possible"
  :group 'mdrp-packages
  :type 'boolean)

(provide 'mdrp-packages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-packages.el ends here
