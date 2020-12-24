;;; mdrp-git.el --- -*- lexical-binding: t -*-

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

(use-package magit
  :ensure t
)

(use-package git-commit
  :hook (git-commit-mode . my-english-dict))

(use-package git-messenger
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

(use-package gitignore-mode
  :mode (("/\\.gitignore\\'"      . gitignore-mode)
         ("/info/exclude\\'"      . gitignore-mode)
         ("/git/ignore\\'"        . gitignore-mode)))

(provide 'mdrp-git)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-git.el ends here
