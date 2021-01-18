;;; mdrp-spotify.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-const.el
;; Description: Initialize Spotify
;; Author: mdrp
;; Copyright (C) 2020 mdrp
;; Version: 1.0
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes spotify
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

(use-package counsel-spotify
  :after ivy
  :init
  (define-prefix-command 'mdrp-spotify-map nil "Spotify-")
  :custom
  (counsel-spotify-client-id (get-secrets-config-value 'spotify-client-id))
  (counsel-spotify-client-secret (get-secrets-config-value 'spotify-client-secret))
  :bind-keymap ("s-x" . mdrp-spotify-map)
  :bind (
         (:map mdrp-spotify-map
               ("q"       . counsel-spotify-previous)
               ("<left>"  . counsel-spotify-previous)
               ("s"       . counsel-spotify-toggle-play-pause)
               ("<down>"  . counsel-spotify-toggle-play-pause)
               ("d"       . counsel-spotify-next)
               ("<right>" . counsel-spotify-next)
               ("z"       . counsel-spotify-play)
               ("<up>"    . counsel-spotify-play)
         )
         )
  )

(provide 'mdrp-spotify)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-spotify.el ends here
