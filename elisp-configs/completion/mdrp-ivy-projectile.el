;;; mdrp-ivy-projectile.el --- -*- lexical-binding: t -*-

;; Copyright (c) 2020-2020 mdrp and contributors.

;; Author: mdrp
;; Maintainer: mdrp <https://github.com/MonsieurPi>
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

(use-package ivy
  :init
  (ivy-mode 1)
  (let ((standard-search-fn #'+ivy-prescient-non-fuzzy)
        (alt-search-fn #'ivy--regex-ignore-order))
    (setq ivy-re-builders-alist
          `((counsel-rg     . ,standard-search-fn)
            (swiper         . ,standard-search-fn)
            (swiper-isearch . ,standard-search-fn)
            (t . ,alt-search-fn))
          ivy-more-chars-alist
          '((counsel-rg . 1)
            (counsel-search . 2)
            (t . 3))))
  :bind (
         ([remap switch-to-buffer]              . ivy-switch-buffer)
         ("C-c v"                               . ivy-push-view)
         ("C-c V"                               . ivy-switch-view)
         )
  :config
  ;; The default sorter is much to slow and the default for `ivy-sort-max-size'
  ;; is way too big (30,000). Turn it down so big repos affect project
  ;; navigation less.
  (setq ivy-sort-max-size 7500)
  (setq ivy-count-format "%d/%d ")
  ;; (setq ivy-height 10)
  ;; (setq ivy-use-selectable-prompt t)
  ;; (setq ivy-use-virtual-buffers t)
  ;; (setq ivy-wrap t)

  ;; Counsel changes a lot of ivy's state at startup; to control for that, we
  ;; need to load it as early as possible. Some packages (like `ivy-prescient')
  ;; require this.
  (require 'counsel nil t)
  (setq ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        projectile-completion-system 'ivy
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function #'ignore
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t)

  )


(use-package ivy-rich
  :after ivy
  :config
  (setq ivy-rich-parse-remote-buffer nil)

  (cl-pushnew '(+ivy-rich-buffer-icon)
              (cadr (plist-get ivy-rich-display-transformers-list
                               'ivy-switch-buffer))
              :test #'equal)

  (defun ivy-rich-bookmark-filename-or-empty (candidate)
    (let ((filename (ivy-rich-bookmark-filename candidate)))
      (if (not filename) "" filename)))

  ;; Enhance the appearance of a couple counsel commands
  (plist-put! ivy-rich-display-transformers-list
              'counsel-describe-variable
              '(:columns
                ((counsel-describe-variable-transformer (:width 40)) ; the original transformer
                 (+ivy-rich-describe-variable-transformer (:width 50)) ; display variable value
                 (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
              'counsel-M-x
              '(:columns
                ((counsel-M-x-transformer (:width 60))
                 (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
              ;; Apply switch buffer transformers to `counsel-projectile-switch-to-buffer' as well
              'counsel-projectile-switch-to-buffer
              (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer)
              'counsel-bookmark
              '(:columns
                ((ivy-rich-candidate (:width 0.5))
                 (ivy-rich-bookmark-filename-or-empty (:width 60)))))

  ;; Remove built-in coloring of buffer list; we do our own
  (setq ivy-switch-buffer-faces-alist nil)
  (ivy-set-display-transformer 'internal-complete-buffer nil)

  (progn
    (defvar ek/ivy-rich-cache
      (make-hash-table :test 'equal))

    (defun ek/ivy-rich-cache-lookup (delegate candidate)
      (let ((result (gethash candidate ek/ivy-rich-cache)))
        (unless result
          (setq result (funcall delegate candidate))
          (puthash candidate result ek/ivy-rich-cache))
        result))

    (defun ek/ivy-rich-cache-reset ()
      (clrhash ek/ivy-rich-cache))

    (defun ek/ivy-rich-cache-rebuild ()
      (mapc (lambda (buffer)
              (ivy-rich--ivy-switch-buffer-transformer (buffer-name buffer)))
            (buffer-list)))

    (defun ek/ivy-rich-cache-rebuild-trigger ()
      (ek/ivy-rich-cache-reset)
      (run-with-idle-timer 1 nil 'ek/ivy-rich-cache-rebuild))

    (advice-add 'ivy-rich--ivy-switch-buffer-transformer :around 'ek/ivy-rich-cache-lookup)
    (advice-add 'ivy-switch-buffer :after 'ek/ivy-rich-cache-rebuild-trigger))
  (ivy-rich-mode +1)
  )


(use-package all-the-icons-ivy
  :after ivy
  :config
  ;; `all-the-icons-ivy' is incompatible with ivy-rich's switch-buffer
  ;; modifications, so we disable them and merge them ourselves
  (setq all-the-icons-ivy-buffer-commands nil)

  (all-the-icons-ivy-setup)
  (use-package counsel-projectile
    :config
    (let ((all-the-icons-ivy-file-commands
           '(counsel-projectile
             counsel-projectile-find-file
             counsel-projectile-find-dir)))
      (all-the-icons-ivy-setup))))

(use-package counsel
  :defer t
  :bind (
         ([remap apropos]                  . counsel-apropos)
         ([remap bookmark-jump]            . counsel-bookmark)
         ([remap compile]                  . counsel-compile)
         ([remap describe-bindings]        . counsel-descbinds)
         ([remap describe-face]            . counsel-faces)
         ([remap describe-function]        . counsel-describe-function)
         ([remap describe-variable]        . counsel-describe-variable)
         ([remap evil-ex-registers]        . counsel-evil-registers)
         ([remap evil-show-marks]          . counsel-mark-ring)
         ([remap execute-extended-command] . counsel-M-x)
         ([remap find-file]                . counsel-find-file)
         ([remap find-library]             . counsel-find-library)
         ([remap imenu]                    . counsel-imenu)
         ([remap info-lookup-symbol]       . counsel-info-lookup-symbol)
         ([remap load-theme]               . counsel-load-theme)
         ([remap locate]                   . counsel-locate)
         ([remap org-goto]                 . counsel-org-goto)
         ([remap org-set-tags-command]     . counsel-org-tag)
         ([remap recentf-open-files]       . counsel-recentf)
         ([remap set-variable]             . counsel-set-variable)
         ([remap swiper]                   . counsel-grep-or-swiper)
         ([remap unicode-chars-list-chars] . counsel-unicode-char)
         ([remap yank-pop]                 . counsel-yank-pop))
  :config
  ;; (set-popup-rule! "^\\*ivy-occur" :size 0.35 :ttl 0 :quit nil)
  (defun mdrp/just-use-current-directory ()
    "Just return the directory of the current file or default-directory."
    (let ((bf (buffer-file-name (current-buffer))))
      (if bf
          (file-name-directory bf)
        default-directory)))

  (add-to-list 'counsel-compile-root-functions
               'mdrp/just-use-current-directory t)
  ;; Don't use ^ as initial input. Set this here because `counsel' defines more
  ;; of its own, on top of the defaults.
  (setq ivy-initial-inputs-alist nil)

  ;; REVIEW Counsel allows `counsel-rg-base-command' to be a string or list.
  ;;        Simpler to just force it to always be a list.
  (when (stringp counsel-rg-base-command)
    (setq counsel-rg-base-command (split-string counsel-rg-base-command)))

  ;; REVIEW Fix #3215: prevents mingw on Windows throwing an error trying to
  ;;        expand / to an absolute path. Remove this when it is fixed upstream
  ;;        in counsel.
  (when (and (memq system-type '(windows-nt ms-dos))
             (listp counsel-rg-base-command)
             (member "--path-separator" counsel-rg-base-command))
    (setf (cadr (member "--path-separator" counsel-rg-base-command))
          "/"))

  ;; Integrate with `helpful'
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable)

  ;; Record in jumplist when opening files via counsel-{ag,rg,pt,git-grep}
  (add-hook 'counsel-grep-post-action-hook #'better-jumper-set-jump)
  (add-hook 'counsel-grep-post-action-hook #'recenter)

  ;; Make `counsel-compile' projectile-aware
  (defun mdrp/just-use-current-directory ()
    "Just return the directory of the current file or default-directory."
    (let ((bf (buffer-file-name (current-buffer))))
      (if bf
          (file-name-directory bf)
        default-directory)))

  (add-to-list 'counsel-compile-root-functions #'mdrp/just-use-current-directory)
  (add-to-list 'counsel-compile-root-functions #'projectile-project-root)
  (use-package savehist
    ;; Persist `counsel-compile' history
    :config
    (add-to-list 'savehist-additional-variables 'counsel-compile-history))

  ;; `counsel-imenu' -- no sorting for imenu. Sort it by appearance in page.
  (add-to-list 'ivy-sort-functions-alist '(counsel-imenu))

  ;; `counsel-find-file'
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")
  (dolist (fn '(counsel-rg counsel-find-file))
    (ivy-add-actions
     fn '(("p" (lambda (path) (with-ivy-window (insert (file-relative-name path default-directory))))
           "insert relative path")
          ("P" (lambda (path) (with-ivy-window (insert path)))
           "insert absolute path")
          ("l" (lambda (path) (with-ivy-window (insert (format "[[./%s]]" (file-relative-name path default-directory)))))
           "insert relative org-link")
          ("L" (lambda (path) (with-ivy-window (insert (format "[[%s]]" path))))
           "Insert absolute org-link"))))

  (ivy-add-actions 'counsel-file-jump (plist-get ivy--actions-list 'counsel-find-file))

  ;; `counsel-search': use normal page for displaying results, so that we see
  ;; custom ddg themes (if one is set).
  (setf (nth 1 (alist-get 'ddg counsel-search-engines-alist))
        "https://duckduckgo.com/?q=")

  (defadvice! +ivy--counsel-file-jump-use-fd-rg-a (args)
    "Change `counsel-file-jump' to use fd or ripgrep, if they are available."
    :override #'counsel--find-return-list
    (cl-destructuring-bind (find-program . args)
        (cond ((when-let (fd (executable-find (or mdrp/fd-binary "fd")))
                 (append (list fd "-H" "--color=never" "--type" "file" "--type" "symlink" "--follow")
                         (if IS-WINDOWS '("--path-separator=/")))))
              ((executable-find "rg")
               (append (list "rg" "--files" "--follow" "--color=never" "--hidden" "-g!.git" "--no-messages")
                       (cl-loop for dir in projectile-globally-ignored-directories
                                collect "--glob"
                                collect (concat "!" dir))
                       (if IS-WINDOWS (list "--path-separator" "/"))))
              ((cons find-program args)))
      (unless (listp args)
        (user-error "`counsel-file-jump-args' is a list now, please customize accordingly."))
      (counsel--call
       (cons find-program args)
       (lambda ()
         (goto-char (point-min))
         (let (files)
           (while (< (point) (point-max))
             (push (buffer-substring (line-beginning-position) (line-end-position))
                   files)
             (forward-line 1))
           (nreverse files)))))))


(use-package counsel-projectile
  :defer t
  :bind (
         ([remap projectile-find-dir]         . counsel-projectile-find-dir)
         ([remap projectile-switch-to-buffer] . counsel-projectile-switch-to-buffer)
         ([remap projectile-grep]             . counsel-projectile-grep)
         ([remap projectile-ag]               . counsel-projectile-ag)
         ([remap projectile-switch-project]   . counsel-projectile-switch-project))
  :config

  ;; no highlighting visited files; slows down the filtering
  (ivy-set-display-transformer #'counsel-projectile-find-file nil)

  (setq counsel-projectile-sort-files t))


(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))


(use-package ivy-posframe
  :config
  (ivy-posframe-mode)
  (setq ivy-fixed-height-minibuffer nil
        ivy-posframe-border-width 10
        ivy-posframe-parameters
        `((min-width . 90)
          (min-height . ,ivy-height)))

  ;; default to posframe display function
  (setf (alist-get t ivy-posframe-display-functions-alist)
        #'+ivy-display-at-frame-center-near-bottom-fn)

  ;; posframe doesn't work well with async sources (the posframe will
  ;; occasionally stop responding/redrawing), and causes violent resizing of the
  ;; posframe.
  (dolist (fn '(swiper counsel-rg counsel-grep counsel-git-grep))
    (setf (alist-get fn ivy-posframe-display-functions-alist)
          #'ivy-display-function-fallback))

  )

(use-package ivy-avy
  :after ivy)

(use-package ivy-prescient
  :hook (ivy-prescient-mode . prescient-persist-mode)
  :commands +ivy-prescient-non-fuzzy
  :init
  (setq prescient-filter-method
        '(literal regexp initialism))
  :config
  (ivy-prescient-mode)
  (setq ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer
          lsp-ivy-workspace-symbol ivy-resume ivy--restore-session
          counsel-grep counsel-git-grep counsel-rg counsel-ag
          counsel-ack counsel-fzf counsel-pt counsel-imenu
          counsel-yank-pop counsel-recentf counsel-buffer-or-recentf)
        ivy-prescient-retain-classic-highlighting t)
  (defun +ivy-prescient-non-fuzzy (str)
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))

  ;; NOTE prescient config duplicated with `company'
  (setq prescient-save-file (expand-file-name "prescient-save.el" user-emacs-directory)))

;;;###package swiper
(setq swiper-action-recenter t)

;;;###package amx
(setq amx-save-file (expand-file-name "amx-items" user-emacs-directory))  ; used by `counsel-M-x'

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind (
         ([remap describe-function] . counsel-describe-function)
         ([remap describe-command] . helpful-command)
         ([remap describe-variable] . counsel-describe-variable)
         ([remap describe-key] . helpful-key)
         )
  )

(use-package projectile
  :bind
  ("M-p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  :init
  (projectile-mode 1)
  ;; :config
  ;; (add-to-list 'projectile-globally-ignored-directories "node_modules")
  )

(provide 'mdrp-ivy-projectile)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mdrp-ivy-projectile.el ends here
