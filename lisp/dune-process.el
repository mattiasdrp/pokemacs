;;; dune-process.el --- Dune Process Major Mode -*-lexical-binding: t-*-

;; Copyright (C) 2015  Kevin W. van Rooijen

;; Author: Kevin W. van Rooijen <kevin.van.rooijen@attichacker.com>
;; Keywords: processes, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Dune Process Major mode.
;; Used to run Dune background processes.
;; Current supported Dune functions:
;;  * dune-process-build              - Compile the current project.
;;  * dune-process-clean              - Remove the target directory.
;;  * dune-process-exec               - Execute the current
;;  * dune-process-init               - Execute the current

;;
;;; Code:

(require 'compile)
(require 'button)
;; (require 'rust-mode)
;; (require 'markdown-mode)

(defgroup dune-process nil
  "Dune Process group."
  :prefix "dune-process-"
  :group 'dune)

(defvar opam-bin
  (let ((reply (shell-command-to-string "opam config var bin --safe")))
    (when reply (substring reply 0 -1))))

(defcustom dune-process--custom-path-to-bin
  (or (executable-find "dune")
      (expand-file-name "dune" opam-bin)
      "/usr/local/bin/dune")
  "Custom path to the dune executable"
  :type 'file
  :group 'dune-process)

(defcustom dune-process--ocamlc-cmd
  (or (executable-find "ocamlc")
      (expand-file-name "ocamlc" opam-bin)
      "/usr/local/bin/ocamlc")
  "Custom path to the rustc executable"
  :type 'file
  :group 'dune-process)


(defcustom dune-process--ocamlopt-cmd
  (or (executable-find "ocamlopt")
      (expand-file-name "ocamlopt" opam-bin)
      "/usr/local/bin/ocamlopt")
  "Custom path to the rustc executable"
  :type 'file
  :group 'dune-process)

(defcustom dune-process--open-file-after-new nil
  "Open the created project file after generating a new project"
  :group 'dune-process
  :type 'boolean)

(defvar dune-process-mode-map
  (nconc (make-sparse-keymap) compilation-mode-map)
  "Keymap for Dune major mode.")

(defvar dune-process-last-command nil "Command used last for repeating.")

(make-variable-buffer-local 'dune-process-last-command)

(defcustom dune-process--command-build "build"
  "Subcommand used by `dune-process-build'."
  :type 'string)

(defcustom dune-process--command-clean "clean"
  "Subcommand used by `dune-process-clean'."
  :type 'string)

(defcustom dune-process--command-exec "exec"
  "Subcommand used by `dune-process-exec-name'."
  :type 'string)

(defcustom dune-process--command-init "init"
  "Subcommand used by `dune-process-init'."
  :type 'string)

(defface dune-process--ok-face
  '((t (:inherit success)))
  "Ok face"
  :group 'dune-process)

(defface dune-process--error-face
  '((t (:inherit error)))
  "Error face"
  :group 'dune-process)

(defface dune-process--warning-face
  '((t (:inherit warning)))
  "Warning face"
  :group 'dune-process)

(defface dune-process--pointer-face
  '((t (:inherit font-lock-negation-char-face)))
  "Pointer face"
  :group 'dune-process)

(defface dune-process--standard-face
  '((t (:inherit font-lock-comment-face)))
  "Standard face"
  :group 'dune-process)

(defface dune-process--errno-face
  '((t (:inherit link)))
  "Error number face"
  :group 'dune-process)

;; (defconst dune-process-test-regexp "^[[:space:]]*fn[[:space:]]*"
;;   "Regex to find Rust unit test function.")

;; (defconst dune-process-test-mod-regexp "^[[:space:]]*mod[[:space:]]+[[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*[[:space:]]*{")

(defconst dune-process-font-lock-keywords
  '(("^error\\:?" . 'dune-process--error-face)
    ("^warning\\:?" . 'dune-process--warning-face)
    ("^\s*\\^\\~*\s*$" . 'dune-process--pointer-face)
    ("^\s*Compiling.*" . 'dune-process--standard-face)
    ("^\s*Running.*" . 'dune-process--standard-face)
    ("^\s*Updating.*" . 'dune-process--standard-face)
    ("test result: FAILED." . 'dune-process--error-face)
    ("test result: ok." . 'dune-process--ok-face)
    ("test\s.*\sFAILED" . 'dune-process--error-face)
    ("test\s.*\sok" . 'dune-process--ok-face))
  "Minimal highlighting expressions for dune-process mode.")

;; Bind `case-fold-search' to nil before using the regex.
(defconst dune-process--errno-regex "\\bE[0-9]\\{4\\}\\b"
  "A regular expression to match Rust error number.")

(define-button-type 'rustc-errno
  'follow-link t
  'face 'dune-process--errno-face
  'action #'dune-process--explain-action)

(defun dune-process--defun-at-point-p ()
  (string-match dune-process-test-regexp
                (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))))

(defun dune-process--project-root (&optional extra)
  "Find the root of the current Dune project."
  (let ((root (locate-dominating-file (or buffer-file-name default-directory) "dune-project")))
    (when root
      (file-truename (concat root extra)))))

(define-derived-mode dune-process-mode compilation-mode "Dune-Process."
  "Major mode for the Dune process buffer."
  (use-local-map dune-process-mode-map)
  (setq major-mode 'dune-process-mode)
  (setq mode-name "Dune-Process")
  (setq-local truncate-lines t)
  (run-hooks 'dune-process-mode-hook)
  (font-lock-add-keywords nil dune-process-font-lock-keywords))

(defun dune-process--finished-sentinel (process event)
  "Execute after PROCESS return and EVENT is 'finished'."
  (compilation-sentinel process event)
  (when (equal event "finished\n")
    (message "Dune Process finished.")))

(defun dune-process--activate-mode (buffer)
  "Execute commands BUFFER at process start."
  (with-current-buffer buffer
    (funcall 'dune-process-mode)
    (setq-local window-point-insertion-type t)))

(defun dune-process--start (name command &optional last-cmd opens-external)
  "Start the Dune process NAME with the dune command COMMAND.
OPENS-EXTERNAL is non-nil if the COMMAND is expected to open an external application.
Returns the created process."
  (let* ((buffer (concat "*compilation*"))
         (project-root (dune-process--project-root))
         (cmd
          (or last-cmd
              (dune-process--maybe-read-command
               (dune-process--augment-cmd-for-os opens-external
                                                  (mapconcat #'identity (list (shell-quote-argument dune-process--custom-path-to-bin)
                                                                              command
                                                                              )
                                                             " ")))))
         (default-directory (or project-root default-directory)))
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda ()
                         (and project-root
                              buffer-file-name
                              (string-prefix-p project-root (file-truename buffer-file-name)))))
    (setq dune-process-last-command (list name command cmd))
    (compilation-start cmd 'dune-process-mode (lambda(_) buffer))
    (let ((process (get-buffer-process buffer)))
      (set-process-sentinel process 'dune-process--finished-sentinel)
      process)))

(defun dune-process--maybe-read-command (default)
  "Prompt to modify the DEFAULT command when the prefix argument is present.
Without the prefix argument, return DEFAULT immediately."
  (if current-prefix-arg
      (read-shell-command "Dune command: " default)
    default))

(defun dune-process--augment-cmd-for-os (opens-external cmd)
  "Augment the dune CMD according to OS. OPENS-EXTERNAL is non-nil
if the CMD is expected to open and external application."
  (if (and opens-external
           (not (member system-type '(windows-nt ms-dos)))
           (executable-find "setsid"))
      (concat "setsid -w " cmd)
    cmd))

;;;###autoload
(defun dune-process-build (name)
  "Run the Dune build command.
With the prefix argument, modify the command's invocation.
Dune: Compile the current project."
  (interactive "sBuild name: ")
  (dune-process--start (concat "Build " name)
                        (concat dune-process--command-build " " name)))

;;;###autoload
(defun dune-process-exec-name (name)
  "Run the dune run <name> command.
With the prefix argument, modify the command's invocation.
Dune: Build and execute a specific binary"
  (interactive "sBinary name: ")
  (dune-process--start (concat "Run " name)
                        (concat dune-process--command-exec " " name)))

;;;###autoload
(defun dune-process-clean ()
  "Run the Dune clean command.
With the prefix argument, modify the command's invocation.
Dune: Remove the target directory."
  (interactive)
  (dune-process--start "Clean" dune-process--command-clean))

;;;###autoload
(defun dune-process-init (name &optional bin)
  "Run the dune init command.
With the prefix argument, modify the command's invocation.
NAME is the name of your application.
If BIN is t then create a binary application, otherwise a library.
Dune: Create a new dune project."
  (interactive "sProject name (authorised characters: 'A'..'Z', 'a'..'z', '_' or '0'..'9'): ")
  (let* ((read-answer-short t)
         (kind (read-answer "Kind of the new dune component "
                              '(("project"      ?p "create a bin and a lib dir")
                                ("library"      ?l "create a lib dir")
                                ("executable"   ?e "create a bin dir")
                                ("test"         ?t "create a test dir")
                                ))
               )
         (command
          (concat dune-process--command-init " " kind " " name))
         (process
          (dune-process--start "Init" command)))
    (set-process-sentinel
     process
     (lambda (process event)
       (let* ((project-root (dune-process--project-root))
              (default-directory (or project-root default-directory)))
         (cond
          ((and dune-process--open-file-after-new
                (or
                 (string= "project" kind)
                 (string= "executable" kind)
                 ))
           (find-file (format "%s/bin/main.ml" name)))

          ((and dune-process--open-file-after-new
                (string= "library" kind))
           (find-file (format "%s/lib/dune" name)))

          ((and dune-process--open-file-after-new
                (string= "test" kind))
           (find-file (format "%s/test/dune" name)))
          ))))))

(define-key dune-process-mode-map (kbd "n") 'forward-button)
(define-key dune-process-mode-map (kbd "p") 'backward-button)

(provide 'dune-process)
;;; dune-process.el ends here
