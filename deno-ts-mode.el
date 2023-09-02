;;; deno-ts-mode.el --- Major mode for Deno  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Graham Marlow

;; Author: Graham Marlow <info@mgmarlow.com>
;; Keywords: languages
;; URL: https://git.sr.ht/~mgmarlow/deno-ts-mode
;; Version: 0.1.1
;; Package-Requires: ((emacs "29.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for Deno.
;;
;; `deno-ts-mode' is derived from `typescript-ts-mode' so it depends
;; on the same TypeScript tree-sitter parsers.  Install both the TSX
;; and TypeScript parsers for full deno syntax support (see README for
;; full details).
;;
;; This package helps solve some of the problems that arise from deno
;; and TypeScript sharing the same file extension.  With
;; `deno-ts-setup-auto-mode-alist', `deno-ts-mode' will check for the
;; presence of a Deno config file when a major mode is selected for a
;; ".ts" or ".tsx" file.  When the config file is located,
;; `deno-ts-mode' is selected.  Otherwise, `typescript-ts-mode' and
;; `tsx-ts-mode' are selected as fallbacks.  This function is
;; optional, so you can determine your auto-mode bindings however you
;; wish.
;;
;; Example configuration:
;; 
;; (use-package deno-ts-mode
;;   :config
;;   (deno-ts-setup-auto-mode-alist))
;;
;; (use-package eglot
;;   :ensure t
;;   :hook ((deno-ts-mode . eglot-ensure))
;;   :config
;;   (deno-ts-setup-eglot))

;;; Code:

(require 'eglot)
(require 'project)
(require 'typescript-ts-mode) ; Make sure to load auto-mode-alist here first
(require 'json)
(require 'cl-lib)

(defgroup deno-ts nil
  "Major mode for Deno."
  :link '(url-link "https://git.sr.ht/~mgmarlow/deno-ts-mode")
  :group 'languages)

(defcustom deno-ts-bin "deno"
  "Path to deno executable."
  :type 'string
  :group 'deno)

(defun deno-ts--project-config-path ()
  "Return the filepath of the current project's deno config.

Return nil if `project-current' is nil or if a deno config file
cannot be found."
  (when-let* ((project (project-current))
              (p-root (project-root project))
              (possible-filepaths
               (mapcar (lambda (filepath) (concat p-root filepath))
                       '("deno.json" "deno.jsonc"))))
    (seq-find 'file-exists-p possible-filepaths)))

(defun deno-ts-project-p ()
  "Return t if `project-current' is a Deno project.

Return nil if `project-current' is not a Deno project or the
current project cannot be read."
  (and (deno-ts--project-config-path) t))

;; https://deno.land/manual@v1.36.1/getting_started/setup_your_environment#eglot
(defun deno-ts-setup-eglot ()
  "Add `deno-ts-mode' to `eglot-server-programs'."
  (add-to-list 'eglot-server-programs
               '(deno-ts-mode . ("deno" "lsp" :initializationOptions (:enable t :lint t)))))

(cl-defstruct deno-ts-config
  "Deno configuration file struct."
  tasks lint fmt test)

(defun deno-ts--build-config (json)
  "Return an instance of `deno-ts-config' from JSON string."
  (let ((raw (json-read-from-string json)))
    (make-deno-ts-config :tasks (alist-get 'tasks raw)
                         :lint (alist-get 'lint raw)
                         :fmt (alist-get 'fmt raw)
                         :test (alist-get 'test raw))))

(defun deno-ts-project-config ()
  "Return an instance of `deno-ts-config' built from the current project.

Return nil if no project is found."
  (when-let ((p-config-path (deno-ts--project-config-path)))
    (deno-ts--build-config
     (with-temp-buffer
       (insert-file-contents p-config-path)
       (buffer-string)))))

(defun deno-ts-run-task ()
  "Execute a deno task from the current project's deno config."
  (interactive)
  (if-let* ((config (deno-ts-project-config))
            (tasks (mapcar 'car (deno-ts-config-tasks config))))
      (compile (format "deno task %s"
                       (completing-read "Run task: " tasks nil t)))
    (error "Cannot find Deno configuration file")))

(define-derived-mode deno-ts-mode
  typescript-ts-mode "Deno"
  "Major mode for Deno."
  :group 'deno-ts-mode)

(defun deno-ts--ts-auto-mode ()
  "Return `deno-ts-mode' if project is a Deno project, else `typescript-ts-mode'."
  (cond ((deno-ts-project-p) (deno-ts-mode))
        (t (typescript-ts-mode))))

(defun deno-ts--tsx-auto-mode ()
  "Return `deno-ts-mode' if project is a Deno project, else `tsx-ts-mode'."
  (cond ((deno-ts-project-p) (deno-ts-mode))
        (t (tsx-ts-mode))))

(defun deno-ts-setup-auto-mode-alist ()
  "Add Deno to `auto-mode-alist' for .ts and .tsx files.

If the visited .ts file does not detect a Deno project (as
determined by `deno-project-p') this function will fallback to
`typescript-ts-mode'."
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . deno-ts--ts-auto-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . deno-ts--tsx-auto-mode)))

;; Required for Deno's color output.
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;;;###autoload
(with-eval-after-load 'project
  (dolist (file '("deno.json" "deno.jsonc"))
    (add-to-list 'project-vc-extra-root-markers file)))

(provide 'deno-ts-mode)
;;; deno-ts-mode.el ends here
