;;; deno-ts-mode.el --- Major mode for Deno  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Graham Marlow

;; Author: Graham Marlow <info@mgmarlow.com>
;; Keywords: languages
;; URL: https://git.sr.ht/~mgmarlow/deno-ts-mode
;; Version: 0.3.0
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
;; This package helps solve some of the problems that arise from Deno
;; and TypeScript sharing the same file extension.  If a Deno
;; configuration file is found at project root, `deno-ts-mode' takes
;; precedence over `typescript-ts-mode'.  Both ".ts" and ".tsx" file
;; extensions are supported.
;;
;; Example configuration:
;; 
;; (use-package deno-ts-mode
;;   :ensure t)
;;
;; (use-package eglot
;;   :ensure t
;;   :hook ((deno-ts-mode . eglot-ensure)
;;          (deno-tsx-ts-mode . eglot-ensure)))

;;; Code:

(require 'eglot)
(require 'project)
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

(defun deno-ts-project-config-path ()
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
  (and (deno-ts-project-config-path) t))

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
  (when-let ((p-config-path (deno-ts-project-config-path)))
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

;;;###autoload
(define-derived-mode deno-ts-mode
  typescript-ts-mode "Deno"
  "Major mode for Deno."
  :group 'deno-ts-mode)

;;;###autoload
(define-derived-mode deno-tsx-ts-mode
  tsx-ts-mode "Deno[TSX]"
  "Major mode for TSX and JSX with Deno."
  :group 'deno-ts-mode)

;;;###autoload
(defun deno-ts-mode-maybe ()
  "Maybe activate `deno-ts-mode'.

If the current file belongs to a Deno project, `deno-ts-mode' is
activated.  Otherwise, fallback to either `typescript-ts-mode' or
`tsx-ts-mode' depending on the visited file extension."
  (if-let* ((filename (buffer-file-name))
            (ext (file-name-extension filename)))
      (cond ((and (equal "ts" ext) (deno-ts-project-p))
             (deno-ts-mode))
            ((equal "ts" ext)
             (typescript-ts-mode))
            ((and (equal "tsx" ext) (deno-ts-project-p))
             (deno-tsx-ts-mode))
            ((equal "tsx" ext)
             (tsx-ts-mode)))))

;; `deno-ts-mode' is competing with `typescript-ts-mode' for control
;; over the ".ts" and ".tsx" file extensions.  Because `deno-ts-mode'
;; derives `typescript-ts-mode', the `auto-mode-alist' entries from
;; `typescript-ts-mode' are added after the ones from this library,
;; shadowing them.  To work around this, this library manually removes
;; the `typescript-ts-mode' and `tsx-ts-mode' entries from
;; `auto-mode-alist'.
(with-eval-after-load 'typescript-ts-mode
  (setq auto-mode-alist (seq-filter
                         (lambda (x)
                           (not (or (equal (cdr x) 'typescript-ts-mode)
                                    (equal (cdr x) 'tsx-ts-mode))))
                         auto-mode-alist)))

;; Register eglot server program for TS and TSX.
(dolist (mode '(deno-ts-mode deno-tsx-ts-mode))
    (add-to-list 'eglot-server-programs
                 (cons mode '("deno" "lsp" :initializationOptions
                              (:enable t :lint t :unstable t)))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ts\\'" . deno-ts-mode-maybe))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . deno-ts-mode-maybe))

;; Required for Deno's color output.
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;;;###autoload
(with-eval-after-load 'project
  (dolist (file '("deno.json" "deno.jsonc"))
    (add-to-list 'project-vc-extra-root-markers file)))

(provide 'deno-ts-mode)
;;; deno-ts-mode.el ends here
