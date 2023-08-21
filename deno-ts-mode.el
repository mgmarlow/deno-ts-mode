;;; deno-ts-mode.el --- Major mode for Deno  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Graham Marlow

;; Author: Graham Marlow <info@mgmarlow.com>
;; Keywords: languages
;; URL: https://git.sr.ht/~mgmarlow/deno-ts-mode
;; Version: 0.1.0
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

;; Major mode for Deno.  `deno-ts-mode' is derived from
;; `typescript-ts-mode', so it depends on the TypeScript tree-sitter
;; parser and definitions.  Since Deno uses the same file extension as
;; TypeScript, `deno-ts-mode' helps alleviate some of the common
;; issues that come out of swapping between Deno projects and regular
;; TS projects.

;;; Code:

(require 'eglot)
(require 'project)
(require 'typescript-ts-mode) ; Make sure to load auto-mode-alist here first

(defgroup deno nil
  "Major mode for Deno."
  :link '(url-link "https://git.sr.ht/~mgmarlow/deno-ts-mode")
  :group 'languages)

(defcustom deno-bin "deno"
  "Path to deno executable."
  :type 'string
  :group 'deno)

(defun deno-project-p ()
  "Return t if `project-current' is a Deno project."
  (when-let* ((project (project-current))
              (p-root (project-root project)))
    (file-exists-p (concat p-root "deno.json"))))

;; https://deno.land/manual@v1.36.1/getting_started/setup_your_environment#eglot
(defun deno-setup-eglot ()
  "Add `deno-ts-mode' to `eglot-server-programs'."
  (add-to-list 'eglot-server-programs
               '(deno-ts-mode . ("deno" "lsp" :initializationOptions (:enable t :lint t)))))

;; TODO
;; (defun deno--project-cmd (format-string)
;;   (unless (deno-project-p)
;;     (error "No Deno project found"))
;;   (let ((default-main (concat (project-root (project-current)) "main.ts")))
;;     (compile (apply #'format
;;                     (concat "%s " format-string " %s")
;;                     (list deno-bin default-main)))))

;; (defun deno-compile ()
;;   (interactive)
;;   (deno--project-cmd "compile"))

;; (defun deno-run ()
;;   (interactive)
;;   (deno--project-cmd "run"))

;; This is executed in a directory, does not take a file param:
;; (defun deno-test ()
;;   (interactive)
;;   (deno--project-cmd "test"))

(define-derived-mode deno-ts-mode
  typescript-ts-mode "Deno"
  "Major mode for Deno."
  :group 'deno-ts-mode)

(defun deno--auto-mode ()
  "Return `deno-ts-mode' if project is a Deno project, else `typescript-ts-mode'."
  (cond ((deno-project-p) (deno-ts-mode))
        (t (typescript-ts-mode))))

(defun deno-setup-auto-mode-alist ()
  "Add Deno to `auto-mode-alist' for .ts and .tsx files.

If the visited .ts file does not detect a Deno project (as
determined by `deno-project-p') this function will fallback to
`typescript-ts-mode'."
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . deno--auto-mode)))

(provide 'deno-ts-mode)
;;; deno-ts-mode.el ends here
