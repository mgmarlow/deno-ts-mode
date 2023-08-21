;;; deno-ts-mode.el --- Useful extensions when working with Deno  -*- lexical-binding: t; -*-

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

;; 

;;; Code:

(require 'eieio)
(require 'eglot)
(require 'project)

(defgroup deno-ts-mode nil
  "Major mode for Deno."
  :link '(url-link "https://git.sr.ht/~mgmarlow/deno-ts-mode")
  :group 'languages)

(defcustom deno-bin "deno"
  "Path to deno executable."
  :type 'string
  :group 'deno)

;; https://deno.land/manual@v1.36.1/getting_started/setup_your_environment#eglot
(defclass deno-eglot (eglot-lsp-server) ()
  :documentation "A custom class for deno lsp.")

(cl-defmethod eglot-initialization-options ((server deno-eglot))
  "Passes through required deno initialization options"
  (list :enable t
        :lint t))

(defun deno-project-p ()
  "Predicate for determining if the open project is a Deno one."
  (when-let* ((project (project-current))
              (p-root (project-root project)))
    (file-exists-p (concat p-root "deno.json"))))

(defun deno--eglot-server-program (&rest _)
  "Decide which server to use based on project characteristics."
  (cond ((deno-project-p) '(deno-eglot "deno" "lsp"))
        (t '("typescript-language-server" "--stdio"))))

(defun deno-setup-eglot ()
  "Add symbol `deno-eglot' to Eglot's server programs."
  (add-to-list 'eglot-server-programs
               '(deno-ts-mode . deno--eglot-server-program)))

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
  typescript-ts-mode "Deno-tree-sitter"
  "Major mode for Deno."
  :group 'deno-ts-mode)

(defun deno--alist-mode ()
  (if (deno-project-p)
      (deno-ts-mode)
    (typescript-ts-mode)))

(defun deno-setup-auto-mode-alist ()
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . deno--alist-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . deno--alist-mode)))

(provide 'deno-ts-mode)
;;; deno-ts-mode.el ends here
