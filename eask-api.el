;;; eask-api.el --- Core Eask APIs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Shen, Jen-Chieh

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is the core APIs of the Eask CLI, a tool for building and testing
;; Emacs Lisp packages
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom eask-api-strict-p t
  "Set to nil if you want to load Eask API whenever it's possible."
  :type 'boolean
  :group 'eask)

(defcustom eask-api-executable nil
  "Executable to eask-cli."
  :type 'string
  :group 'eask)

;;
;;; Externals

(declare-function project-root "project" (project))

;;
;;; Compat

(defun eask-api-project-root ()
  "Return project root."
  (if (fboundp #'project-root)
      (ignore-errors (project-root (project-current)))
    (cdr (project-current))))

;;
;;; Executable

(defun eask-api-executable ()
  "Return Eask CLI path."
  (or eask-api-executable (executable-find "eask")))

(defun eask-api-executable-p ()
  "Return t if Eask CLI is executed from executable and not shell script."
  (not
   (string= "bin"
            (file-name-nondirectory
             (directory-file-name (file-name-directory (eask-api-executable)))))))

(defun eask-api-lisp-root ()
  "Return Eask CLI `lisp' path."
  (file-name-as-directory
   (expand-file-name "lisp"
                     (if (eask-api-executable-p)
                         (eask-api-executable)
                       (expand-file-name "../../" (eask-api-executable))))))

;;
;;; Entry

(defun eask-api-check-filename (name)
  "Return non-nil if NAME is a valid Eask-file."
  (when-let* ((name (file-name-nondirectory (directory-file-name name)))
              (prefix (cond ((string-prefix-p "Easkfile" name) "Easkfile")
                            ((string-prefix-p "Eask" name)     "Eask"))))
    (let ((suffix (car (split-string name prefix t))))
      (or (null suffix)
          (string-match-p "^[.][.0-9]*$" suffix)))))

(defun eask-api-files (&optional dir)
  "Return a list of Eask files.

If argument DIR is nil, we use `default-directory' instead.

This is a simpliy version of `eask--all-files' function."
  (setq dir (or dir default-directory))
  (let ((files (or
                ;; Easkfile is common file for Eask development!
                (directory-files dir t "Easkfile[.0-9]*\\'")
                ;; Allow regular Eask project!?
                (unless eask-api-strict-p
                  (directory-files dir t "Eask[.0-9]*\\'")))))
    (cl-remove-if #'file-directory-p files)))

;;;###autoload
(defun eask-api-setup ()
  "Set up for `eask-api'.

Since this is the entry, we only check for the project root and current
workspace.  The full version uses function `locate-dominating-file' to search
for Eask-files, but we don't want to do that for our entry."
  (let* ((proj-root (eask-api-project-root))
         (e-default (eask-api-files default-directory))
         (e-project (unless e-default (eask-api-files proj-root)))
         (root (if e-default default-directory proj-root))
         (files (or e-default e-project)))
    (when files
      (require 'eask-core)
      (list :root root :files files))))

(provide 'eask-api)
;;; eask-api.el ends here
