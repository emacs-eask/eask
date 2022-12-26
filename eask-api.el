;;; eask-api.el --- Core Eask APIs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-eask/eask-api
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: lisp eask api

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

(defgroup eask-api nil
  "Eask API."
  :prefix "eask-api-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-eask/eask-api"))

(defcustom eask-api-strict-p t
  "Set to nil if you want to load Eask API whenever it's possible."
  :type 'boolean
  :group 'eask-api)

;;
;;; Externals

(declare-function project-root "project" (project))

;;
;;; Core

(defun eask-api-files (&optional dir)
  "Return a list of Eask files.

If argument DIR is nil, we use `default-directory' instead."
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
  "Set up for `eask-api'."
  (when-let* ((root (if (fboundp #'project-root)
                        (ignore-errors (project-root (project-current)))
                      (cdr (project-current))))
              ((eask-api-files root)))
    (require 'eask-api-core)))

(provide 'eask-api)
;;; eask-api.el ends here
