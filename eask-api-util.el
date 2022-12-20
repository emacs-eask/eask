;;; eask-api-util.el --- Eask API  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

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
;; Utility functions
;;

;;; Code:

(require 'subr-x)

;;; Externals

(declare-function project-root "project" (project))

;;; Core

;;;###autoload
(defun eask-api-setup ()
  "Set up for `eask-api'."
  (when-let* ((root (if (fboundp #'project-root)
                        (ignore-errors (project-root (project-current)))
                      (cdr (project-current))))
              ;; Just Eask is not allowed!
              (eask-files (directory-files root t "Easkfile[.0-9]*\\'"))
              (eask-files (cl-remove-if #'file-directory-p eask-files)))
    (require 'eask-api)))

(provide 'eask-api-util)
;;; eask-api.el ends here
