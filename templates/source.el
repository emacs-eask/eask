;;; eask-core.el --- Core Eask APIs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Shen, Jen-Chieh

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
;; Core Eask APIs
;;
;; This file is generated from a template file, see templates/source.el file.
;;

;;; Code:

;;
;; (@* "Externals" )
;;
(defvar ansi-inhibit-ansi)
(declare-function ansi-bright-black "ext:ansi.el")
(declare-function ansi-underscore "ext:ansi.el")
(declare-function ansi-red "ext:ansi.el")
(declare-function ansi-blue "ext:ansi.el")
(declare-function ansi-green "ext:ansi.el")
(declare-function ansi-white "ext:ansi.el")
(declare-function ansi-yellow "ext:ansi.el")
(declare-function ansi--substitute "ext:ansi.el")
(defvar github-elpa-working-dir)
(defvar github-elpa-archive-dir)
(defvar github-elpa-recipes-dir)
(declare-function github-elpa-build "ext:github-elpa.el")
(defvar license-templates--data)
(declare-function license-templates-keys "ext:license-templates.el")
(defvar checkdoc-version)
(defvar checkdoc-create-error-function)
(defvar check-declare-warning-buffer)
(defvar finder-known-keywords)
(declare-function --each "ext:dash.el")
(defvar package-build-default-files-spec)
(declare-function package-directory-recipe "ext:package-recipe.el")
(declare-function package-build-expand-files-spec "ext:package-build.el")
(declare-function checkdoc-buffer-label "ext:checkdoc.el")
(declare-function package-lint-current-buffer "ext:package-lint.el")
(defvar elsa-global-state)
(declare-function elsa-analyse-file "ext:elsa.el")
(declare-function elsa-message-format "ext:elsa.el")
(declare-function elisp-lint-file "ext:elisp-lint.el")
(declare-function elint-get-log-buffer "ext:elint.el")
(declare-function relint-buffer "ext:relint.el")
(declare-function gitignore-templates-names "ext:gitignore-templates.el")

;;
;; (@* "Source" )
;;
{ SOURCE }

(provide 'eask-core)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; eask-core.el ends here
