;;; eask.el --- Core Eask APIs, for Eask CLI development  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-eask/eask
;; Version: 0.10.2
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
;; This package provides everything you need for Eask CLI development.
;;
;;   - auto-completion
;;   - eldoc
;;   - code navigation
;;   - peek definition
;;
;; Generally, you would not want to call any of these functions or use any of
;; these variables from your Emacs editor environment.  Unless you are extending
;; Eask's core functionalities.
;;
;; (@* "Usage" )
;;
;; Call the following whenever you need the to know Eask's API,
;;
;;   (require 'eask-core)
;;
;; Or enable it when the project is a valid Eask project,
;;
;;   (add-hook 'emacs-lisp-hook #'eask-api-setup)
;;
;; For more information, please visit our repo page
;; https://github.com/emacs-eask/eask-api
;;

;;; Code:

(defgroup eask nil
  "Core Eask APIs, for Eask CLI development."
  :prefix "eask-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-eask/eask"))

(require 'eask-api)

(provide 'eask)
;;; eask.el ends here
