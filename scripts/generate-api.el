;;; generate-api.el --- Generate source  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst eask-lisp-path "~/lisp/"
  "Eask's lisp directory.")

(dolist (path (directory-files-recursively eask-lisp-path "[.]el$"))
  (message "path: %s" path))

;;; generate-api.el ends here
