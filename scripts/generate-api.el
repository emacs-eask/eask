;;; generate-api.el --- Generate source  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst eask-lisp-path "~/lisp/"
  "Eask's lisp directory.")


(defconst in-ci (getenv "GITHUB_WORKSPACE")
  "It's non-nil if the environment is identified as CI.")

(defun locate-project-file (path)
  "Locate the project by enviornment."
  (concat (if in-ci "./" "../") path))

(defun let-kill-line ()
  "Kill current line."
  (goto-char (line-beginning-position)) (kill-line 1))

(with-current-buffer (find-file (locate-project-file "eask-api.el"))
  (erase-buffer)
  (insert-file-contents (locate-project-file "templates/source.el"))

  ;; navigate to insertion point
  (goto-char (point-min))
  (search-forward "{ SOURCE }")
  (let-kill-line)

  (dolist (path (directory-files-recursively eask-lisp-path "[.]el$"))

    (message "path: %s" path)
    )

  (message (buffer-string))

  (save-buffer))

;;; generate-api.el ends here
