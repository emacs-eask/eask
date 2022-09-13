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

;;
;;; Util

(defun let-kill-line ()
  "Kill current line."
  (goto-char (line-beginning-position)) (kill-line 1))

(defun let-kill-scope ()
  "Kill a scope."
  (interactive)
  (let ((beg (point)))
    (forward-sexp)
    (kill-region beg (point))))

;;
;;; Generate it

(with-current-buffer (find-file (locate-project-file "eask-api.el"))
  (erase-buffer)
  (insert-file-contents (locate-project-file "templates/source.el"))

  ;; navigate to insertion point
  (goto-char (point-min))
  (search-forward "{ SOURCE }")
  (let-kill-line)

  (dolist (path (directory-files-recursively eask-lisp-path "[.]el$"))
    (message "Processing file %s..." path)
    (insert "\n")
    (insert ";; " path)
    (insert "\n")
    (insert
     (with-temp-buffer
       (insert-file-contents path)
       (goto-char (point-min))
       (while (not (eobp))
         (let ((line (thing-at-point 'line)))
           (cond ((or (string-prefix-p ";;" line)
                      (string-empty-p (string-trim line)))
                  (let-kill-line))
                 ;; skip these
                 ((or (string-prefix-p "(defun " line)
                      (string-prefix-p "(defcustom " line)
                      (string-prefix-p "(defvar " line)
                      (string-prefix-p "(defmacro " line))
                  (forward-sexp)
                  (forward-line 1)
                  (goto-char (line-beginning-position)))
                 ;; other scope, we kill the entire scope
                 ((string-prefix-p "(" line) (let-kill-scope))
                 ;; kill a line for everything else
                 (t (let-kill-line)))))
       (buffer-string))))

  (save-buffer))

;;; generate-api.el ends here
