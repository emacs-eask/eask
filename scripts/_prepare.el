;;; _prepare.el --- Prepration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'thingatpt)
(require 'json)
(require 'subr-x)

(defconst in-ci (getenv "GITHUB_WORKSPACE")
  "It's non-nil if the environment is identified as CI.")

;;
;;; Util

(defun locate-project-file (path)
  "Locate the project by enviornment."
  (concat (if in-ci "./" "../") path))

(defun s-count-matches (regexp s &optional start end)
  "..."
  (declare (side-effect-free t))
  (save-match-data
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (count-matches regexp (or start 1) (or end (point-max))))))

(defun let-kill-line ()
  "Kill current line."
  (goto-char (line-beginning-position)) (kill-line 1))

(defun let-kill-scope ()
  "Kill a scope."
  (interactive)
  (let ((beg (point)))
    (forward-sexp)
    (kill-region beg (point))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; _prepare.el ends here
