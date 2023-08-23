;;; generate-source.el --- Generate source  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load-file "./scripts/_prepare.el")

(defconst eask-lisp-path "~/lisp/"
  "Eask's lisp directory.")

;;
;;; Generate it

(with-current-buffer (find-file (locate-project-file "eask-core.el"))
  (erase-buffer)
  (insert-file-contents (locate-project-file "templates/source.el"))

  ;; navigate to insertion point
  (goto-char (point-min))
  (search-forward "{ SOURCE }")
  (let-kill-line)

  (let* ((files (directory-files-recursively eask-lisp-path "[.]el$"))
         ;; Sort `_prepare.el' to the top!
         (files (sort files
                      (lambda (file1 &rest _)
                        (string-prefix-p "_" (file-name-nondirectory file1)))))
         ;; Exclude `extern' directory!
         (files (cl-remove-if (lambda (file1)
                                (string-match-p "/lisp/extern/" file1))
                              files)))
    (dolist (path files)
      (message "Processing file %s..." path)
      (insert "\n")
      (insert ";; " path)
      (insert "\n")
      (insert
       (with-temp-buffer
         (insert-file-contents path)
         (goto-char (point-min))
         (let ((forward-str ""))
           (while (not (eobp))
             (let ((line (thing-at-point 'line))
                   (start (point)))
               (cond ((or (string-prefix-p ";;" line)
                          (string-empty-p (string-trim line)))
                      (let-kill-line))
                     ;; skip these
                     ((or (string-prefix-p "(defclass " line)
                          (string-prefix-p "(defun " line)
                          (string-prefix-p "(defconst " line))
                      (forward-sexp)
                      (forward-line 1)
                      (beginning-of-line))
                     ;; Move macro to the top, avoid macro defined too late error.
                     ((or (string-prefix-p "(defvar " line)
                          (string-prefix-p "(declare-function " line)
                          (string-prefix-p "(defcustom " line)
                          (string-prefix-p "(defmacro " line))
                      (forward-sexp)
                      (setq forward-str (concat forward-str
                                                (buffer-substring start (1+ (point)))))
                      (delete-region start (1+ (point)))
                      (beginning-of-line))
                     ;; Add require to avoid compile warnings.
                     ((or (string-prefix-p "(require " line))
                      (forward-sexp)
                      (when-let ((params-count (s-count-matches " " (buffer-substring start (point))))
                                 (_ (= params-count 1)))
                        (forward-char -1)
                        (insert " nil t"))  ; Add no-error flag
                      (forward-line 1)
                      (beginning-of-line))
                     ;; other scope, we kill the entire scope
                     ((string-prefix-p "(" line) (let-kill-scope))
                     ;; kill a line for everything else
                     (t (let-kill-line)))))
           (goto-char (point-min))
           (insert forward-str))
         (buffer-string)))))

  (save-buffer))

;;; generate-source.el ends here
