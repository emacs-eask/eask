;;; generate-badges-system.el --- Generate SVG badges, system  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(load-file "./scripts/_prepare.el")
(require 'url-handlers)

(defconst svg-url-format
  "https://img.shields.io/badge/Emacs-%s-7F5AB6.svg?logo=gnu%%20emacs&logoColor=white"
  "Format to generate badges.")

(defconst output-dir "./etc/badges/"
  "Where the badges store.")

(ignore-errors (make-directory output-dir t))

;; Emacs
(let* ((url (format svg-url-format emacs-version))
       (file (concat output-dir "emacs.svg")))
  (message "Download SVG from `%s` to `%s`" url file)
  (url-copy-file url file t))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; generate-badges-system.el ends here
