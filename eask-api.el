;;; eask-api.el --- Eask API  -*- lexical-binding: t; -*-

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
;; Eask API
;;

;;; Code:


;; ~/lisp/checker/check-eask.el

;;; Commentary:
;;
;; Commmand use to run Eask checker
;;
;;   $ eask check-eask
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defvar eask--checker-log nil)

(defun eask--load-buffer ()
  "Return the current loading file session."
  (car (cl-remove-if-not
        (lambda (elm) (string-prefix-p " *load*-" (buffer-name elm))) (buffer-list))))

(defun eask--write-log (level msg)
  "Write the log."
  (unless (string= " *temp*" (buffer-name))  ; avoid error from `package-file' directive
    (with-current-buffer (or (eask--load-buffer) (buffer-name))
      (let* ((level-string (cl-case level
                             (`error "Error")
                             (`warn  "Warning")))
             (log (format "%s:%s:%s %s: %s"
                          (file-name-nondirectory (or load-file-name eask-file))
                          (if load-file-name (line-number-at-pos) 0)
                          (if load-file-name (current-column) 0)
                          level-string
                          msg)))
        (push (ansi-color-filter-apply log) eask--checker-log)))))

(add-hook 'eask-on-error-hook #'eask--write-log)
(add-hook 'eask-on-warning-hook #'eask--write-log)

(eask-start
  (if eask--checker-log
      (mapc #'eask-msg (reverse eask--checker-log))
    (eask-msg "(No issues found)")))

;;; check-eask.el ends here

;; ~/lisp/core/activate.el

;;; Commentary:
;;
;; Command use to activate package
;;
;;   $ eask activate
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (eask-pkg-init)
  (let ((name (eask-guess-package-name)))
    (eask-with-progress
      (format "Activating the package `%s'... " (ansi-green name))
      (require (intern name))
      "succeeded ✓"))
  (eask-call "core/load"))

;;; activate.el ends here

;; ~/lisp/core/archives.el

;;; Commentary:
;;
;; Command use to list out all package archives,
;;
;;   $ eask archives
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defvar eask--length-name)
(defvar eask--length-url)
(defvar eask--length-priority)

(defun eask--print-archive (archive)
  "Print the archive."
  (let* ((name (car archive))
         (url (cdr archive))
         (priority (assoc name package-archive-priorities))
         (priority (cdr priority)))
    (message (concat "  %-" eask--length-name "s  %-" eask--length-url "s  %-" eask--length-priority "s")
             name url (or priority 0))))

(eask-start
  (if package-archives
      (progn
        (let* ((names (mapcar #'car package-archives))
               (eask--length-name (format "%s" (eask-seq-str-max names)))
               (urls (mapcar #'cdr package-archives))
               (eask--length-url (format "%s" (eask-seq-str-max urls)))
               (priorities (mapcar #'cdr package-archive-priorities))
               (eask--length-priority (format "%s" (eask-seq-str-max priorities))))
          (mapc #'eask--print-archive package-archives))
        (eask-info "(Total of %s archives)" (length package-archives)))
    (eask-info "(No archive has been selected)")))

;;; archives.el ends here

;; ~/lisp/core/autoloads.el

;;; Commentary:
;;
;; Command use generate autoload file,
;;
;;   $ eask autoloads
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (let* ((name (eask-guess-package-name))
         (autoloads-file (expand-file-name (concat name "-autoloads.el"))))
    (package-generate-autoloads name default-directory)
    (eask-info "Write file %s..." autoloads-file)))

;;; autoloads.el ends here

;; ~/lisp/core/clean-all.el

;;; Commentary:
;;
;; Command that cleans .eask directory, and all elc files.
;;
;;   $ eask clean-all
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (eask-call "core/clean-elc")
  (eask-call "core/clean"))

;;; clean-all.el ends here

;; ~/lisp/core/clean-elc.el

;;; Commentary:
;;
;; Remove byte compiled files generated by cask build
;;
;;   $ eask clean-elc
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--delete-file (filename)
  "Delete FILENAME from disk."
  (eask-with-progress
    (format "Deleting %s... " filename)
    (eask-with-verbosity 'log
      (ignore-errors (delete-file filename)))
    "done ✓"))

(eask-start
  (if-let ((files (eask-package-elc-files)))
      (progn
        (mapc #'eask--delete-file files)
        (eask-info "✓ (Total of %s .elc file%s deleted)" (length files)
                   (eask--sinr files "" "s")))
    (eask-info "(No .elc file found in workspace)")))

;;; clean-elc.el ends here

;; ~/lisp/core/clean.el

;;; Commentary:
;;
;; Command use to clean up `.eask' in the working directory,
;;
;;   $ eask clean [-g]
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (let ((target-dir
         (if (eask-global-p) user-emacs-directory
           (file-name-directory (directory-file-name user-emacs-directory)))))
    (ignore-errors (delete-directory target-dir t))
    (if eask--first-init-p
        (eask-info "(Workspace already cleaned)")
      (eask-info "✓ Done (workspace `%s` is cleaned)" target-dir))))

;;; clean.el ends here

;; ~/lisp/core/compile.el

;;; Commentary:
;;
;; Byte compile all Emacs Lisp files in the package
;;
;;   $ eask compile [names..]
;;
;;
;;  Initialization options:
;;
;;    [names..]     specify files to byte-compile
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

;; Handle options
(add-hook 'eask-before-command-hook
          (lambda ()
            (when (eask-strict-p) (setq byte-compile-error-on-warn t))
            (when (= eask-verbosity 4) (setq byte-compile-verbose t))))

(defconst eask-compile-log-buffer-name "*Compile-Log*"
  "Byte-compile log buffer name.")

(defun eask--print-compile-log ()
  "Print `*Compile-Log*' buffer."
  (when (get-buffer eask-compile-log-buffer-name)
    (with-current-buffer eask-compile-log-buffer-name
      (eask-print-log-buffer)
      (message ""))))

(defun eask--byte-compile-file (filename)
  "Byte compile FILENAME."
  ;; *Compile-Log* does not kill itself. Make sure it's clean before we do
  ;; next byte-compile task.
  (ignore-errors (kill-buffer eask-compile-log-buffer-name))
  (let* ((filename (expand-file-name filename))
         (result))
    (eask-with-progress
      (unless byte-compile-verbose (format "Compiling %s... " filename))
      (eask-with-verbosity 'debug
        (setq result (byte-compile-file filename)
              result (eq result t)))
      (if result "done ✓" "skipped ✗"))
    (eask--print-compile-log)
    result))

(defun eask--compile-files (files)
  "Compile sequence of FILES."
  (let* ((compiled (cl-remove-if-not #'eask--byte-compile-file files))
         (compiled (length compiled))
         (skipped (- (length files) compiled)))
    (eask-info "(Total of %s file%s compiled, %s skipped)" compiled
               (eask--sinr compiled "" "s")
               skipped)))

(eask-start
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (if-let ((files (or (eask-expand-file-specs (eask-args))
                      (eask-package-el-files))))
      (eask--compile-files files)
    (eask-info "(No files have been compiled)")
    (eask-help 'compile)))

;;; compile.el ends here

;; ~/lisp/core/concat.el

;;; Commentary:
;;
;; Byte compile all Emacs Lisp files in the package
;;
;;   $ eask concat [names..]
;;
;;
;;  Initialization options:
;;
;;    [names..]         specify files to concatenate
;;
;;  Action options:
;;
;;    [destination]     optional output destination
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (let* ((name (eask-guess-package-name))
         (files (or (eask-expand-file-specs (eask-args))
                    (eask-package-el-files)))
         (eask-dist-path (or (eask-dest) eask-dist-path))
         (eask-dist-path (expand-file-name eask-dist-path))
         (target-file (concat name ".built.el"))
         (target-filename (expand-file-name target-file eask-dist-path)))
    (eask-debug "Destination path in %s" eask-dist-path)
    (ignore-errors (make-directory eask-dist-path t))

    (eask-info "Prepare to concatenate files %s..." target-filename)
    (write-region "" nil target-filename)

    (eask-with-verbosity 'log
      (with-temp-buffer
        (eask-progress-seq "  - Visiting" files "appended! ✓" #'insert-file-contents)
        (write-region (buffer-string) nil target-filename)
        (eask-info "Done. (Wrote file in %s)" target-filename)))))

;;; concat.el ends here

;; ~/lisp/core/create.el

;;; Commentary:
;;
;; Create a new elisp project,
;;
;;   $ eask create [name]
;;
;;
;;  Initialization options:
;;
;;    [name]     new project name
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defconst eask--template-project-name "template-elisp"
  "Holds template project name.")

(defun eask--replace-string-in-buffer (old new)
  "Replace OLD to NEW in buffer."
  (let ((str (buffer-string)))
    (setq str (s-replace old new str))
    (delete-region (point-min) (point-max))
    (insert str)))

;; XXX: we can't use `user-full-name' in batch-mode. It will always return
;; empty string.
(defun eask--get-user ()
  "Return user name."
  (string-trim (shell-command-to-string "git config user.name")))

;; XXX: we can't use `user-mail-address' in batch-mode. It will always return
;; empty string.
(defun eask--get-mail ()
  "Return user email."
  (string-trim (shell-command-to-string "git config user.email")))

(eask-start
  (ignore-errors (delete-directory ".git" t))
  (eask-with-progress
    "Preparing your new elisp project... "
    (let ((template-package-file (expand-file-name (concat eask--template-project-name ".el"))))
      (rename-file template-package-file eask-package-file)
      (with-current-buffer (find-file eask-package-file)
        (eask--replace-string-in-buffer eask--template-project-name (eask-package-name))
        (eask--replace-string-in-buffer "{ SUMMARY }" (eask-package-description))
        (eask--replace-string-in-buffer "{ YEAR }" (format-time-string "%Y"))
        (eask--replace-string-in-buffer "{ FULL_NAME }" (eask--get-user))
        (eask--replace-string-in-buffer "{ MAIL_ADDR }" (eask--get-mail))
        (eask--replace-string-in-buffer "{ WEBSITE_URL }" (or eask-website-url ""))
        (eask--replace-string-in-buffer "{ VERSION }" (eask-package-version))
        (eask--replace-string-in-buffer "{ EMACS_VERSION }" (eask-depends-emacs-version))
        (eask--replace-string-in-buffer "{ KEYWORDS }" (string-join eask-keywords " "))
        (save-buffer))
      (with-current-buffer (find-file (expand-file-name "README.md"))
        (eask--replace-string-in-buffer eask--template-project-name (eask-package-name))
        (eask--replace-string-in-buffer "{ SUMMARY }" (eask-package-description))
        (save-buffer)))
    "done ✓")
  (eask-msg "")
  (eask-msg "Congratulations! Your new Elisp project is created in %s" eask-file-root)
  (eask-msg "")
  (eask-msg "  [1] Navigate to %s" eask-file-root)
  (eask-msg "  [2] Try out the command `eask info`")
  (eask-msg "")
  (eask-msg "Visit https://emacs-eask.github.io/ for quickstart guide and full documentation."))

;;; create.el ends here

;; ~/lisp/core/emacs.el

;;; Commentary:
;;
;; Execute emacs with the appropriate environment
;;
;;   $ eask emacs [args..]
;;
;;
;;  Initialization options:
;;
;;    [args..]     arguments feed into Emacs executable
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-l" command-line-args))))
      nil t)

;;
;;; Below is copied from `eask-start' macro

(eask--handle-global-options)
(eask--print-env-info)

(setq user-emacs-directory (expand-file-name (concat ".eask/" emacs-version "/"))
      package-user-dir (expand-file-name "elpa" user-emacs-directory)
      user-init-file (locate-user-emacs-file "init.el")
      custom-file (locate-user-emacs-file "custom.el"))

(package-activate-all)
(ignore-errors (make-directory package-user-dir t))
(eask--silent (eask-setup-paths))

;;; emacs.el ends here

;; ~/lisp/core/eval.el

;;; Commentary:
;;
;; Evaluate lisp form with a proper PATH,
;;
;;   $ eask eval [form]
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (eask-pkg-init)
  (let ((form (eask-argv 0)))
    (with-temp-buffer (insert form) (eval-buffer))))

;;; eval.el ends here

;; ~/lisp/core/exec-path.el

;;; Commentary:
;;
;; Print the PATH (exec-path) from workspace
;;
;;   $ eask path
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--print-exec-path (path)
  "Print out the PATH."
  (message "%s" path))

(eask-start
  (eask-pkg-init)
  (mapc #'eask--print-exec-path exec-path)
  (eask-info "(Total of %s exec-path)" (length exec-path)))

;;; exec-path.el ends here

;; ~/lisp/core/exec.el

;;; Commentary:
;;
;; Execute command with correct load-path set up
;;
;;   $ eask exec [args..]
;;
;;
;;  Initialization options:
;;
;;    [args..]     execute command with correct load-path set up
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defconst eask--homedir (getenv "EASK_HOMEDIR")  ; temporary environment from node
  "Eask temporary storage.")

(defun eask--export-env ()
  "Export environments."
  (let ((epf (expand-file-name "exec-path" eask--homedir))
        (lpf (expand-file-name "load-path" eask--homedir)))
    (ignore-errors (make-directory eask--homedir t))  ; generate dir ~/.eask/
    (write-region (getenv "PATH") nil epf)
    (write-region (getenv "EMACSLOADPATH") nil lpf)))

(eask-start
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  ;; XXX This is the hack by adding all `bin' folders from local elpa.
  (eask-setup-paths)
  (ignore-errors (delete-directory eask--homedir t))  ; clean up
  (if-let ((name (eask-argv 1)))
      (or
       ;; 1) For Elisp executable (github-elpa)
       (let ((program (executable-find name)))
         (setq commander-args (cddr argv))  ; by pass `--' as well
         (ignore-errors (load program nil t)))
       ;; 2) Export environments, and return back to node for subcommand execution
       (eask-with-progress
         (ansi-green "Exporting environment variables... ")
         (eask--export-env)
         (ansi-green "done ✓")))
    (eask-info "✗ (No exeuction output)")
    (eask-help 'exec)))

;;; exec.el ends here

;; ~/lisp/core/files.el

;;; Commentary:
;;
;; Print the list of all package files
;;
;;   $ eask files
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--print-filename (filename)
  "Print out the FILENAME."
  (message "%s" filename))

(eask-start
  (let ((files (eask-package-files)))
    (mapc #'eask--print-filename files)
    (eask-info "(Total of %s item%s listed)" (length files) (eask--sinr files "" "s"))))

;;; files.el ends here

;; ~/lisp/core/info.el

;;; Commentary:
;;
;; Display information about the current package
;;
;;   $ eask info
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defvar eask--max-offset 0)

(defun eask--print-deps (title dependencies)
  "Print dependencies."
  (when dependencies
    (eask-msg "")
    (eask-msg title)
    (let* ((names (mapcar #'car dependencies))
           (offset (eask-seq-str-max names)))
      (setq eask--max-offset (max offset eask--max-offset)
            offset (format "%s" eask--max-offset))
      (dolist (dep dependencies)
        (let* ((target-version (cdr dep))
               (target-version (if (= (length target-version) 1)
                                   (nth 0 target-version)
                                 "specified")))
          (eask-msg (concat "  %-" offset "s (%s)") (car dep) target-version)
          (eask-debug "    Recipe: %s" (car dep)))))))

(eask-start
  (if eask-package
      (progn
        (eask-msg "")
        (eask-msg "%s (%s) | deps: %s | devDeps: %s"
                  (ansi-green (eask-package-name))
                  (ansi-yellow (eask-package-version))
                  (ansi-cyan (length eask-depends-on))
                  (ansi-cyan (length eask-depends-on-dev)))
        (eask-msg (eask-package-description))
        (when-let ((url (or (eask-package-desc-url) eask-website-url)))
          (eask-msg (ansi-cyan url)))
        (when-let ((keywords (or (eask-package-desc-keywords) eask-keywords)))
          (eask-msg "")
          (eask-msg "keywords: %s" (string-join keywords ", ")))
        (eask-msg "")
        (when eask-package-file
          (eask-msg "entry: %s" (eask-root-del eask-package-file)))
        (eask-msg "kind: %s" (if (eask-package-multi-p) "tar" "single"))
        (eask-msg "")
        (eask-msg "dist")
        (eask-msg ".total-files: %s" (length (eask-package-files)))
        (eask-msg ".unpacked-size: %s" (eask-unpacked-size))
        (eask--print-deps "dependencies:" eask-depends-on)
        (eask--print-deps "devDependencies:" eask-depends-on-dev))
    (eask-info "(Eask file has no package information)")
    (eask-help 'info)))

;;; info.el ends here

;; ~/lisp/core/install-deps.el

;;; Commentary:
;;
;; Command use to install package dependencies
;;
;;   $ eask install-deps
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (if (eask-dependencies)
      (progn
        (when (and (eask-dev-p) (not eask-depends-on-dev))
          (eask-warn "No development dependencies found in your Eask file; but continue to install package dependencies"))
        (eask-install-dependencies))
    (eask-info "✗ (No dependencies found in your Eask file)")
    (eask-help 'install-deps)))

;;; install-deps.el ends here

;; ~/lisp/core/install.el

;;; Commentary:
;;
;; Command use to install Emacs packages,
;;
;;   $ eask install [names..]
;;
;;
;;  Initialization options:
;;
;;    [names..]     name of the package to install; else we try to install
;;                  package from current directory by calling function
;;                  `package-install-file'
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-load "core/package")  ; load dist path

(defun eask--install-packages (names)
  "Install packages."
  (let* ((names (mapcar #'intern names))
         (len (length names)) (s (eask--sinr len "" "s"))
         (pkg-not-installed (cl-remove-if #'package-installed-p names))
         (installed (length pkg-not-installed)) (skipped (- len installed)))
    (eask-log "Installing %s specified package%s..." len s)
    (mapc #'eask-package-install names)
    (eask-info "(Total of %s package%s installed, %s skipped)"
               installed s skipped)))

(eask-start
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (if-let ((names (eask-args)))
      ;; If package [name..] are specified, we try to install it
      (eask--install-packages names)
    ;; Else we try to install package from the working directory
    (eask-install-dependencies)
    (let* ((name (eask-guess-package-name))
           (packaged (eask-packaged-file))
           (target (or packaged eask-package-file)))
      (eask-log "Searching for artifact to install...")
      (if packaged (eask-info "✓ Found artifact in %s" target)
        (eask-info "? Missing artifact, install directly from %s" target))
      (if target
          (progn
            (add-to-list 'load-path (expand-file-name (eask-packaged-name) package-user-dir))
            (package-install-file target)
            (eask-info "(Installed in %s)"
                       (file-name-directory (locate-library name))))
        (eask-info "✗ (No files have been intalled)")
        (eask-help 'install)))))

;;; install.el ends here

;; ~/lisp/core/keywords.el

;;; Commentary:
;;
;; List available keywords that can be used in the header section,
;;
;;   $ eask keywords
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(require 'finder)

(eask-start
  (let* ((keywords (mapcar #'car finder-known-keywords))
         (offset (eask-seq-str-max keywords))
         (fmt (concat "  %-" (eask-2str offset) "s  %s")))
    (dolist (keyword keywords)
      (eask-msg fmt keyword (cdr (assq keyword finder-known-keywords))))
    (eask-msg "")
    (eask-info "(Total of %s keywords listed)" (length keywords))))

;;; keywords.el ends here

;; ~/lisp/core/list-all.el

;;; Commentary:
;;
;; Command use to list out available Emacs packages from archives,
;;
;;   $ eask list-all
;;
;;
;;  Action options:
;;
;;    [--depth]  dependency level to print
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-load "core/list")

(eask-start
  (eask-pkg-init)
  (let ((pkg-list (reverse (mapcar #'car package-archive-contents))))
    (eask--list pkg-list package-archive-contents))
  (eask-info "(Total of %s package%s available)" (length package-archive-contents)
             (eask--sinr package-archive-contents "" "s")))

;;; list-all.el ends here

;; ~/lisp/core/list.el

;;; Commentary:
;;
;; Command use to list out installed Emacs packages,
;;
;;   $ eask list
;;
;;
;;  Action options:
;;
;;    [--depth]  dependency level to print
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defvar eask--list-pkg-name-offset nil)
(defvar eask--list-pkg-version-offset nil)
(defvar eask--list-pkg-archive-offset nil)

(defun eask--format-s (offset)
  "Format OFFSET."
  (concat " %-" (number-to-string offset) "s "))

(defun eask--align (depth &optional rest)
  "Format string to align starting from the version number."
  (let ((prefix (if (= depth 0) "[+]" "[+]")))
    (concat (spaces-string (* depth 2))  ; indent for depth
            " " prefix
            (eask--format-s (- eask--list-pkg-name-offset (* depth 2)))
            (eask--format-s eask--list-pkg-version-offset)
            (eask--format-s eask--list-pkg-archive-offset)
            rest)))

(defun eask-print-pkg (name depth max-depth pkg-alist)
  "Print NAME package information."
  (when-let*
      ((pkg (assq name pkg-alist))
       (desc (cadr pkg))
       (name (package-desc-name desc))
       (version (package-desc-version desc))
       (version (package-version-join version))
       (archive (or (package-desc-archive desc) ""))
       (summary (package-desc-summary desc)))
    (if (= depth 0)
        (eask-msg (eask--align depth " %-80s") name version archive summary)
      (eask-msg (eask--align depth) name "" "" ""))
    (when-let ((reqs (package-desc-reqs desc))
               ((< depth max-depth)))
      (dolist (req reqs)
        (eask-print-pkg (car req) (1+ depth) max-depth pkg-alist)))))

(defun eask--version-list (pkg-alist)
  "Return list of versions."
  (mapcar (lambda (elm)
            (package-version-join (package-desc-version (cadr elm))))
          pkg-alist))

(defun eask--archive-list (pkg-alist)
  "Return list of archives."
  (mapcar (lambda (elm)
            (or (package-desc-archive (cadr elm)) ""))
          pkg-alist))

(defun eask--list (list pkg-alist &optional depth)
  "List packages."
  (let* ((eask--list-pkg-name-offset (eask-seq-str-max list))
         (version-list (eask--version-list pkg-alist))
         (eask--list-pkg-version-offset (eask-seq-str-max version-list))
         (archive-list (eask--archive-list pkg-alist))
         (eask--list-pkg-archive-offset (eask-seq-str-max archive-list)))
    (dolist (name list)
      (eask-print-pkg name 0 (or depth (eask-depth) 999) pkg-alist))))

(eask-start
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask--list package-activated-list package-alist)
  (eask-info "(Total of %s package%s installed)"
             (length package-activated-list)
             (eask--sinr package-activated-list "" "s")))

;;; list.el ends here

;; ~/lisp/core/load-path.el

;;; Commentary:
;;
;; Print the load-path from workspace
;;
;;   $ eask load-path
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--print-load-path (path)
  "Print out the PATH."
  (message "%s" path))

(eask-start
  (eask-pkg-init)
  (mapc #'eask--print-load-path load-path)
  (eask-info "(Total of %s load-path)" (length load-path)))

;;; load-path.el ends here

;; ~/lisp/core/load.el

;;; Commentary:
;;
;; Command use to load files (accept multiple)
;;
;;   $ eask load
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (if-let ((files (eask-expand-file-specs (eask-args))))
      (mapc #'load-file files)
    (eask-info "(Nothing to load.)")))

;;; load.el ends here

;; ~/lisp/core/outdated.el

;;; Commentary:
;;
;; Command use to show all outdated dependencies,
;;
;;   $ eask outdated
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-load "core/upgrade")
(eask-load "core/list")

(eask-start
  (eask-pkg-init)
  (if-let* ((upgrades (eask-package--upgrades))
            (pkg-list (mapcar #'package-desc-name upgrades)))
      (progn
        (unless (eask-global-p)
          ;; Remove current developing packages
          (setq pkg-list (remove (intern (eask-guess-package-name)) pkg-list)))
        (eask--list pkg-list package-alist 0)
        (eask-info "(Total of %s dependenc%s %s outdated)" (length pkg-list)
                   (eask--sinr pkg-list "y" "ies")
                   (eask--sinr pkg-list "is" "are")))
    (eask-info "(No outdated dependencies)")))

;;; outdated.el ends here

;; ~/lisp/core/package-directory.el

;;; Commentary:
;;
;; Print path to package directory,
;;
;;   $ eask package-directory
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (message "%s" package-user-dir))

;;; package-directory.el ends here

;; ~/lisp/core/package.el

;;; Commentary:
;;
;; Build a package artifact, and put it into the given destination
;;
;;   $ eask package [destination]
;;
;;
;;  Positional options:
;;
;;    [destination]      destination path/folder
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask-package-dir--patterns ()
  "Return patterns for directory recipe."
  (if eask-files
      (if (member eask-package-file (eask-expand-file-specs (eask-files-spec)))
          ;; Else we return default
          eask-files
        ;; If files DSL doesn't contain package main file, we added manually!
        ;;
        ;; This would avoid error, single file doesn't match package name.
        (append eask-files (list eask-package-file)))
    package-build-default-files-spec))

(defun eask-package-dir-recipe ()
  "Form a directory recipe."
  (eask-load "extern/package-recipe")
  (let ((name (eask-guess-package-name))
        (patterns (eask-package-dir--patterns))
        (path default-directory))
    (package-directory-recipe name :name name :files patterns :dir path)))

(defun eask-packaged-name ()
  "Find a possible packaged name."
  (let ((name (eask-guess-package-name))
        (version (eask-package-version)))
    (concat name "-" version)))

(defun eask--packaged-file (ext)
  "Find a possible packaged file."
  (let* ((dist eask-dist-path)
         (file (expand-file-name (concat (eask-packaged-name) "." ext) dist)))
    (and (file-exists-p file) file)))

(defun eask-packaged-file ()
  "Return generated package artifact; it could be a tar or el."
  (if (eask-package-multi-p) (eask--packaged-file "tar")
    (eask--packaged-file "el")))

(eask-start
  (let ((eask-dist-path (or (eask-argv 0) eask-dist-path))
        (eask-dist-path (expand-file-name eask-dist-path))
        (packaged))
    (ignore-errors (make-directory eask-dist-path t))

    (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
    (eask-with-archives "melpa"
      (eask-package-install 'package-build))
    (eask-load "extern/package-build")  ; override

    (let* ((version (eask-package-version))
           (rcp (eask-package-dir-recipe))
           (package-build-working-dir default-directory)
           (package-build-archive-dir eask-dist-path))
      (eask-with-progress
        (format "Building artifact %s (%s)... " (eask-package-name) version)
        (package-build--package rcp version)
        "done ✓"))

    (setq packaged (eask-packaged-file))

    (when (and eask-is-windows (eask-package-single-p))
      (with-current-buffer (find-file packaged)
        (set-buffer-file-coding-system 'utf-8-unix)
        (save-buffer)))

    (eask-info "(Built in %s)" packaged)))

;;; package.el ends here

;; ~/lisp/core/pkg-file.el

;;; Commentary:
;;
;; Command use generate -pkg file,
;;
;;   $ eask pkg-file
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defvar eask--pkg-filename)

(defun eask--generate-from-pkg-desc ()
  "Generate pkg-file from a package-descriptor."
  (let* ((name (package-desc-name eask-package-desc))
         (pkg-file (expand-file-name (format "%s-pkg.el" name))))
    (setq eask--pkg-filename pkg-file)
    (package-generate-description-file eask-package-desc pkg-file)))

(defun eask--generate-from-eask-file ()
  "Generate pkg-file from Eask file."
  (let* ((name (eask-guess-package-name))
         (pkg-file (expand-file-name (concat name "-pkg.el")))
         (version (eask-package-version))
         (description (eask-package-description))
         (reqs (mapcar (lambda (elm)
                         (list (if (stringp (car elm)) (intern (car elm)) (car elm))
                               (if (= (length (cdr elm)) 1)
                                   (nth 0 (cdr elm))
                                 "latest")))
                       (append eask-depends-on-emacs eask-depends-on))))
    (setq eask--pkg-filename pkg-file)
    (write-region
     (pp-to-string `(define-package ,name ,version ,description ',reqs))
     nil pkg-file)))

(eask-start
  (if eask-package-desc (eask--generate-from-pkg-desc)
    (eask--generate-from-eask-file))
  (eask-info "(Generated pkg-file -> %s)" eask--pkg-filename))

;;; pkg-file.el ends here

;; ~/lisp/core/recipe.el

;;; Commentary:
;;
;; Command would suggest you the recipe for current package:
;;
;;   $ eask recipe
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (if-let ((url (eask-package-desc-url)))
      (let* ((fetcher (cond ((string-match-p "github.com" url) 'github)
                            ((string-match-p "gitlab.com" url) 'gitlab)
                            (t 'git)))
             (url-regex (if (eq fetcher 'github)
                            "http[s]://github.com/"
                          "http[s]://gitlab.com/"))
             (repo (replace-regexp-in-string url-regex "" url))
             (recipe
              `(,(intern (eask-guess-package-name)) :fetcher ,fetcher)))
        (cond ((memq fetcher '(git hg))
               (nconc recipe `(:url ,url)))
              ((memq fetcher '(gitlab github))
               (nconc recipe `(:repo ,repo))))
        (when eask-files
          (nconc recipe `(:files ,(append '(:defaults) eask-files))))
        (eask-msg "")
        (eask-msg "Recipe: %s" (pp-to-string recipe))
        (eask-msg ""))
    (eask-info "(Repository URL is required to form a recipe)")
    (eask-help 'recipe)))

;;; recipe.el ends here

;; ~/lisp/core/refresh.el

;;; Commentary:
;;
;; Command use to download package archives
;;
;;   $ eask refresh
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (eask-pkg-init))

;;; refresh.el ends here

;; ~/lisp/core/reinstall.el

;;; Commentary:
;;
;; Command use to reinstall Emacs packages,
;;
;;   $ eask reinstall [names..]
;;
;;
;;  Initialization options:
;;
;;    [names..]     name of the package to reinstall
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-load "core/package")  ; load dist path

(defun eask--reinstall-packages (names)
  "Install packages."
  (let* ((names (mapcar #'intern names))
         (len (length names)) (s (eask--sinr len "" "s"))
         (pkg-not-installed (cl-remove-if #'package-installed-p names))
         (installed (length pkg-not-installed)) (skipped (- len installed)))
    (eask-log "Reinstalling %s specified package%s..." len s)
    (mapc #'eask-package-reinstall names)
    (eask-info "(Total of %s package%s reinstalled, %s skipped)"
               installed s skipped)))

(eask-start
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (if-let ((names (eask-args)))
      ;; If package [name..] are specified, we try to install it
      (eask--reinstall-packages names)
    (if-let* ((name (intern (eask-guess-package-name)))
              ((package-installed-p name)))
        (progn
          (eask-package-reinstall name)
          (eask-info "(Reinstalled %s)" name))
      (eask-info "✗ (No packages have been reintalled)")
      (eask-help 'reinstall))))

;;; reinstall.el ends here

;; ~/lisp/core/search.el

;;; Commentary:
;;
;; Command use to search Emacs packges,
;;
;;   $ eask search [queries..]
;;
;;
;;  Initialization options:
;;
;;    [queries..]     query to search packages
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-load "core/list")

(defun eask--search-packages (query)
  "Filter available packages with QUERY."
  (let ((result))
    (dolist (package (mapcar #'car package-archive-contents))
      (when (string-match-p query (format "%s" package))
        (push package result)))
    result))

(eask-start
  (eask-pkg-init)
  (if-let ((queries (eask-args)))
      (let ((result))
        (dolist (query queries)
          (setq result (append result (eask--search-packages query))))
        (delete-dups result)
        (eask--list result package-archive-contents)
        (eask-info "(Search result of %s package%s)" (length result)
                   (eask--sinr result "" "s")))
    (eask-info "(No search operation; missing queries specification)")
    (eask-help 'search)))

;;; search.el ends here

;; ~/lisp/core/uninstall.el

;;; Commentary:
;;
;; Command use to uninstall Emacs packages,
;;
;;   $ eask uninstall [names..]
;;
;;
;;  Initialization options:
;;
;;    [names..]     name of the package to uninstall; else we uninstall pacakge
;;                  from current workspace
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--uninstall-packages(names)
  "Uninstall packages."
  (let* ((names (mapcar #'intern names))
         (len (length names)) (s (eask--sinr len "" "s"))
         (pkg-installed (cl-remove-if-not #'package-installed-p names))
         (deleted (length pkg-installed)) (skipped (- len deleted)))
    (eask-log "Uninstalling %s specified package%s..." len s)
    (mapc #'eask-package-delete names)
    (eask-info "(Total of %s package%s deleted, %s skipped)"
               deleted s skipped)))

(eask-start
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (if-let ((names (eask-args)))
      (eask--uninstall-packages names)
    (if-let* ((name (intern (eask-guess-package-name)))
              ((package-installed-p name)))
        (progn
          (eask-package-delete name)
          (eask-info "(Deleted %s)" name))
      (eask-info "✗ (No packages have been unintalled)")
      (eask-help 'uninstall))))

;;; uninstall.el ends here

;; ~/lisp/core/upgrade.el

;;; Commentary:
;;
;; Command use to upgrade Emacs packages,
;;
;;   $ eask upgrade [names..]
;;
;;
;;  Initialization options:
;;
;;    [names..]     package to upgrade; else we upgrade all packages
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--package-version-string (pkg-desc)
  "Get package version string with color."
  (let ((version (package-desc-version pkg-desc)))
    (ansi-yellow (package-version-join version))))

(defun eask-package-upgrade (pkg-desc)
  "Upgrade package using PKG-DESC."
  (let* ((name (package-desc-name pkg-desc))
         (pkg-string (ansi-green name))
         (version-new (eask--package-version-string pkg-desc))
         (old-pkg-desc (eask-package-desc name t))
         (version-old (eask--package-version-string old-pkg-desc)))
    (eask-with-progress
      (format "  - Upgrading %s (%s) -> (%s)..." pkg-string version-old version-new)
      (eask-with-verbosity 'debug
        (when (eask-force-p) (package-delete old-pkg-desc))
        (package-install pkg-desc)
        (unless (eask-force-p) (package-delete old-pkg-desc)))
      "done ✓")))

(defun eask-package--upgradable-p (pkg)
  "Return non-nil if PKG can be upgraded."
  (let ((current (eask-package--version pkg t))
        (latest (eask-package--version pkg nil)))
    (version-list-< current latest)))

(defun eask-package--upgrades ()
  "Return a list of upgradable package description."
  (let (upgrades)
    (eask-with-progress
      (ansi-green "Collecting information for upgradable packages...")
      (dolist (pkg (mapcar #'car package-alist))
        (when (eask-package--upgradable-p pkg)
          (push (cadr (assq pkg package-archive-contents)) upgrades)))
      (ansi-green "done ✓"))
    upgrades))

(defun eask-package-upgrade-all ()
  "Upgrade for archive packages."
  (if-let ((upgrades (eask-package--upgrades)))
      (progn
        (mapcar #'eask-package-upgrade upgrades)
        (eask-info "(Done upgrading all packages)"))
    (eask-info "(All packages are up to date)")))

(eask-start
  (eask-pkg-init)
  (if-let ((names (eask-args)))
      (dolist (name names)
        (setq name (intern name))
        (if (package-installed-p name)
            (if (or (eask-package--upgradable-p name) (eask-force-p))
                (eask-package-upgrade (cadr (assq name package-archive-contents)))
              (eask-warn "Package `%s` is already up to date" name))
          (eask-error "Package does not exists `%s`, you need to install before upgrade" name)))
    (eask-package-upgrade-all)))

;;; upgrade.el ends here

;; ~/lisp/extern/ansi.el

;; Copyright (C) 2010-2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.4.1
;; Keywords: terminals color ansi
;; URL: http://github.com/rejeep/ansi
;; Package-Requires: ((emacs "24.1") (cl-lib "0.6"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Turns simple strings to ansi strings.

;; Turning a string into an ansi string can be to add color to a
;; text, add color in the background of a text or adding a style,
;; such as bold, underscore or italic.

;;; Code:

(require 'cl-lib)

(defgroup ansi nil
  "Turn string into ansi strings."
  :group 'lisp)

(defcustom ansi-inhibit-ansi nil
  "If non-nil, no apply ANSI code.
This variable affects `with-ansi', `with-ansi-princ'."
  :group 'ansi
  :type 'boolean)



(defconst ansi-colors
  '((black   . 30)
    (red     . 31)
    (green   . 32)
    (yellow  . 33)
    (blue    . 34)
    (magenta . 35)
    (cyan    . 36)
    (white   . 37))
  "List of text colors.")

(defconst ansi-on-colors
  '((on-black   . 40)
    (on-red     . 41)
    (on-green   . 42)
    (on-yellow  . 43)
    (on-blue    . 44)
    (on-magenta . 45)
    (on-cyan    . 46)
    (on-white   . 47))
  "List of colors to draw text on.")

(defconst ansi-styles
  '((bold       . 1)
    (dark       . 2)
    (italic     . 3)
    (underscore . 4)
    (blink      . 5)
    (rapid      . 6)
    (contrary   . 7)
    (concealed  . 8)
    (strike     . 9))
  "List of styles.")

(defvar ansi-csis
  '((up       . "A")
    (down     . "B")
    (forward  . "C")
    (backward . "D"))
  "...")

(defconst ansi-reset 0 "Ansi code for reset.")



(defun ansi--concat (&rest sequences)
  "Concat string elements in SEQUENCES."
  (apply #'concat (cl-remove-if-not 'stringp sequences)))

(defun ansi--code (effect)
  "Return code for EFFECT."
  (or
   (cdr (assoc effect ansi-colors))
   (cdr (assoc effect ansi-on-colors))
   (cdr (assoc effect ansi-styles))))

(defun ansi--char (effect)
  "Return char for EFFECT."
  (cdr (assoc effect ansi-csis)))

(defmacro ansi--define (effect)
  "Define ansi function with EFFECT."
  (let ((fn-name (intern (format "ansi-%s" (symbol-name effect)))))
    `(defun ,fn-name (format-string &rest objects)
       ,(format "Add '%s' ansi effect to text." effect)
       (apply 'ansi-apply (cons ',effect (cons format-string objects))))))

(defmacro with-ansi (&rest body)
  "Shortcut names (without ansi- prefix) can be used in this BODY."
  (if ansi-inhibit-ansi
      `(ansi--concat ,@body)
    `(cl-macrolet
         ,(mapcar
           (lambda (alias)
             (let ((fn (intern (format "ansi-%s" (symbol-name alias)))))
               `(,alias (string &rest objects)
                        ,(list 'backquote (list fn ',string ',@objects)))))
           (append
             (mapcar 'car ansi-colors)
             (mapcar 'car ansi-on-colors)
             (mapcar 'car ansi-styles)
             (mapcar 'car ansi-csis)))
       ,(cons 'ansi--concat body))))

(defmacro with-ansi-princ (&rest body)
  "Shortcut names (without ansi- prefix) can be used in this BODY and princ."
  (if ansi-inhibit-ansi
      `(princ (ansi--concat ,@body))
    `(princ (with-ansi ,@body))))

(defun ansi-apply (effect-or-code format-string &rest objects)
  "Apply EFFECT-OR-CODE to text.
FORMAT-STRING and OBJECTS are processed same as `apply'."
  (let* ((format-string (if (stringp format-string) format-string
                          (format "%s" format-string)))
         (code (if (numberp effect-or-code)
                   effect-or-code
                 (ansi--code effect-or-code)))
         (text (apply 'format format-string objects)))
    (if ansi-inhibit-ansi text
      (format "\e[%dm%s\e[%sm" code text ansi-reset))))

(defun ansi-csi-apply (effect-or-char &optional reps)
  "Apply EFFECT-OR-CHAR REPS (1 default) number of times."
  (let ((char (if (symbolp effect-or-char)
                  (ansi--char effect-or-char)
                effect-or-char)))
    (format "\u001b[%d%s" (or reps 1) char)))

(defun ansi-up (&optional n)
  "Move N steps (1 step default) up."
  (ansi-csi-apply 'up n))

(defun ansi-down (&optional n)
  "Move N steps (1 step default) down."
  (ansi-csi-apply 'down n))

(defun ansi-forward (&optional n)
  "Move N steps (1 step default) forward."
  (ansi-csi-apply 'forward n))

(defun ansi-backward (&optional n)
  "Move N steps (1 step default) backward."
  (ansi-csi-apply 'backward n))




(ansi--define black)
(ansi--define red)
(ansi--define green)
(ansi--define yellow)
(ansi--define blue)
(ansi--define magenta)
(ansi--define cyan)
(ansi--define white)

(ansi--define on-black)
(ansi--define on-red)
(ansi--define on-green)
(ansi--define on-yellow)
(ansi--define on-blue)
(ansi--define on-magenta)
(ansi--define on-cyan)
(ansi--define on-white)

(ansi--define bold)
(ansi--define dark)
(ansi--define italic)
(ansi--define underscore)
(ansi--define blink)
(ansi--define rapid)
(ansi--define contrary)
(ansi--define concealed)
(ansi--define strike)

(provide 'ansi)

;;; ansi.el ends here

;; ~/lisp/extern/github-elpa.el
;;; Commentary:
;;; Code:

(unless eask-depends-on-recipe-p
  (defvar github-elpa-working-dir (expand-file-name "./temp-elpa/.working/" user-emacs-directory))
  (defvar github-elpa-archive-dir (expand-file-name "./temp-elpa/packages/" user-emacs-directory))
  (defvar github-elpa-recipes-dir (expand-file-name "./temp-elpa/recipes/" user-emacs-directory))

  (ignore-errors (make-directory   github-elpa-working-dir t))
  (ignore-errors (make-directory   github-elpa-archive-dir t))

  (ignore-errors (delete-directory github-elpa-recipes-dir t))
  (ignore-errors (make-directory   github-elpa-recipes-dir t)))

(defun github-elpa-build ()
  "Github elpa build."
  (eask-load "extern/package-build")  ; override
  (let ((package-build-working-dir github-elpa-working-dir)
        (package-build-archive-dir github-elpa-archive-dir)
        (package-build-recipes-dir github-elpa-recipes-dir))
    ;;(github-elpa--git-check-repo)
    ;;(github-elpa--git-check-workdir-clean)
    (make-directory package-build-archive-dir t)
    ;; Currently no way to detect build failure...
    (dolist (recipe (directory-files package-build-recipes-dir nil "^[^.]"))
      (message "")
      (message "")
      (message ":: temp-elpa: packaging recipe %s" recipe)
      (package-build-archive recipe))
    (package-build-cleanup)))

;;; github-elpa.el ends here

;; ~/lisp/extern/package-build.el
;;; Commentary:
;;; Code:

(require 'package-build nil t)

(defun package-build--create-tar (name version directory mtime)
  "Create a tar file containing the contents of VERSION of package NAME.
DIRECTORY is a temporary directory that contains the directory
that is put in the tarball.  MTIME is used as the modification
time of all files, making the tarball reproducible."
  (let ((tar (expand-file-name (concat name "-" version ".tar")
                               package-build-archive-dir))
        (dir (concat name "-" version)))
    ;; XXX: https://github.com/melpa/package-build/pull/34
    ;;
    ;; We definitely need to remove these two lines, or else it won't able to
    ;; build on Windows.
    ;;
    ;; (when (eq system-type 'windows-nt)
    ;;   (setq tar (replace-regexp-in-string "^\\([a-z]\\):" "/\\1" tar)))
    (let ((default-directory directory))
      (process-file
       package-build-tar-executable nil
       (get-buffer-create "*package-build-checkout*") nil
       "-cf" tar dir
       ;; Arguments that are need to strip metadata that
       ;; prevent a reproducable tarball as described at
       ;; https://reproducible-builds.org/docs/archives.
       "--sort=name"
       (format "--mtime=@%d" mtime)
       "--owner=0" "--group=0" "--numeric-owner"
       "--pax-option=exthdr.name=%d/PaxHeaders/%f,delete=atime,delete=ctime"))
    (when (and package-build-verbose noninteractive)
      (message "Created %s containing:" (file-name-nondirectory tar))
      (dolist (line (sort (process-lines package-build-tar-executable
                                         "--list" "--file" tar)
                          #'string<))
        (message "  %s" line)))))

;;
;; NOTE: Following code are  brought in cuz it's very useful, but we don't want
;; to bring the whole `package-build' package unless it's needed.
;;

(defconst package-build-default-files-spec
  '("*.el" "lisp/*.el"
    "dir" "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
    (:exclude
     ".dir-locals.el" "lisp/.dir-locals.el"
     "test.el" "tests.el" "*-test.el" "*-tests.el"
     "lisp/test.el" "lisp/tests.el" "lisp/*-test.el" "lisp/*-tests.el"))
  "Default value for :files attribute in recipes.")

(defun package-build-expand-file-specs (dir specs &optional subdir allow-empty)
  "In DIR, expand SPECS, optionally under SUBDIR.
The result is a list of (SOURCE . DEST), where SOURCE is a source
file path and DEST is the relative path to which it should be copied.

If the resulting list is empty, an error will be reported.  Pass t
for ALLOW-EMPTY to prevent this error."
  (let ((default-directory dir)
        (prefix (if subdir (format "%s/" subdir) ""))
        (lst))
    (dolist (entry specs)
      (setq lst
            (if (consp entry)
                (if (eq :exclude (car entry))
                    (cl-nset-difference lst
                                        (package-build-expand-file-specs
                                         dir (cdr entry) nil t)
                                        :key #'car
                                        :test #'equal)
                  (nconc lst
                         (package-build-expand-file-specs
                          dir
                          (cdr entry)
                          (concat prefix (car entry))
                          t)))
              (nconc
               lst (mapcar (lambda (f)
                             (cons f
                                   (concat prefix
                                           (replace-regexp-in-string
                                            "\\.el\\.in\\'"
                                            ".el"
                                            (file-name-nondirectory f)))))
                           (file-expand-wildcards entry))))))
    (when (and (null lst) (not allow-empty))
      (error "No matching file(s) found in %s: %s" dir specs))
    lst))

;;; package-build.el ends here

;; ~/lisp/extern/package-recipe.el
;;; Commentary:
;;; Code:

(require 'package-recipe nil t)

;; Specializations of package-build classes and methods to define a
;; directory based recipe.
(defclass package-directory-recipe (package-recipe)
  ((dir           :initarg :dir   :initform ".")))

(cl-defmethod package-recipe--working-tree ((rcp package-directory-recipe))
  (oref rcp dir))

(cl-defmethod package-build--get-commit ((_rcp package-directory-recipe)))

(cl-defmethod package-build--get-commit-time ((_rcp package-directory-recipe) _rev)
  (let ((now (current-time))) (logior (lsh (car now) 16) (cadr now))))

;;; package-recipe.el ends here

;; ~/lisp/extern/package.el

;;; Commentary:
;;
;; This module is used to compatible with older Emacs version.
;;

;;; Code:

(eask-defvc< 28
  (defvar package-quickstart-file
    (locate-user-emacs-file "package-quickstart.el"))

  (defun package--alist ()
    "Return `package-alist', after computing it if needed."
    (or package-alist
        (progn (package-load-all-descriptors)
               package-alist)))

  (defun package--activate-all ()
    (dolist (elt (package--alist))
      (condition-case err
          (package-activate (car elt))
        ;; Don't let failure of activation of a package arbitrarily stop
        ;; activation of further packages.
        (error (message "%s" (error-message-string err))))))

  (defun package-activate-all ()
    "Activate all installed packages.
The variable `package-load-list' controls which packages to load."
    (setq package--activated t)
    (let* ((elc (concat package-quickstart-file "c"))
           (qs (if (file-readable-p elc) elc
                 (if (file-readable-p package-quickstart-file)
                     package-quickstart-file))))
      (if qs
          ;; Skip load-source-file-function which would slow us down by a factor
          ;; 2 when loading the .el file (this assumes we were careful to
          ;; save this file so it doesn't need any decoding).
          (let ((load-source-file-function nil))
            (unless (boundp 'package-activated-list)
              (setq package-activated-list nil))
            (load qs nil 'nomessage))
        (require 'package)
        (package--activate-all)))))

;;; package.el ends here

;; ~/lisp/extern/s.el
;;; Commentary:
;;; Code:

(defun s-replace (old new s)
  "Replaces OLD with NEW in S."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (regexp-quote old) new s t t))

;;; s.el ends here

;; ~/lisp/lint/checkdoc.el

;;; Commentary:
;;
;; Commmand use to run `checkdoc' for all files
;;
;;   $ eask lint checkdoc [files..]
;;
;;
;;  Initialization options:
;;
;;    [files..]     files you want checkdoc to run on
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defvar eask--checkdoc-errors nil "Error flag.")

(defun eask--checkdoc-print-error (text start end &optional unfixable)
  "Print error for checkdoc."
  (setq eask--checkdoc-errors t)
  (let ((msg (concat (checkdoc-buffer-label) ":"
                     (int-to-string (count-lines (point-min) (or start (point-min))))
                     ": " text)))
    (if (eask-strict-p) (error msg) (warn msg))
    ;; Return nil because we *are* generating a buffered list of errors.
    nil))

(setq checkdoc-create-error-function #'eask--checkdoc-print-error)

(defun eask--checkdoc-file (filename)
  "Run checkdoc on FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         (eask--checkdoc-errors))
    (eask-msg "")
    (eask-msg "`%s` with checkdoc (%s)" (ansi-green file) checkdoc-version)
    (checkdoc-file filename)
    (unless eask--checkdoc-errors (eask-msg "No issues found"))))

(eask-start
  (require 'checkdoc)
  (if-let* ((files (eask-args-or-package-el-files))
            (len (length files))
            (s (eask--sinr len "" "s"))
            (have (eask--sinr len "has" "have")))
      (progn
        (mapcar #'eask--checkdoc-file files)
        (eask-info "(Total of %s file%s %s checked)" len s have))
    (eask-info "(No files have been checked (checkdoc))")
    (if (eask-args)
        (eask--print-no-matching-files)
      (eask-help "lint/checkdoc"))))

;;; checkdoc.el ends here

;; ~/lisp/lint/declare.el

;;; Commentary:
;;
;; Commmand use to run `check-declare' for all files
;;
;;   $ eask lint declare [files..]
;;
;;
;;  Initialization options:
;;
;;    [files..]     files you want check-declare to run on
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--check-declare-file (filename)
  "Run check-declare on FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         (errors))
    (eask-msg "")
    (eask-msg "`%s` with check-declare" (ansi-green file))
    (setq errors (check-declare-file filename))
    (if errors
        (with-current-buffer check-declare-warning-buffer
          (eask-msg (buffer-string)))
      (eask-msg "No issues found"))))

(eask-start
  (require 'check-declare)
  (if-let* ((files (eask-args-or-package-el-files))
            (len (length files))
            (s (eask--sinr len "" "s"))
            (have (eask--sinr len "has" "have")))
      (progn
        (mapcar #'eask--check-declare-file files)
        (eask-info "(Total of %s file%s %s checked)" len s have))
    (eask-info "(No files have been checked (declare))")
    (if (eask-args)
        (eask--print-no-matching-files)
      (eask-help "lint/declare"))))

;;; declare.el ends here

;; ~/lisp/lint/elint.el

;;; Commentary:
;;
;; Commmand use to run `elint' for all files
;;
;;   $ eask lint elint [files..]
;;
;;
;;  Initialization options:
;;
;;    [files..]     files you want elint to run on
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--elint-file (filename)
  "Run elint on FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         (noninteractive))
    (eask-msg "")
    (eask-msg "`%s` with elint" (ansi-green file))
    (eask-with-verbosity 'debug (elint-file filename))
    (eask-print-log-buffer (elint-get-log-buffer))
    (kill-buffer (elint-get-log-buffer))))

(eask-start
  (require 'elint)
  (if-let* ((files (eask-args-or-package-el-files))
            (len (length files))
            (s (eask--sinr len "" "s"))
            (have (eask--sinr len "has" "have")))
      (progn
        (mapcar #'eask--elint-file files)
        (eask-info "(Total of %s file%s %s checked)" len s have))
    (eask-info "(No files have been checked (elint))")
    (if (eask-args)
        (eask--print-no-matching-files)
      (eask-help 'elint))))

;;; elint.el ends here

;; ~/lisp/lint/elsa.el

;;; Commentary:
;;
;; Commmand use to run `elsa' for all files
;;
;;   $ eask lint elsa [files..]
;;
;;
;;  Initialization options:
;;
;;    [files..]     files you want elint to run on
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defconst eask--elsa-version nil
  "Elsa version.")

(defun eask--elsa-process-file (filename)
  "Process FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         errors)
    (eask-msg "")
    (eask-msg "`%s` with elsa (%s)" (ansi-green file) eask--elsa-version)
    (eask-with-verbosity 'debug
      (setq errors (oref (elsa-process-file filename) errors)))
    (if errors
        (--each (reverse errors)
          (let ((line (string-trim (concat file ":" (elsa-message-format it)))))
            (cond ((string-match-p "[: ][Ee]rror:" line) (eask-error line))
                  ((string-match-p "[: ][Ww]arning:" line) (eask-warn line))
                  (t (eask-log line)))))
      (eask-msg "No issues found"))))

(eask-start
  (eask-with-archives "melpa"
    (eask-package-install 'elsa))
  (setq eask--elsa-version (eask-package--version-string 'elsa))
  (require 'elsa)
  (if-let ((files (eask-args-or-package-el-files)))
      (progn
        (mapcar #'eask--elsa-process-file files)
        (eask-info "(Total of %s files linted)" (length files)))
    (eask-info "(No files have been linted)")
    (if (eask-args)
        (eask--print-no-matching-files)
      (eask-help 'elsa))))

;;; elsa.el ends here

;; ~/lisp/lint/indent.el

;;; Commentary:
;;
;; Command use to check package with indent-lint,
;;
;;   $ eask lint indent [files..]
;;
;;
;;  Initialization options:
;;
;;    [files..]     files you want indent-lint to run on
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--undo-lines (undo-list)
  "Return list of lines changed in UNDO-LIST."
  (let ((lines))
    (dolist (elm undo-list)
      (when (and (consp elm) (numberp (cdr elm)))
        (push (line-number-at-pos (abs (cdr elm))) lines)))
    (reverse lines)))

(defun eask--indent-lint-file (file)
  "Lint indent for FILE."
  (eask-msg "")
  (eask-msg "`%s` with indent-lint" (ansi-green (eask-root-del file)))
  (find-file file)
  (let ((tick (buffer-modified-tick)))
    (eask--silent (indent-region (point-min) (point-max)))
    (if (/= tick (buffer-modified-tick))
        ;; Indentation changed: warn for each line.
        (dolist (line (eask--undo-lines buffer-undo-list))
          (eask-report "%s:%s: mismatch indentation" (buffer-name) line))
      (eask-log "No mismatch indentation found"))))

(eask-start
  (if-let ((files (if (eask-args)
                      (eask-expand-file-specs (eask-args))
                    (eask-package-el-files))))
      (progn
        (mapcar #'eask--indent-lint-file files)
        (eask-info "(Total of %s files linted)" (length files)))
    (eask-info "(No files have been linted)")
    (if (eask-args)
        (eask--print-no-matching-files)
      (eask-help "package/indent"))))

;;; indent.el ends here

;; ~/lisp/lint/keywords.el

;;; Commentary:
;;
;; Command use to check keywords header inside the package,
;;
;;   $ eask lint keywords
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(require 'finder)

(defun eask--defined-keywords (keywords)
  "Return t if KEYWORDS are defined correctly."
  (let ((available-keywords (mapcar #'car finder-known-keywords))
        (result))
    (dolist (keyword keywords)
      (when (member keyword available-keywords)
        (setq result t)))
    result))

(eask-start
  (let ((keywords (eask-package-desc-keywords))
        (file (eask-root-del eask-package-file)))
    (cond
     ((not eask-package-file)
      (eask-report "Can't lint keywords without the package-file specified")
      (eask-help "lint/keywords-file"))
     ((not keywords)
      (eask-report "Keywords header seems to be missing in the `%s' file" file)
      (eask-help "lint/keywords-header"))
     (t
      (eask-msg "")
      (eask-msg "`%s` with keywords-lint" (ansi-green file))
      (if (eask--defined-keywords keywords)
          (eask-info "(No issues found.)")
        (eask-report "Missing a standard keyword, consider adding one to the Keywords header!")
        (eask-help "lint/keywords"))))))

;;; keywords.el ends here

;; ~/lisp/lint/package.el

;;; Commentary:
;;
;; Command use to lint current Emacs package,
;;
;;   $ eask lint package [files..]
;;
;;
;;  Initialization options:
;;
;;    [files..]     specify files to do package lint
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

;; Handle options
(add-hook 'eask-before-command-hook
          (lambda ()
            (setq package-lint-batch-fail-on-warnings t)))

(defconst eask--package-lint-version nil
  "`package-lint' version.")

(defun eask--package-lint-file (filename)
  "Package lint FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename)))
    (eask-msg "")
    (eask-msg "`%s` with package-lint (%s)" (ansi-green file) eask--package-lint-version)
    (with-current-buffer (find-file filename)
      (package-lint-current-buffer)
      (kill-this-buffer)))
  (eask-print-log-buffer "*Package-Lint*"))

(eask-start
  (eask-with-archives "melpa"
    (eask-package-install 'package-lint))
  (setq eask--package-lint-version (eask-package--version-string 'package-lint))
  (require 'package-lint)
  (if-let ((files (eask-args-or-package-el-files)))
      (progn
        (eask-pkg-init)
        (setq package-lint-main-file eask-package-file)
        (mapcar #'eask--package-lint-file files)
        (eask-info "(Total of %s files linted)" (length files)))
    (eask-info "(No files have been linted)")
    (if (eask-args)
        (eask--print-no-matching-files)
      (eask-help "lint/package"))))

;;; package.el ends here

;; ~/lisp/lint/regexps.el

;;; Commentary:
;;
;; Commmand use to run `relint' for all files
;;
;;   $ eask lint regexps [files..]
;;
;;
;;  Initialization options:
;;
;;    [files..]     files you want relint to run on
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defconst eask--relint-version nil
  "`relint' version.")

(defun eask--relint-file (filename)
  "Package lint FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         (errors))
    (eask-msg "")
    (eask-msg "`%s` with relint (%s)" (ansi-green file) eask--relint-version)
    (with-current-buffer (find-file filename)
      (setq errors (relint-buffer (current-buffer)))
      (dolist (err errors)
        (let* ((msg       (nth 0 err))
               (error-pos (nth 2 err))
               (severity  (nth 5 err))
               (report-func (pcase severity
                              (`error #'eask-error)
                              (`warning #'eask-warn))))
          (funcall report-func "%s:%s %s: %s"
                   file (line-number-at-pos error-pos)
                   (capitalize (eask-2str severity)) msg)))
      (kill-this-buffer))))

(eask-start
  (eask-with-archives "gnu"
    (eask-package-install 'relint))
  (setq eask--relint-version (eask-package--version-string 'relint))
  (require 'relint)
  (if-let ((files (eask-args-or-package-el-files)))
      (progn
        (setq package-lint-main-file eask-package-file)
        (mapcar #'eask--relint-file files)
        (eask-info "(Total of %s files linted)" (length files)))
    (eask-info "(No files have been linted)")
    (if (eask-args)
        (eask--print-no-matching-files)
      (eask-help 'regexps))))

;;; regexps.el ends here

;; ~/lisp/test/buttercup.el

;;; Commentary:
;;
;; Command to run buttercup tests,
;;
;;   $ eask buttercup
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (eask-with-archives "melpa"
    (eask-package-install 'buttercup))
  (require 'buttercup)
  ;; Propose fix from https://github.com/jorgenschaefer/emacs-buttercup/pull/217
  (let ((load-path (cons "." load-path)))
    (buttercup-run-discover)))

;;; buttercup.el ends here

;; ~/lisp/test/ert-runner.el

;;; Commentary:
;;
;; Command to run ert tests using ert-runner,
;;
;;   $ eask ert-runner
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

;; Handle options
(add-hook 'eask-before-command-hook
          (lambda ()
            (when (= eask-verbosity 4) (setq ert-runner-verbose t))))

(eask-start
  (eask-with-archives "melpa"
    (eask-package-install 'ert-runner))
  (require 'ert-runner))

;;; ert-runner.el ends here

;; ~/lisp/test/ert.el

;;; Commentary:
;;
;; Command to run ert tests using ert-runner,
;;
;;   $ eask ert [files..]
;;
;;
;;  Initialization options:
;;
;;    [files..]     specify files to run ert tests
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(require 'ert)

(defvar ert--message-loop nil
  "Prevent inifinite recursive message function.")

(defun eask--ert-message (func &rest args)
  "Colorized ert messages."
  (if ert--message-loop (apply func args)
    (let ((ert--message-loop t))
      (cond
       ((string-match-p "^[ ]+FAILED " (apply #'format args))
        (eask-msg (ansi-red (apply #'format args))))
       ((string-match-p "^[ ]+SKIPPED " (apply #'format args))
        (eask-msg (ansi-white (apply #'format args))))
       ((string-match-p "^[ ]+passed " (apply #'format args))
        (eask-msg (ansi-green (apply #'format args))))
       (t (apply func args))))))

(advice-add 'message :around #'eask--ert-message)

(eask-start
  (if-let ((files (eask-expand-file-specs (eask-args))))
      (progn
        (eask-pkg-init)
        (eask-ignore-errors
          (mapc #'load-file files)
          (ert-run-tests-batch-and-exit)))
    (eask-info "(No tests found.)")
    (eask-help "test/ert")))

;;; ert.el ends here

;; ~/lisp/_prepare.el
;;; Commentary: Prepare to setup Eask environment for sandboxing
;;; Code:

(require 'ansi-color)
(require 'package)
(require 'project)
(require 'nsm)
(require 'url-vars)

(require 'cl-lib)
(require 'ls-lisp)
(require 'pp)
(require 'rect)
(require 'subr-x)

;; Determine the underlying operating system
(defconst eask-is-windows (memq system-type '(cygwin windows-nt ms-dos))   "Windows")
(defconst eask-is-mac     (eq system-type 'darwin)                         "macOS")
(defconst eask-is-linux   (eq system-type 'gnu/linux)                      "Linux")
(defconst eask-is-bsd     (or eask-is-mac (eq system-type 'berkeley-unix)) "BSD")

(defconst eask-system-type
  (cond (eask-is-windows 'dos)
        (eask-is-bsd     'mac)
        (eask-is-linux   'unix)
        (t               'unknown))
  "Return current OS type.")

(setq make-backup-files nil)

(setq package-enable-at-startup  nil            ; To avoid initializing twice
      package-check-signature    nil
      package-archives           nil            ; Leave it to custom use
      package-archive-priorities nil)

(defun eask--load--adv (fnc &rest args)
  "Prevent `_prepare.el' loading twice."
  (unless (string= (nth 0 args) (eask-script "_prepare")) (apply fnc args)))
(advice-add 'load :around #'eask--load--adv)

;;
;;; Execution

(defconst eask-argv argv
  "This stores the real argv; the argv will soon be replaced with `(eask-args)'.")

(defconst eask--script (nth 1 (or (member "-scriptload" command-line-args)
                                  (member "-l" command-line-args)))
  "Script currently executing.")

(defconst eask-lisp-root
  (expand-file-name
   (concat (file-name-directory eask--script) "../"))
  "Source lisp directory; should always end with slash.")

(defun eask-command ()
  "What's the current command?

If the command is with subcommand, it will return command with concatenate with
dash separator. For example, the following:

   $ eask lint checkdoc [FILES..]

will return `lint-checkdoc' with a dash between two subcommands."
  (let* ((script-dir (file-name-directory eask--script))
         (script-file (file-name-sans-extension (file-name-nondirectory eask--script)))
         (module-name (s-replace eask-lisp-root "" script-dir))
         (module-name (s-replace "/" "" module-name)))
    ;; Ignore if it's inside core module
    (if (member module-name '("core" "checker")) script-file
      (concat module-name "-" script-file))))

(defun eask-special-p ()
  "Return t if the command that can be run without Eask-file existence."
  (member (eask-command) '("keywords")))

(defun eask-checker-p ()
  "Return t if running Eask as the checker."
  (member (eask-command) '("check-eask")))

(defun eask-script (script)
  "Return full script filename."
  (concat eask-lisp-root script ".el"))

(defvar eask-loading-file-p nil
  "This became t; if we are loading script from another file and not expecting
the `eask-start' execution.")

(defun eask-load (script)
  "Load another eask script; so we can reuse functions across all scripts."
  (let ((eask-loading-file-p t)) (eask-call script)))

(defun eask-call (script)
  "Call another eask script."
  (if-let* ((script-file (eask-script script))
            ((file-exists-p script-file)))
      (load script-file nil t)
    (eask-error "Scripting missing %s..." script-file)))

;;
;;; Util

(defmacro eask-defvc< (version &rest body)
  "Define scope if Emacs version is below VERSION."
  (declare (indent 1) (debug t))
  `(when (< emacs-major-version ,version) ,@body))

(defmacro eask--silent (&rest body)
  "Execute BODY without message."
  (declare (indent 0) (debug t))
  `(let ((inhibit-message t) message-log-max) ,@body))

(defmacro eask--unsilent (&rest body)
  "Execute BODY with message."
  (declare (indent 0) (debug t))
  `(let (inhibit-message) ,@body))

(defun eask-2str (obj)
  "Convert OBJ to string."
  (format "%s" obj))

(defun eask-listify (obj)
  "Turn OBJ to list."
  (if (listp obj) obj (list obj)))

(defun eask-intern (obj)
  "Safely intern OBJ."
  (if (stringp obj) (intern obj) obj))

(defun eask--sinr (len-or-list form-1 form-2)
  "If LEN-OR-LIST has length of 1; return FORM-1, else FORM-2."
  (let ((len (if (numberp len-or-list) len-or-list (length len-or-list))))
    (if (= 1 len) form-1 form-2)))

(defun eask-seq-str-max (sequence)
  "Return max length in list of strings."
  (let ((result 0))
    (mapc (lambda (elm) (setq result (max result (length (eask-2str elm))))) sequence)
    result))

;;
;;; Archive

(defun eask--download-archives ()
  "If archives download failed; download it manually."
  (dolist (archive package-archives)
    (let* ((location (cdr archive))
           (name (car archive))
           (file "archive-contents")
           (dir (expand-file-name (concat "archives/" name) package-user-dir))
           (local-file (expand-file-name file dir))
           (url (format
                 "https://raw.githubusercontent.com/emacs-eask/archives/master/%s/%s" name file))
           (download-p)
           (local-archive-p (string= name "local")))  ; exclude local elpa
      (unless (file-exists-p local-file)
        (eask-with-progress
          (format "Downloading archive `%s' manually... " (ansi-yellow name))
          (unless local-archive-p
            (if (url-file-exists-p url)
                (progn
                  (ignore-errors (make-directory dir t))
                  (url-copy-file url local-file t)
                  (setq download-p t))
              (eask-debug "No archive-contents found in `%s'" (ansi-yellow name))))
          (cond (download-p      "done ✓")
                (local-archive-p "skipped ✗")
                (t               "failed ✗"))))
      (when download-p (eask-pkg-init t)))))

;;
;;; Package

(defun eask--update-exec-path ()
  "Add all bin directory to `exec-path'."
  (dolist (filename (directory-files-recursively package-user-dir "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
    (when (string-suffix-p "bin/" (file-name-directory filename))
      (add-to-list 'exec-path (file-name-directory filename) t)))
  (delete-dups exec-path))

(defun eask--update-load-path ()
  "Add all load-path for all .el files."
  (dolist (filename (eask-package-el-files))
    (add-to-list 'load-path (file-name-directory filename) t))
  (delete-dups load-path))

(defun eask-dependencies ()
  "Return list of dependencies."
  (append eask-depends-on (and (eask-dev-p) eask-depends-on-dev)))

(defun eask--install-deps (dependencies msg)
  "Install DEPENDENCIES."
  (let* ((names (mapcar #'car dependencies))
         (names (mapcar #'eask-intern names))
         (len (length dependencies))
         (ies (eask--sinr len "y" "ies"))
         (pkg-installed (cl-remove-if #'package-installed-p names))
         (installed (length pkg-installed)) (skipped (- len installed)))
    (eask-log "Installing %s %s dependenc%s..." len msg ies)
    (mapc #'eask-package-install names)
    (eask-info "(Total of %s dependenc%s installed, %s skipped)"
               installed ies skipped)))

(defun eask-install-dependencies ()
  "Install dependencies defined in Eask file."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (when eask-depends-on-recipe-p
    (eask-log "Installing required external packages...")
    (eask-with-archives "melpa"
      (eask-package-install 'package-build))
    (eask-with-progress
      "Building temporary archives (this may take a while)... "
      (eask-with-verbosity 'debug (github-elpa-build))
      "done ✓")
    (eask-pkg-init t))
  (when eask-depends-on
    (eask--install-deps eask-depends-on "package"))
  (when (and eask-depends-on-dev (eask-dev-p))
    (eask--install-deps eask-depends-on-dev "development")))

(defun eask-setup-paths ()
  "Setup both `exec-path' and `load-path'."
  (eask-with-progress
    (ansi-green "Updating environment variables... ")
    (eask-with-verbosity 'debug
      (eask--update-exec-path) (eask--update-load-path)
      (setenv "PATH" (string-join exec-path path-separator))
      (setenv "EMACSLOADPATH" (string-join load-path path-separator)))
    (ansi-green "done ✓")))

(defvar eask--package-initialized nil
  "Flag for package initialization in global scope.")

(defun eask-pkg-init (&optional force)
  "Package initialization."
  (when (or (not package--initialized) (not package-archive-contents) force
            ;; XXX we need to initialize once in global scope since most Emacs
            ;; configuration would likely to set `package-archives' variable
            ;; themselves.
            (and (eask-global-p) (not eask--package-initialized)))
    (setq eask--package-initialized t)
    (eask-with-progress
      (ansi-green "Loading package information... ")
      (eask-with-verbosity 'debug
        (package-initialize t) (package-refresh-contents)
        (eask--download-archives))
      (ansi-green "done ✓"))))

(defun eask--pkg-transaction-vars (pkg)
  "Return 1 symbol and 2 strings."
  (let* (;; Ensure symbol
         (pkg (if (stringp pkg) (intern pkg) pkg))
         ;; Wrap package name with color
         (pkg-string (ansi-green pkg))
         ;; Wrap version number with color
         (pkg-version (ansi-yellow (eask-package--version-string pkg))))
    (list pkg pkg-string pkg-version)))

(defmacro eask--pkg-process (pkg &rest body)
  "Execute BODY with PKG's related variables."
  (declare (indent 1) (debug t))
  `(let* ((pkg-info (eask--pkg-transaction-vars ,pkg))
          (pkg      (nth 0 pkg-info))
          (name     (nth 1 pkg-info))
          (version  (nth 2 pkg-info)))
     ,@body))

(defmacro eask-with-archives (archives &rest body)
  "Scope that temporary makes ARCHIVES available."
  (declare (indent 1) (debug t))
  `(let ((package-archives package-archives)
         (archives (eask-listify ,archives))
         (added))
     (dolist (archive archives)
       (unless (assoc archive package-archives)
         (setq added t)
         (eask-with-progress
           (format "Adding required archives (%s)... " (ansi-yellow archive))
           (eask-source archive)
           "done ✓")))
     (when added
       (eask-with-progress
         "Refresh archives information... "
         (eask--silent (eask-pkg-init t))
         "done ✓"))
     ,@body))

(defun eask-package-installable-p (pkg)
  "Return non-nil if package is installable."
  (assq (if (stringp pkg) (intern pkg) pkg) package-archive-contents))

(defun eask-package-install (pkg)
  "Install the package."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask--pkg-process pkg
    (cond
     ((package-installed-p pkg)
      (eask-msg "  - Skipping %s (%s)... already installed ✗" name version))
     ((progn
        (eask-pkg-init)
        (unless (eask-package-installable-p pkg)
          (eask-error "Package not installable `%s'; make sure package archive is included" pkg))))
     ((when-let* ((desc (eask-package-desc pkg))
                  (req-emacs (assoc 'emacs (package-desc-reqs desc)))
                  (req-emacs (package-version-join (nth 0 (cdr req-emacs))))
                  ((version< emacs-version req-emacs)))
        (if (eask-strict-p)
            (eask-error "  - Skipping %s (%s)... it requires Emacs %s and above ✗"
                        pkg (eask-package--version-string pkg) emacs-version)
          (eask-msg "  - Skipping %s (%s)... it requires Emacs %s and above ✗"
                    name version (ansi-yellow emacs-version)))))
     (t
      (eask--pkg-process pkg
        (eask-with-progress
          (format "  - Installing %s (%s)... " name version)
          (eask-with-verbosity 'debug
            ;; XXX Without ignore-errors guard, it will trigger error
            ;;
            ;;   Can't find library xxxxxxx.el
            ;;
            ;; But we can remove this after Emacs 28, since function `find-library-name'
            ;; has replaced the function `signal' instead of the `error'.
            (eask-ignore-errors (package-install pkg)))
          "done ✓"))))))

(defun eask-package-delete (pkg)
  "Delete the package."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask--pkg-process pkg
    (cond
     ((not (package-installed-p pkg))
      (eask-msg "  - Skipping %s (%s)... not installed ✗" name version))
     (t
      (eask--pkg-process pkg
        (eask-with-progress
          (format "  - Uninstalling %s (%s)... " name version)
          (eask-with-verbosity 'debug
            (package-delete (eask-package-desc pkg t) (eask-force-p)))
          "done ✓"))))))

(defun eask-package-reinstall (pkg)
  "Reinstall the package."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask--pkg-process pkg
    (cond
     ((not (package-installed-p pkg))
      (eask-msg "  - Skipping %s (%s)... not installed ✗" name version))
     (t
      (eask-pkg-init)
      (eask--pkg-process pkg
        (eask-with-progress
          (format "  - Reinstalling %s (%s)... " name version)
          (eask-with-verbosity 'debug
            (package-delete (eask-package-desc pkg t) t)
            (eask-ignore-errors (package-install pkg)))
          "done ✓"))))))

(defun eask-package-desc (name &optional current)
  "Build package description by PKG-NAME."
  (cadr (assq name (if current package-alist
                     (or package-archive-contents package-alist)))))

(defun eask-package--version (name &optional current)
  "Return PKG's version."
  (when-let ((desc (eask-package-desc name current)))
    (package-desc-version desc)))

(defun eask-package--version-string (pkg)
  "Return PKG's version."
  (if-let ((version (eask-package--version pkg)))
      (package-version-join version)
    ;; Just in case, but this should never happens!
    "latest"))

(defun eask-package-desc-url ()
  "Return url from package descriptor."
  (when eask-package-desc
    (when-let ((extras (package-desc-extras eask-package-desc)))
      (cdr (assoc :url extras)))))

(defun eask-package-desc-keywords ()
  "Return keywords from package descriptor."
  (when eask-package-desc (package-desc--keywords eask-package-desc)))

(defun eask-pkg-el ()
  "Return package description file if exists."
  (let ((pkg-el (package--description-file default-directory)))
    (when (file-readable-p pkg-el) pkg-el)))

;;
;;; Environments

(defconst eask-has-colors (getenv "EASK_HASCOLORS")
  "Return non-nil if terminal support colors.")

;;
;;; Flags

(defun eask--str2num (str) (ignore-errors (string-to-number str)))

(defun eask--flag (flag)
  "Return non-nil if FLAG exists.."
  (member (concat "--eask" flag) eask-argv))

(defun eask--flag-value (flag)
  "Return value for FLAG."
  (nth 1 (eask--flag flag)))

;;; Boolean
(defun eask-global-p ()        (eask--flag "-g"))               ; -g, --global
(defun eask-force-p ()         (eask--flag "-f"))               ; -f, --force
(defun eask-dev-p ()           (eask--flag "--dev"))            ; --dev, --development
(defun eask-debug-p ()         (eask--flag "--debug"))          ; --debug
(defun eask-strict-p ()        (eask--flag "--strict"))         ; --strict
(defun eask-timestamps-p ()    (eask--flag "--timestamps"))     ; --timestamps
(defun eask-log-level-p ()     (eask--flag "--log-level"))      ; --log-level
(defun eask-log-file-p ()      (eask--flag "--log-file"))       ; --log-file, --lf
(defun eask-elapsed-time-p ()  (eask--flag "--elapsed-time"))   ; --elapsed-time, --et
(defun eask-allow-error-p ()   (eask--flag "--allow-error"))    ; --allow-error
(defun eask-insecure-p ()      (eask--flag "--insecure"))       ; --insecure
(defun eask-no-color-p ()      (eask--flag "--no-color"))       ; --no-color

;;; String (with arguments)
(defun eask-proxy ()       (eask--flag-value "--proxy"))        ; --proxy
(defun eask-http-proxy ()  (eask--flag-value "--http-proxy"))   ; --http-proxy
(defun eask-https-proxy () (eask--flag-value "--https-proxy"))  ; --https-proxy
(defun eask-no-proxy ()    (eask--flag-value "--no-proxy"))     ; --no-proxy
(defun eask-destination () (eask--flag-value "--dest"))         ; --dest, --destination
(defalias 'eask-dest #'eask-destination)

;;; Number (with arguments)
(defun eask-depth () (eask--str2num (eask--flag-value "--depth")))       ; --depth
(defun eask-verbose () (eask--str2num (eask--flag-value "--verbose")))   ; -v, --verbose

(defun eask--handle-global-options ()
  "Handle global options."
  (when (eask-debug-p)        (setq debug-on-error t))
  (when (eask-verbose)        (setq eask-verbosity (eask-verbose)))
  (when (eask-insecure-p)     (setq network-security-level 'low))
  (when (eask-timestamps-p)   (setq eask-timestamps t))
  (when (eask-log-level-p)    (setq eask-log-level t))
  (when (eask-log-file-p)     (setq eask-log-file t))
  (when (eask-elapsed-time-p) (setq eask-elapsed-time t))
  (when (eask-no-color-p)     (setq ansi-inhibit-ansi t))
  (unless eask-has-colors     (setq ansi-inhibit-ansi t))
  (when (display-graphic-p)   (setq ansi-inhibit-ansi t))
  (eask--add-proxy "http"     (eask-proxy))
  (eask--add-proxy "https"    (eask-proxy))
  (eask--add-proxy "http"     (eask-http-proxy))
  (eask--add-proxy "https"    (eask-https-proxy))
  (eask--add-proxy "no_proxy" (eask-no-proxy)))

;;
;;; Proxy

(defun eask--add-proxy (protocal host)
  "Add proxy."
  (when host (push (cons protocal (eask-proxy)) url-proxy-services)))

;;
;;; Core

(defvar eask--first-init-p nil
  "Is non-nil if .eask does not exists; meaning users haven't called eask in the
current workspace.")

(defvar eask--initialized-p nil
  "Set to t once the environment setup has done; this is used when calling
other scripts internally.  See function `eask-call'.")

(defun eask--form-options (options)
  "Add --eask to all OPTIONS."
  (mapcar (lambda (elm) (concat "--eask" elm)) options))

(defconst eask--option-switches
  (eask--form-options
   '("-g" "-f" "--dev"
     "--debug" "--strict"
     "--allow-error"
     "--insecure"
     "--timestamps" "--log-level"
     "--log-file"
     "--elapsed-time"
     "--no-color"))
  "List of boolean type options.")

(defconst eask--option-args
  (eask--form-options
   '("--proxy" "--http-proxy" "--https-proxy" "--no-proxy"
     "--verbose" "--silent"
     "--depth" "--dest"))
  "List of arguments (number/string) type options.")

(defconst eask--command-list
  (append eask--option-switches eask--option-args)
  "List of commands to accept, so we can avoid unknown option error.")

(defun eask-self-command-p (arg)
  "Return non-nil if ARG is known internal command."
  (member arg eask--command-list))

(defun eask-argv (index) "Return argument value by INDEX." (elt eask-argv index))

(defun eask-args ()
  "Get all arguments except options."
  (let ((argv (cl-remove-if (lambda (arg) (member arg eask--option-switches)) eask-argv))
        (args) (skip-next))
    (dolist (arg argv)
      (if skip-next (setq skip-next nil)
        (if (member arg eask--option-args)
            (setq skip-next t)
          (push arg args))))
    (reverse args)))

(defmacro eask--batch-mode (&rest body)
  "Initialize for batch-mode"
  (declare (indent 0) (debug t))
  `(let ((argv (eask-args))
         load-file-name buffer-file-name)
     ,@body))

(defmacro eask--setup-env (&rest body)
  "Execute BODY with workspace setup."
  (declare (indent 0) (debug t))
  `(eask--batch-mode
     (let (;; XXX: this will make command `info', `files' work as expected;
           ;; but the relative paths file spec will be lost...
           ;;
           ;; So commands like `load' would NOT work!
           (default-directory (if (eask-global-p) user-emacs-directory
                                default-directory))
           (alist))
       (dolist (cmd eask--command-list)
         (push (cons cmd '(lambda (&rest _))) alist))
       (setq command-switch-alist (append command-switch-alist alist))
       ,@body)))

(defconst eask-file-keywords
  '("package" "website-url" "keywords"
    "package-file" "files"
    "depends-on" "development"
    "source" "source-priority"
    "exec-paths" "load-paths")
  "List of Eask file keywords.")

(defun eask--loop-file-keywords (func)
  "Loop through Eask file keywords for environment replacement.  Internal used
for function `eask--alias-env'."
  (dolist (keyword eask-file-keywords)
    (let ((keyword-sym (intern keyword))
          (api (intern (concat "eask-" keyword)))      ; existing function
          (old (intern (concat "eask--f-" keyword))))  ; variable that holds function pointer
      (funcall func keyword-sym api old))))

(defmacro eask--alias-env (&rest body)
  "Replace all Eask file functions temporary; this is only used when loading
Eask file in the workspace."
  (declare (indent 0) (debug t))
  `(let (result)
     ;; XXX magic here is we replace all keyword functions with `eask-xxx'...
     (eask--loop-file-keywords
      (lambda (keyword api old)
        (defalias old (symbol-function keyword))
        (defalias keyword (symbol-function api))))
     (setq result (progn ,@body))
     ;; XXX after loading Eask file, we revert those functions back to normal!
     (eask--loop-file-keywords
      (lambda (keyword api old)
        (defalias keyword (symbol-function old))))
     result))

(defvar eask-file nil "The Eask file's filename.")
(defvar eask-file-root nil "The Eask file's directory.")

(defun eask-root-del (filename)
  "Remove Eask file root path from FILENAME."
  (when (stringp filename) (s-replace eask-file-root "" filename)))

(defun eask-file-load (location &optional noerror)
  "Load Eask file in the LOCATION."
  (when-let* ((target-eask-file (expand-file-name location user-emacs-directory))
              (result (eask--alias-env (load target-eask-file noerror t))))
    (setq eask-file target-eask-file  ; assign eask file only if success
          eask-file-root (file-name-directory target-eask-file))
    (run-hooks 'eask-file-loaded-hook)
    result))

(defun eask-file-try-load (relative-path)
  "Try load eask file in RELATIVE-PATH."
  (or (eask-file-load (concat relative-path "Easkfile") t)
      (eask-file-load (concat relative-path "Eask") t)))

(defun eask--print-env-info ()
  "Display environment information at the very top of the execution."
  (eask-msg "")
  (eask-msg "✓ Checking Emacs version %s... done!" emacs-version)
  (eask-with-verbosity 'debug
    (eask-msg "  ✓ Checking build number %s... done!" emacs-build-number)
    (eask-msg "  ✓ Checking system configuration %s... done!" system-configuration)
    (when-let ((emacs-build-time)
               (time (format-time-string "%Y-%m-%d" emacs-build-time)))
      (eask-msg "  ✓ Checking build time %s... done!" time)))
  (eask-msg "✓ Checking system %s... done!" system-type))

(defmacro eask--with-hooks (&rest body)
  "Execute BODY with before/after hooks."
  (declare (indent 0) (debug t))
  `(progn
     (run-hooks 'eask-before-command-hook)
     (run-hooks (intern (concat "eask-before-" (eask-command) "-hook")))
     ,@body
     (run-hooks (intern (concat "eask-after-" (eask-command) "-hook")))
     (run-hooks 'eask-after-command-hook)))

(defmacro eask-start (&rest body)
  "Execute BODY with workspace setup."
  (declare (indent 0) (debug t))
  `(unless eask-loading-file-p
     (if eask--initialized-p (progn ,@body)
       (setq eask--initialized-p t)
       (eask--setup-env
         (eask--handle-global-options)
         (eask--print-env-info)
         (cond
          ((eask-global-p)
           ;; We accept Eask file in global scope, but it shouldn't be used
           ;; as a sandbox.
           (if (eask-file-try-load "./")
               (eask-msg "✓ Loading config Eask file in %s... done!" eask-file)
             (eask-msg "✗ Loading config Eask file... missing!"))
           (message "")
           (package-activate-all)
           (eask-with-progress
             (ansi-green "Loading your configuration... ")
             (eask-with-verbosity 'debug
               (load (locate-user-emacs-file "early-init.el") t)
               (load (locate-user-emacs-file "../.emacs") t)
               (load (locate-user-emacs-file "init.el") t))
             (ansi-green "done"))
           (eask--with-hooks ,@body))
          (t
           (let* ((user-emacs-directory (expand-file-name (concat ".eask/" emacs-version "/")))
                  (package-user-dir (expand-file-name "elpa" user-emacs-directory))
                  (eask--first-init-p (not (file-directory-p user-emacs-directory)))
                  (user-init-file (locate-user-emacs-file "init.el"))
                  (custom-file (locate-user-emacs-file "custom.el"))
                  (special (eask-special-p)))
             (if (or (eask-file-try-load "../../")
                     special)
                 (progn
                   (if eask-file
                       (eask-msg "✓ Loading Eask file in %s... done!" eask-file)
                     (eask-msg "✗ Loading Eask file... missing!"))
                   (message "")
                   (package-activate-all)
                   (unless special
                     (ignore-errors (make-directory package-user-dir t))
                     (eask--silent (eask-setup-paths)))
                   (eask--with-hooks ,@body))
               (eask-msg "✗ Loading Eask file... missing!")
               (eask-help 'init)))))))))

;;
;;; Eask file

(defun eask-network-insecure-p ()
  "Are we attempt to use insecure connection?"
  (eq network-security-level 'low))

(defconst eask-source-mapping
  `((gnu          . "https://elpa.gnu.org/packages/")
    (nongnu       . "https://elpa.nongnu.org/nongnu/")
    (celpa        . "https://celpa.conao3.com/packages/")
    (jcs-elpa     . "https://jcs-emacs.github.io/jcs-elpa/packages/")
    (marmalade    . "https://marmalade-repo.org/packages/")
    (melpa        . "https://melpa.org/packages/")
    (melpa-stable . "https://stable.melpa.org/packages/")
    (org          . "https://orgmode.org/elpa/")
    (shmelpa      . "https://shmelpa.commandlinesystems.com/packages/"))
  "Mapping of source name and url.")

(defvar eask-package          nil)
(defvar eask-package-desc     nil)  ; package descriptor
(defvar eask-website-url      nil)
(defvar eask-keywords         nil)
(defvar eask-package-file     nil)
(defvar eask-files            nil)
(defvar eask-depends-on-emacs nil)
(defvar eask-depends-on       nil)
(defvar eask-depends-on-dev   nil)

(defun eask-package--get (key)
  "Return package info by KEY."
  (plist-get eask-package key))

(defun eask-package-name ()        (eask-package--get :name))
(defun eask-package-version ()     (eask-package--get :version))
(defun eask-package-description () (eask-package--get :description))

(defun eask-depends-emacs-version ()
  "Get Eask-file Emacs version string."
  (nth 0 (cdar eask-depends-on-emacs)))

(defun eask-package (name version description)
  "Set the package information."
  (if eask-package
      (eask-error "Multiple definition of `package'")
    (setq eask-package `(:name ,name :version ,version :description ,description))
    (progn  ; Run checker
      (eask--checker-string "Name" name)
      (version= version "0.1.0")
      (eask--checker-string "Description" description))))

(defun eask-website-url (url)
  "Set website URL."
  (if eask-website-url
      (eask-error "Multiple definition of `website-url'")
    (setq eask-website-url url)))

(defun eask-keywords (&rest keywords)
  "Set package keywords."
  (if eask-keywords
      (eask-error "Multiple definition of `keywords'")
    (setq eask-keywords keywords)))

(defun eask-package-file (file)
  "Set package file."
  (if eask-package-file
      (eask-error "Multiple definition of `package-file'")
    (setq eask-package-file (expand-file-name file))
    (let* ((package-file-exists (file-exists-p eask-package-file))
           (def-point (if (eask-pkg-el) "-pkg.el file" "package-file"))
           (target-file (cond ((eask-pkg-el) (expand-file-name (eask-pkg-el)))
                              (package-file-exists eask-package-file))))
      (unless package-file-exists
        (eask-warn "Package-file seems to be missing `%s'" file))
      (when target-file
        (with-temp-buffer
          (insert-file-contents target-file)
          (setq eask-package-desc (ignore-errors
                                    (if (eask-pkg-el)
                                        (package--read-pkg-desc 'dir)
                                      (package-buffer-info)))))
        (eask-msg (concat
                   (if eask-package-desc "✓ " "✗ ")
                   "Try constructing the package-descriptor (%s)... "
                   (if eask-package-desc "succeeded! " "failed!"))
                  (file-name-nondirectory target-file))))))

(defun eask-files (&rest patterns)
  "Set files patterns."
  (setq eask-files (append eask-files patterns)))

(defun eask-source (name &optional location)
  "Add archive NAME with LOCATION."
  (when (assoc name package-archives)
    (eask-error "Multiple definition of source `%s'" name))
  (setq location (or location (cdr (assq (intern name) eask-source-mapping))))
  (unless location (eask-error "Unknown package archive `%s'" name))
  (when (and location
             (gnutls-available-p)
             (not (eask-network-insecure-p)))
    (setq location (s-replace "https://" "http://" location)))
  (add-to-list 'package-archives (cons name location) t))

(defun eask-source-priority (archive-id &optional priority)
  "Add PRIORITY for to ARCHIVE-ID."
  (add-to-list 'package-archive-priorities (cons archive-id priority) t))

(defvar eask-depends-on-recipe-p nil
  "Set to t if package depends on recipe.")

(defun eask--setup-dependencies ()
  "Setup dependencies list."
  (setq eask-depends-on (reverse eask-depends-on)
        eask-depends-on-dev (reverse eask-depends-on-dev))
  (when eask-depends-on-recipe-p
    (eask-with-progress
      "✓ Checking local archives... "
      (eask-with-verbosity 'debug
        (add-to-list 'package-archives `("local" . ,github-elpa-archive-dir) t)
        ;; If the local archives is added, we set the priority to a very
        ;; high number so user we always use the specified dependencies!
        (add-to-list 'package-archive-priorities `("local" . 90) t))
      "done!")))

(add-hook 'eask-file-loaded-hook #'eask--setup-dependencies)

(defun eask-depends-on (pkg &rest args)
  "Specify a dependency of this package."
  (cond
   ((string= pkg "emacs")
    (if eask-depends-on-emacs
        (eask-error "Define dependencies with the same name `%s'" pkg)
      (let* ((minimum-version (car args))
             (recipe (list pkg minimum-version)))
        (if (version< emacs-version minimum-version)
            (eask-error "This requires Emacs %s and above!" minimum-version)
          (push recipe eask-depends-on-emacs))
        recipe)))
   ;; No argument specify
   ((<= (length args) 1)
    (let* ((minimum-version (or (car args) "latest"))
           (recipe (list pkg minimum-version)))
      (if (member recipe eask-depends-on)
          (eask-error "Define dependencies with the same name `%s'" pkg)
        (push recipe eask-depends-on))
      recipe))
   ;; recipe are entered
   (t
    (let ((recipe (append (list (intern pkg)) args)))
      (if (member recipe eask-depends-on)
          (eask-error "Define dependencies with the same name `%s'" pkg)
        (push recipe eask-depends-on)
        (eask-load "extern/github-elpa")
        (eask-with-verbosity 'debug
          (eask-with-progress
            (ansi-blue (format "Generating recipe for package %s... " (ansi-yellow pkg)))
            (write-region (pp-to-string recipe) nil (expand-file-name pkg github-elpa-recipes-dir))
            (ansi-blue "done ✓")))
        (setq eask-depends-on-recipe-p t))
      recipe))))

(defun eask-development (&rest dep)
  "Development scope."
  (dolist (pkg dep)
    (push pkg eask-depends-on-dev)
    (delete-dups eask-depends-on-dev)
    (setq eask-depends-on (remove pkg eask-depends-on))))

(defun eask-load-paths (&rest dirs)
  "Add all DIRS to load-path."
  (dolist (dir dirs) (add-to-list 'load-path (expand-file-name dir) t)))

(defun eask-exec-paths (&rest dirs)
  "Add all DIRS to exec-path."
  (dolist (dir dirs) (add-to-list 'exec-path (expand-file-name dir) t)))

;;
;;; Error Handling

(defvar eask--ignore-error-p nil
  "Don't trigger error when this is non-nil.")

(defmacro eask-ignore-errors (&rest body)
  "Execute BODY but ignore all errors."
  (declare (indent 0) (debug t))
  `(let ((eask--ignore-error-p t)) ,@body))

(defun eask--exit (&rest _) "Send exit code." (kill-emacs 1))

(defun eask--trigger-error ()
  "Trigger error event."
  (when (and (not eask--ignore-error-p)
             (not (eask-checker-p)))  ; ignore when checking Eask-file
    (if (eask-allow-error-p)  ; Trigger error at the right time
        (add-hook 'eask-after-command-hook #'eask--exit)
      (eask--exit))))

(defun eask--error (fnc &rest args)
  "On error."
  (let ((msg (eask--ansi 'error (apply #'format-message args))))
    (eask--unsilent (eask-msg "%s" msg))
    (run-hook-with-args 'eask-on-error-hook 'error msg)
    (eask--trigger-error))
  (when debug-on-error (apply fnc args)))

(advice-add 'error :around #'eask--error)

(defun eask--warn (fnc &rest args)
  "On warn."
  (let ((msg (eask--ansi 'warn (apply #'format-message args))))
    (eask--unsilent (eask-msg "%s" msg))
    (run-hook-with-args 'eask-on-warning-hook 'warn msg))
  (eask--silent (apply fnc args)))

(advice-add 'warn :around #'eask--warn)

;;
;;; Verbosity

(defcustom eask-verbosity 3
  "Log level for all messages; 4 means trace most anything, 0 means nothing.

Standard is, 0 (error), 1 (warning), 2 (info), 3 (log), 4 or above (debug)."
  :type 'integer)

(defcustom eask-timestamps nil
  "Log messages with timestamps."
  :type 'boolean)

(defcustom eask-log-level nil
  "Log messages with level."
  :type 'boolean)

(defcustom eask-level-color
  '((debug . ansi-blue)
    (log   . ansi-white)
    (info  . ansi-cyan)
    (warn  . ansi-yellow)
    (error . ansi-red))
  "Alist of each log level's color, in (SYMBOL . ANSI-FUNCTION)."
  :type 'alist)

(defun eask--verb2lvl (symbol)
  "Convert verbosity SYMBOL to level."
  (cl-case symbol
    (`debug 4)
    (`log   3)
    (`info  2)
    (`warn  1)
    (`error 0)
    (t symbol)))

(defun eask--reach-verbosity-p (symbol)
  "Return t if SYMBOL reach verbosity (should be printed)."
  (>= eask-verbosity (eask--verb2lvl symbol)))

(defmacro eask-with-verbosity (symbol &rest body)
  "If LEVEL is above `eask-verbosity'; hide all messages in BODY."
  (declare (indent 1) (debug t))
  `(if (eask--reach-verbosity-p ,symbol) (progn ,@body)
     (eask--silent ,@body)))

(defun eask-debug (msg &rest args) (apply #'eask--msg 'debug "[DEBUG]"   msg args))
(defun eask-log   (msg &rest args) (apply #'eask--msg 'log   "[LOG]"     msg args))
(defun eask-info  (msg &rest args) (apply #'eask--msg 'info  "[INFO]"    msg args))
(defun eask-warn  (msg &rest args) (apply #'eask--msg 'warn  "[WARNING]" msg args))
(defun eask-error (msg &rest args) (apply #'eask--msg 'error "[ERROR]"   msg args))

(defun eask--ansi (symbol string)
  "Paint STRING with color defined by log level."
  (if-let ((ansi-function (cdr (assq symbol eask-level-color))))
      (funcall ansi-function string)
    string))

(defun eask--msg (symbol prefix msg &rest args)
  "If LEVEL is at or below `eask-verbosity', log message."
  (eask-with-verbosity symbol
    (let* ((string (apply #'eask--format prefix msg args))
           (output (eask--ansi symbol string))
           (func (cl-case symbol
                   ((or error warn) symbol)
                   (t #'message))))
      (funcall func "%s" output))))

(defun eask--format (prefix fmt &rest args)
  "Format Eask messages."
  (apply #'format
         (concat (when eask-timestamps (format-time-string "%Y-%m-%d %H:%M:%S "))
                 (when eask-log-level (concat prefix " "))
                 fmt)
         args))

(defun eask--msg-paint-kwds (string)
  "Paint keywords from STRING."
  (let* ((string (s-replace "✓" (ansi-green "✓") string))
         (string (s-replace "✗" (ansi-red "✗") string)))
    string))

(defun eask--format-paint-kwds (msg &rest args)
  "Paint keywords after format MSG and ARGS."
  (let* ((string (apply #'format msg args))
         (string (eask--msg-paint-kwds string)))
    string))

(defun eask-msg (msg &rest args)
  "Like function `message' but replace unicodes with color."
  (message (apply #'eask--format-paint-kwds msg args)))

(defun eask-write (msg &rest args)
  "Like function `eask-msg' but without newline at the end."
  (unless inhibit-message
    (princ (apply #'eask--format-paint-kwds msg args) 'external-debugging-output)))

(defun eask-report (&rest args)
  "Report error/warning depends on strict flag."
  (apply (if (eask-strict-p) #'eask-error #'eask-warn) args))

;;
;;; Log

(defconst eask-log-path ".log"
  "Directory path to create log files.")

(defcustom eask-log-file nil
  "Weather to generate log files."
  :type 'boolean)

(defmacro eask--log-write-buffer (buffer file)
  "Write BUFFER to FILE."
  `(when (get-buffer-create ,buffer)
     (let ((buffer-file-coding-system 'utf-8))
       (write-region (with-current-buffer ,buffer (buffer-string)) nil
                     (expand-file-name ,file log-dir)))))

(add-hook 'kill-emacs-hook  ; Write log files
          (lambda (&rest _)
            (when eask-log-file
              (let ((log-dir (expand-file-name eask-log-path eask-file-root)))
                (make-directory log-dir t)
                (eask--log-write-buffer "*Messages*" "messages.log")
                (eask--log-write-buffer "*Warnings*" "warnings.log")
                (eask--log-write-buffer "*Backtrace*" "backtrace.log")
                (eask--log-write-buffer "*Compile-Log*" "compile-log.log")))))

;;
;;; File

(defun eask-guess-package-name ()
  "Return the possible package name."
  (or (eask-package-name)
      (ignore-errors (file-name-nondirectory
                      (file-name-sans-extension eask-package-file)))))

(defun eask-files-spec ()
  "Return files spec."
  (or eask-files package-build-default-files-spec))

(defun eask-expand-file-specs (specs)
  "Expand file SPECS."
  (mapcar (lambda (elm) (expand-file-name (car elm) default-directory))
          (package-build-expand-file-specs default-directory specs nil t)))

(defun eask-package-files ()
  "Return package files in workspace."
  (let ((files (eask-expand-file-specs (eask-files-spec))))
    ;; Package file is part of package-files
    (when eask-package-file (push eask-package-file files))
    (delete-dups files)
    (setq files (cl-remove-if-not #'file-exists-p files))
    (unless files
      (eask-debug "No matching file(s) found in %s: %s" default-directory (eask-files-spec)))
    files))

(defun eask-package-el-files ()
  "Return package files in workspace."
  (cl-remove-if-not (lambda (filename) (string= (file-name-extension filename) "el")) (eask-package-files)))

(defun eask-package-elc-files ()
  "Return package files' elc in workspace."
  (when-let ((elcs (mapcar (lambda (elm) (concat elm "c")) (eask-package-el-files))))
    (setq elcs (cl-remove-if-not (lambda (elm) (file-exists-p elm)) elcs))
    elcs))

(defun eask-args-or-package-el-files ()
  "Return args if specified, else return package files by default."
  (if (eask-args)
      (eask-expand-file-specs (eask-args))
    (eask-package-el-files)))

(defun eask-package-multi-p ()
  "Return t if multi-files package."
  (< 1 (length (eask-package-files))))

(defun eask-package-single-p ()
  "Return t if single file package."
  (not (eask-package-multi-p)))

(defun eask-unpacked-size ()
  "Return unpacked size."
  (let ((size 0))
    (dolist (filename (eask-package-files))
      (cl-incf size (file-attribute-size (file-attributes filename))))
    (string-trim (ls-lisp-format-file-size size t))))

;;
;;; Progress

(defcustom eask-elapsed-time nil
  "Log with elapsed time."
  :type 'boolean)

(defcustom eask-minimum-reported-time 0.1
  "Minimal load time that will be reported."
  :type 'number)

(defmacro eask-with-progress (msg-start body msg-end)
  "Progress BODY wrapper with prefix and suffix messages."
  (declare (indent 0) (debug t))
  `(if eask-elapsed-time
       (let ((now (current-time)))
         (ignore-errors (eask-write ,msg-start)) ,body
         (let ((elapsed (float-time (time-subtract (current-time) now))))
           (if (< elapsed eask-minimum-reported-time)
               (ignore-errors (eask-msg ,msg-end))
             (ignore-errors (eask-write ,msg-end))
             (eask-msg (ansi-white (format " (%.3fs)" elapsed))))))
     (ignore-errors (eask-write ,msg-start)) ,body
     (ignore-errors (eask-msg ,msg-end))))

(defun eask-progress-seq (prefix sequence suffix func)
  "Progress SEQUENCE with messages."
  (let* ((total (length sequence)) (count 0)
         (offset (eask-2str (length (eask-2str total)))))
    (mapc
     (lambda (item)
       (cl-incf count)
       (eask-with-progress
         (format (concat "%s [%" offset "d/%d] %s... ") prefix count total
                 (ansi-green item))
         (when func (funcall func item))
         suffix))
     sequence)))

(defun eask-print-log-buffer (&optional buffer-or-name)
  "Loop through each line and print each line with corresponds log level."
  (with-current-buffer (or buffer-or-name (current-buffer))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (cond ((string-match-p "[: ][Ee]rror: " line) (eask-error line))
              ((string-match-p "[: ][Ww]arning: " line) (eask-warn line))
              (t (eask-log line))))
      (forward-line 1))))

;;
;;; Help

(defun eask-help (command)
  "Show help."
  (let* ((command (eask-2str command))  ; convert to string
         (help-file (concat eask-lisp-root "help/" command)))
    (if (file-exists-p help-file)
        (with-temp-buffer
          (insert-file-contents help-file)
          (unless (string= (buffer-string) "")
            (eask-msg (ansi-white (buffer-string)))))
      (eask-error "Help manual missig %s" help-file))))

(defun eask--print-no-matching-files ()
  "Print message for no matching files found."
  (eask-log "")
  (eask-log "Cannot find matching files with given pattern %s" (eask-args))
  (eask-log ""))

;;
;;; Checker

(defun eask--checker-existence ()
  "Return errors if required metadata is missing."
  (unless eask-package (eask-error "Missing metadata package; make sure you have create Eask-file with $ eask init!")))

(defun eask--check-strings (fmt f p &rest args)
  "Test strings (F and P); then print FMT if not equal."
  (unless (string= f p) (apply #'eask-warn (append (list fmt f p) args))))

(defun eask--check-optional (f p msg1 msg2 msg3 msg4)
  "Conditional way to check optional headers, URL and KEYWORDS ."
  (cond ((and f p) (eask--check-strings msg1 f p))
        (f (eask-warn msg2))
        (p (eask-warn msg3))
        (t (eask-warn msg4))))

(defun eask--checker-metadata ()
  "Report warnings if metadata doesn't match."
  (when-let* (((and eask-package eask-package-desc))
              (def-point (if (eask-pkg-el) "-pkg.el file" "package-file")))
    (eask--check-strings
     "Unmatched package name '%s'; it should be '%s'"
     (eask-package-name) (package-desc-name eask-package-desc))
    (when-let* ((ver-eask (eask-package-version))
                (ver-pkg (package-desc-version eask-package-desc))
                ;; `package-version-join' returns only one of the possible
                ;; inverses, since `version-to-list' is a many-to-one operation
                ((not (equal (version-to-list ver-eask) ver-pkg))))
      (eask--check-strings
       "Unmatched version '%s'; it should be '%s'"
       ver-eask (package-version-join ver-pkg)))
    (eask--check-strings
     "Unmatched summary '%s'; it should be '%s'"
     (eask-package-description) (package-desc-summary eask-package-desc))
    (let ((url (eask-package-desc-url)))
      (eask--check-optional
       eask-website-url url
       "Unmatched website URL '%s'; it should be '%s'"
       (format "Unmatched website URL '%s'; add ;; URL: %s to %s" eask-website-url eask-website-url def-point)
       (format "Unmatched website URL '%s'; add (website-url \"%s\") to Eask-file" url url)
       (format "URL header is optional, but it's often recommended")))
    (let ((keywords (eask-package-desc-keywords)))
      (cond
       ((or keywords eask-keywords)
        (dolist (keyword keywords)
          (unless (member keyword eask-keywords)
            (eask-warn "Unmatched keyword '%s'; add (keywords ... \"%s\") to Eask-file or consider removing it" keyword keyword)))
        (dolist (keyword eask-keywords)
          (unless (member keyword keywords)
            (eask-warn "Unmatched keyword '%s'; add ;; Keywords ... %s to %s or consider removing it" keyword keyword def-point))))
       (t
        (eask-warn "Keywords header is optional, but it's often recommended"))))
    (let* ((dependencies (append eask-depends-on-emacs eask-depends-on))
           (dependencies (mapcar #'car dependencies))
           (dependencies (mapcar (lambda (elm) (eask-2str elm)) dependencies))
           (requirements (package-desc-reqs eask-package-desc))
           (requirements (mapcar #'car requirements))
           (requirements (mapcar (lambda (elm) (eask-2str elm)) requirements)))
      (dolist (req requirements)
        (unless (member req dependencies)
          (eask-warn "Unmatched dependency '%s'; add (depends-on \"%s\") to Eask-file or consider removing it" req req)))
      (dolist (dep dependencies)
        (unless (member dep requirements)
          (eask-warn "Unmatched dependency '%s'; add (%s \"VERSION\") to %s or consider removing it" dep dep def-point))))))

(add-hook 'eask-file-loaded-hook #'eask--checker-existence)
(add-hook 'eask-file-loaded-hook #'eask--checker-metadata)

(defun eask--checker-string (name var)
  "Run checker for VAR."
  (unless (stringp var)
    (eask-error "%s must be a string" name))
  (when (string-empty-p var)
    (eask-warn "%s cannot be an empty string" name)))

;;
;;; User customization

(defcustom eask-file-loaded-hook nil
  "Hook runs after Easkfile is loaded."
  :type 'hook)

(defcustom eask-before-command-hook nil
  "Hook runs before command is executed."
  :type 'hook)

(defcustom eask-after-command-hook nil
  "Hook runs after command is executed."
  :type 'hook)

(defcustom eask-on-error-hook nil
  "Hook runs when error is triggered."
  :type 'hook)

(defcustom eask-on-warning-hook nil
  "Hook runs when warning is triggered."
  :type 'hook)

(defcustom eask-dist-path "dist"
  "Name of default target directory for building packages."
  :type 'string)

;;
;;; Externals

(eask-load "extern/ansi")
(with-eval-after-load 'ansi (eask-load "extern/ansi"))  ; override
(eask-load "extern/package")
(eask-load "extern/package-build")
(eask-load "extern/s")

;;
;;; Requirement

(when (version< emacs-version "26.1")
  (eask-error "Eask requires Emacs 26.1 and above!"))

;;; _prepare.el ends here

(provide 'eask-api)
;;; eask-api.el ends here
