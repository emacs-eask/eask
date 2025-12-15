;;; eask-core.el --- Core Eask APIs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Shen, Jen-Chieh

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

;; ~/lisp/_prepare.el
(defvar ansi-inhibit-ansi)
(defvar github-elpa-archive-dir)
(defvar github-elpa-recipes-dir)
(defvar package-build-default-files-spec)
(declare-function package-build-expand-files-spec "ext:package-build.el")
(declare-function github-elpa-build "ext:github-elpa.el")
(declare-function ansi-red "ext:ansi.el")
(declare-function ansi-blue "ext:ansi.el")
(declare-function ansi-green "ext:ansi.el")
(declare-function ansi-yellow "ext:ansi.el")
(declare-function ansi-white "ext:ansi.el")

(defvar eask-dot-emacs-file nil
  "Variable hold .emacs file location.")

(defcustom eask-import-timeout 10
  "Number of seconds before timing out elisp importation attempts.
If nil, never time out."
  :type '(choice (number :tag "Number of seconds")
                 (const  :tag "Never time out" nil))
  :group 'eask)

(defvar eask-loading-file-p nil
  "This became t; if we are loading script from another file and not expecting
the `eask-start' execution.")

(defmacro eask-defvc< (version &rest body)
  "Define scope if Emacs version is below VERSION.

Argument BODY are forms for execution."
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

(defmacro eask-with-buffer (&rest body)
  "Create a temporary buffer (for this program), and evaluate BODY there."
  (declare (indent 0) (debug t))
  `(with-current-buffer (get-buffer-create ,eask-buffer-name) ,@body))

(defmacro eask-with-temp-buffer (&rest body)
  "Create a temporary buffer (for this program), and evaluate BODY there."
  (declare (indent 0) (debug t))
  `(eask-with-buffer (erase-buffer) ,@body))

(defmacro eask--with-no-color (&rest body)
  "Execute forms BODY in when no color output."
  (declare (indent 0) (debug t))
  `(let ((ansi-inhibit-ansi t)) ,@body))

(defcustom eask-elapsed-time nil
  "Log with elapsed time."
  :type 'boolean
  :group 'eask)

(defcustom eask-minimum-reported-time 0.1
  "Minimal load time that will be reported."
  :type 'number
  :group 'eask)

(defmacro eask-with-progress (msg-start body msg-end)
  "Progress BODY wrapper with prefix (MSG-START) and suffix (MSG-END) messages."
  (declare (indent 0) (debug t))
  `(if eask-elapsed-time
       (let ((now (current-time)))
         (ignore-errors (eask-write ,msg-start)) ,body
         (let ((elapsed (float-time (time-subtract (current-time) now))))
           (if (< elapsed eask-minimum-reported-time)
               (ignore-errors (eask-msg ,msg-end))
             (ignore-errors (eask-write ,msg-end))
             (eask-msg " (%.3fs)" elapsed))))
     (ignore-errors (eask-write ,msg-start)) ,body
     (ignore-errors (eask-msg ,msg-end))))

(defvar eask--action-prefix ""
  "The prefix to display before each package action.")

(defvar eask--action-index 0
  "The index ID for each task.")

(defvar eask--package-initialized nil
  "Flag for package initialization in global scope.")

(defmacro eask--pkg-process (pkg &rest body)
  "Execute BODY with PKG's related variables."
  (declare (indent 1) (debug t))
  `(let* ((pkg-info           (eask--pkg-transaction-vars ,pkg))
          (pkg                (nth 0 pkg-info))
          (name               (nth 1 pkg-info))
          (version            (nth 2 pkg-info))
          (installed-p        (package-installed-p pkg))
          (should-reinstall-p (and installed-p (eask-force-p))))
     ,@body))

(defmacro eask-with-archives (archives &rest body)
  "Scope that temporary make ARCHIVES available.

Argument BODY are forms for execution."
  (declare (indent 1) (debug t))
  `(let ((package-archives package-archives)
         (archives (eask-listify ,archives))
         (package-user-dir eask-package-sys-dir)  ; Install as global packages.
         (added))
     (dolist (archive archives)
       (unless (assoc archive package-archives)
         (setq added t)
         (eask-with-progress
           (format "Adding required archives (%s)... " (ansi-yellow archive))
           (eask-f-source archive)
           "done ✓")))
     (when added
       (eask-with-progress
         "Refresh archives information... "
         (eask--silent (eask-pkg-init t))
         "done ✓"))
     ,@body))

(defvar eask--first-init-p nil
  "Is non-nil if .eask does not exists; meaning users haven't called eask in the
current workspace.")

(defvar eask--initialized-p nil
  "Set to t once the environment setup has done; this is used when calling
other scripts internally.  See function `eask-call'.")

(defmacro eask--batch-mode (&rest body)
  "Execute forms BODY in batch-mode."
  (declare (indent 0) (debug t))
  `(let ((argv (eask-args))
         load-file-name buffer-file-name)
     ,@body))

(defmacro eask--setup-env (&rest body)
  "Execute BODY with workspace setup."
  (declare (indent 0) (debug t))
  `(eask--batch-mode
     (let ((alist))
       (dolist (cmd eask--command-list)
         (push (cons cmd (lambda (&rest _))) alist))
       (setq command-switch-alist (append command-switch-alist alist))
       ,@body)))

(defmacro eask--alias-env (&rest body)
  "Replace all Eask file functions temporary; this is only used when loading
Eask file in the workspace.

Argument BODY are forms for execution."
  (declare (indent 0) (debug t))
  `(let (result)
     ;; XXX: Magic here is we replace all keyword functions with `eask-xxx'...
     (eask--loop-file-keywords
      (lambda (keyword api old)
        (defalias old (symbol-function keyword))
        (defalias keyword (symbol-function api))))
     (setq result (progn ,@body))
     ;; XXX: after loading Eask file, we revert those functions back to normal!
     (eask--loop-file-keywords
      (lambda (keyword _api old)
        (defalias keyword (symbol-function old))))
     result))

(defvar eask-file nil "The Eask file's filename.")
(defvar eask-file-root nil "The Eask file's directory.")

(defmacro eask--with-hooks (&rest body)
  "Execute BODY with before/after hooks."
  (declare (indent 0) (debug t))
  `(let* ((command (eask-command))
          (before  (concat "eask-before-" command "-hook"))
          (after   (concat "eask-after-" command "-hook")))
     (eask--unsilent
       (run-hooks 'eask-before-command-hook)
       (run-hooks (intern before))
       ,@body
       (run-hooks (intern after))
       (run-hooks 'eask-after-command-hook))))

(defmacro eask--setup-home (dir &rest body)
  "Set up config directory in DIR, then execute BODY."
  (declare (indent 1) (debug t))
  `(let* ((user-emacs-directory   (expand-file-name (concat ".eask/" emacs-version "/") ,dir))
          (package-user-dir       (expand-file-name "elpa/" user-emacs-directory))
          ;; Add global scope elpa directory.
          (package-directory-list (append package-directory-list
                                          (list eask-package-sys-dir)))
          (early-init-file        (locate-user-emacs-file "early-init.el"))
          (eask-dot-emacs-file    (locate-user-emacs-file ".emacs"))
          (user-init-file         (locate-user-emacs-file "init.el"))
          (custom-file            (locate-user-emacs-file "custom.el")))
     ,@body))

(defmacro eask-start (&rest body)
  "Execute BODY with workspace setup."
  (declare (indent 0) (debug t))
  `(unless eask-loading-file-p
     (if eask--initialized-p (progn ,@body)
       (setq eask--initialized-p t)
       (eask--setup-env
         (eask--handle-global-options)
         (cond
          ((eask-config-p)
           (let ((early-init-file     (locate-user-emacs-file "early-init.el"))
                 (eask-dot-emacs-file (locate-user-emacs-file "../.emacs"))
                 (user-init-file      (locate-user-emacs-file "init.el")))
             ;; We accept Eask-file in `config' scope, but it shouldn't be used
             ;; for the sandbox.
             (eask-with-verbosity 'debug
               (if (eask-file-try-load user-emacs-directory)
                   (eask-msg "✓ Loading config Eask file in %s... done!" eask-file)
                 (eask-msg "✗ Loading config Eask file... missing!"))
               (eask-msg ""))
             (package-activate-all)
             (eask--load-config)
             (eask--with-hooks ,@body)))
          ((eask-global-p)
           (eask--setup-home eask-userdir
             (let ((eask--first-init-p (not (file-directory-p user-emacs-directory))))
               ;; We accept Eask-file in `global' scope, but it shouldn't be used
               ;; for the sandbox.
               (eask-with-verbosity 'debug
                 (eask-ignore-errors  ; Eask-file is optional!
                   (if (eask-file-try-load eask-homedir)
                       (eask-msg "✓ Loading global Eask file in %s... done!" eask-file)
                     (eask-msg "✗ Loading global Eask file... missing!")))
                 (eask-msg ""))
               (package-activate-all)
               (ignore-errors (make-directory package-user-dir t))
               (eask-with-verbosity 'debug (eask--load-config))
               (eask--with-hooks ,@body))))
          ((eask-special-p)  ; Commands without Eask-file needed!
           ;; First, try to find a valid Eask-file!
           (eask-file-try-load default-directory)
           ;; Then setup the user directory according to the Eask-file!
           (eask--setup-home (or eask-file-root
                                 eask-userdir)
             (let ((eask--first-init-p (not (file-directory-p user-emacs-directory)))
                   (scope (if eask-file-root "" "global ")))
               (eask-with-verbosity 'debug
                 (eask-ignore-errors  ; Again, without Eask-file needed!
                   (if (or eask-file-root
                           (eask-file-try-load eask-homedir))
                       (eask-msg "✓ Loading %sEask file in %s... done!" scope eask-file)
                     (eask-msg "✗ Loading %sEask file... missing!" scope)))
                 (eask-msg ""))
               (package-activate-all)
               (ignore-errors (make-directory package-user-dir t))
               (eask-with-verbosity 'debug (eask--load-config))
               (eask--with-hooks ,@body))))
          (t
           (eask--setup-home nil  ; `nil' is the `default-directory'
             (let ((eask--first-init-p (not (file-directory-p user-emacs-directory))))
               (eask-with-verbosity 'debug
                 (if (eask-file-try-load default-directory)
                     (eask-msg "✓ Loading Eask file in %s... done!" eask-file)
                   (eask-msg "✗ Loading Eask file... missing!"))
                 (eask-msg ""))
               (if (not eask-file)
                   (eask-help "core/init")
                 (package-activate-all)
                 (ignore-errors (make-directory package-user-dir t))
                 (eask--silent (eask-setup-paths))
                 (eask-with-verbosity 'debug (eask--load-config))
                 (eask--with-hooks ,@body))))))
         ;; Report exit stats if any.
         (eask--resolve-exit-status)))))

(defvar eask-package            nil)
(defvar eask-package-desc       nil) 
(defvar eask-package-descriptor nil)
(defvar eask-website-url        nil)
(defvar eask-keywords           nil)
(defvar eask-authors            nil)
(defvar eask-licenses           nil)
(defvar eask-package-file       nil)
(defvar eask-files              nil)
(defvar eask-scripts            nil)
(defvar eask-depends-on-emacs   nil)
(defvar eask-depends-on         nil)
(defvar eask-depends-on-dev     nil)

(defmacro eask--save-eask-file-state (&rest body)
  "Execute BODY without touching the Eask-file global variables."
  (declare (indent 0) (debug t))
  `(let (package-archives
         package-archive-priorities
         eask-file
         eask-file-root
         eask-package
         eask-package-desc
         eask-website-url
         eask-keywords
         eask-authors
         eask-licenses
         eask-package-file
         eask-package-descriptor
         eask-files
         eask-scripts
         eask-depends-on-emacs
         eask-depends-on
         eask-depends-on-dev)
     ,@body))

(defmacro eask--save-load-eask-file (file success &rest error)
  "Load an Eask FILE and execute forms SUCCESS or ERROR."
  (declare (indent 2) (debug t))
  `(eask--save-eask-file-state
     (eask--setup-env
       (eask--alias-env
         (if (let ((default-directory (file-name-directory ,file)))
               (ignore-errors (eask-file-load ,file)))
             (progn ,success)
           ,@error)))))

(defvar eask-depends-on-recipe-p nil
  "Set to t if package depends on recipe.")

(defvar eask--local-archive-name "local"
  "The local archive name.")

(defcustom eask-verbosity 3
  "Log level for all messages; 4 means trace most anything, 0 means nothing.

Standard is, 0 (error), 1 (warning), 2 (info), 3 (log), 4 (debug), 5 (all)."
  :type 'integer
  :group 'eask)

(defcustom eask-timestamps nil
  "Log messages with timestamps."
  :type 'boolean
  :group 'eask)

(defcustom eask-log-level nil
  "Log messages with level."
  :type 'boolean
  :group 'eask)

(defcustom eask-level-color
  '((all   . ansi-magenta)
    (debug . ansi-blue)
    (log   . ansi-white)
    (info  . ansi-cyan)
    (warn  . ansi-yellow)
    (error . ansi-red))
  "Alist of each log level's color, in (SYMBOL . ANSI-FUNCTION)."
  :type 'alist
  :group 'eask)

(defmacro eask-with-verbosity (symbol &rest body)
  "Define verbosity scope.

Execute forms BODY limit by the verbosity level (SYMBOL)."
  (declare (indent 1) (debug t))
  `(if (eask-reach-verbosity-p ,symbol) (progn ,@body)
     (eask--silent ,@body)))

(defmacro eask-with-verbosity-override (symbol &rest body)
  "Define override verbosity scope.

Execute forms BODY limit by the verbosity level (SYMBOL)."
  (declare (indent 1) (debug t))
  `(if (eask-reach-verbosity-p ,symbol) (eask--unsilent ,@body)
     (eask--silent ,@body)))

(defvar eask--ignore-error-p nil
  "Don't trigger error when this is non-nil.")

(defvar eask-inhibit-error-message nil
  "Non-nil to stop error/warning message.")

(defvar eask--has-error-p nil
  "Non-nil if an error has occurred.")

(defvar eask--has-warn-p nil
  "Non-nil if a warning has occurred.")

(defmacro eask-ignore-errors (&rest body)
  "Execute BODY without killing the process."
  (declare (indent 0) (debug t))
  `(let ((eask--ignore-error-p t)) ,@body))

(defmacro eask--silent-error (&rest body)
  "Execute BODY and inhibit all error messages."
  (declare (indent 0) (debug t))
  `(let ((eask-inhibit-error-message t)) ,@body))

(defmacro eask-ignore-errors-silent (&rest body)
  "Execute BODY by completely ignore errors."
  (declare (indent 0) (debug t))
  `(eask-ignore-errors (eask--silent-error ,@body)))

(defcustom eask-log-file nil
  "Weather to generate log files."
  :type 'boolean
  :group 'eask)

(defmacro eask--log-write-buffer (buffer file)
  "Write BUFFER to FILE."
  `(when (get-buffer-create ,buffer)
     (let ((buffer-file-coding-system 'utf-8))
       (write-region (with-current-buffer ,buffer (buffer-string)) nil
                     (expand-file-name ,file log-dir)))))

(defcustom eask-file-loaded-hook nil
  "Hook runs after Easkfile is loaded."
  :type 'hook
  :group 'eask)

(defcustom eask-before-command-hook nil
  "Hook runs before command is executed."
  :type 'hook
  :group 'eask)

(defcustom eask-after-command-hook nil
  "Hook runs after command is executed."
  :type 'hook
  :group 'eask)

(defcustom eask-on-error-hook nil
  "Hook runs when error is triggered."
  :type 'hook
  :group 'eask)

(defcustom eask-on-warning-hook nil
  "Hook runs when warning is triggered."
  :type 'hook
  :group 'eask)

(defcustom eask-dist-path "dist"
  "Default path where to place the package artifact."
  :type 'string
  :group 'eask)

(defcustom eask-docs-path "docs/public/"
  "Default path where to place the documentation."
  :type 'string
  :group 'eask)

(defcustom eask-recipe-path "recipes"
  "Name of default target directory for placing recipes."
  :type 'string
  :group 'eask)

(defvar eask-lint-first-file-p nil
  "Set the flag to t after the first file is linted.")

(defvar eask-commands nil
  "List of defined commands.")

(defmacro eask-defcommand (name &rest body)
  "Define an Eask command."
  (declare (doc-string 2) (indent 1))
  (or name (error "Cannot define `%s` as a command" name))
  (push name eask-commands)
  (setq eask-commands (delete-dups eask-commands))
  `(defun ,name nil ,@body))

(defconst eask-required-emacs-version "26.1"
  "The minimum Emacs version required to run Eask.")

(require 'ansi-color nil t)
(require 'lisp-mnt nil t)
(require 'package nil t)
(require 'project nil t)
(require 'json nil t)
(require 'nsm nil t)
(require 'url-vars nil t)
(require 'cl-lib nil t)
(require 'ffap nil t)
(require 'files nil t)
(require 'ls-lisp nil t)
(require 'pp nil t)
(require 'rect nil t)
(require 'subr-x nil t)

(defconst eask-is-windows (memq system-type '(cygwin windows-nt ms-dos))
  "The system is Windows.")

(defconst eask-is-mac     (eq system-type 'darwin)
  "The system is macOS.")

(defconst eask-is-linux   (eq system-type 'gnu/linux)
  "The system is GNU Linux.")

(defconst eask-is-bsd     (or eask-is-mac (eq system-type 'berkeley-unix))
  "The system is BSD.")

(defconst eask-system-type
  (cond (eask-is-windows 'dos)
        (eask-is-bsd     'mac)
        (eask-is-linux   'unix)
        (t               'unknown))
  "Return current OS type.")

(defun eask--load--adv (fnc &rest args)
  "Prevent `_prepare.el' loading twice.

Arguments FNC and ARGS are used for advice `:around'."
  (unless (string= (nth 0 args) (eask-script "_prepare")) (apply fnc args)))

(defconst eask-has-colors (getenv "EASK_HASCOLORS")
  "Return non-nil if terminal supports colors.")

(defconst eask-homedir (getenv "EASK_HOMEDIR")
  "Eask's home directory path.

It points to the global home directory `~/.eask/'.")

(defconst eask-userdir (expand-file-name "../" eask-homedir)
  "Eask's user directory path.

It points to the global user directory `~/'.")

(defconst eask-package-sys-dir (expand-file-name (concat emacs-version "/elpa/")
                                                 eask-homedir)
  "Eask global elpa directory; it will be treated as the system-wide packages.

It points to the global elpa directory `~/.eask/XX.X/elpa/'.")

(defconst eask-invocation (getenv "EASK_INVOCATION")
  "Eask's invocation program path.")

(defconst eask-is-pkg (getenv "EASK_IS_PKG")
  "Return non-nil if Eask is packaged.")

(defconst eask-rest
  (let ((args (getenv "EASK_REST_ARGS")))
    (setq args (ignore-errors (split-string args ",")))
    args)
  "Eask's arguments after command separator `--'; return a list.

If the argument is `-- arg0 arg1'; it will return `(arg0 arg1)'.")

(defun eask-rest ()
  "Eask's arguments after command separator `--'; return a string.

If the argument is `-- arg0 arg1'; it will return `arg0 arg1'."
  (mapconcat #'identity eask-rest " "))

(defconst eask-argv argv
  "This stores the real argv; the argv will soon be replaced with `(eask-args)'.")

(defconst eask--script (nth 1 (or (member "-scriptload" command-line-args)
                                  (member "-l" command-line-args)))
  "Script currently executing.")

(defconst eask-lisp-root
  (let* ((script (ignore-errors (file-name-directory eask--script)))
         (dir (ignore-errors (expand-file-name (concat script "../"))))
         (basename (file-name-nondirectory (directory-file-name dir)))
         (root (expand-file-name "/")))
    (while (and (not (string= root dir))
                (not (string= basename "lisp")))
      (setq dir (expand-file-name (concat dir "../"))
            basename (file-name-nondirectory (directory-file-name dir))))
    dir)
  "Source `lisp' directory; should always end with slash.")

(defun eask-command ()
  "What's the current command?

If the command is with subcommand, it will return command with concatenate with
slash separator.  For example, the following:

   $ eask lint checkdoc [FILES..]

will return `lint/checkdoc' with a dash between two subcommands."
  (let* ((script-dir (file-name-directory eask--script))
         (script-file (file-name-sans-extension (file-name-nondirectory eask--script)))
         (module-name (eask-s-replace eask-lisp-root "" script-dir))
         (module-names (split-string module-name "/" t)))
    ;; Make certain commands the root command; e.g. `core', `checker', etc.
    (if (member (nth 0 module-names) '("core" "checker")) script-file
      (mapconcat #'identity (append module-names
                                    (list script-file))
                 "/"))))

(defun eask-command-check (version)
  "Report error if the current command requires minimum VERSION."
  (when (version< emacs-version version)
    (eask-error "The command `%s' requires Emacs %s and above!"
                (eask-s-replace "/" " " (eask-command))  ; Pretty print.
                version)))

(defun eask-command-p (commands)
  "Return t if COMMANDS is the current command."
  (member (eask-command) (eask-listify commands)))

(defun eask-special-p ()
  "Return t if the command that can be run without Eask-file existence.

These commands will first respect the current workspace.  If the current
workspace has no valid Eask-file; it will load global workspace instead."
  (eask-command-p '("init" "init/source" "init/cask" "init/eldev" "init/keg"
                    "create/package" "create/elpa" "create/el-project"
                    "bump" "cat" "keywords" "repl"
                    "generate/ignore" "generate/license"
                    "test/melpazoid")))

(defun eask-execution-p ()
  "Return t if the command is the execution command.

This is added because we don't want to pollute `error' and `warn' functions."
  (eask-command-p '("load" "exec" "emacs" "eval" "repl"
                    "run/script" "run/command"
                    ;; NOTE: These test commands handle the exit code themselves;
                    ;; therefore, we don't need to handle it for them!
                    "test/ert" "test/ert-runner")))

(defun eask-checker-p ()
  "Return t if running Eask as the checker.

Without this flag, the process will be terminated once the error is occurred.
This flag allows you to run through operations without reporting errors."
  (eask-command-p '("analyze")))

(defun eask-script (script)
  "Return full SCRIPT filename."
  (concat eask-lisp-root script ".el"))

(defun eask-load (script)
  "Load another eask SCRIPT; so we can reuse functions across all scripts."
  (let ((eask-loading-file-p t)) (eask-call script)))

(defun eask-call (script)
  "Call another eask SCRIPT."
  (if-let* ((script-file (eask-script script))
            ((file-exists-p script-file)))
      (load script-file nil t)
    (eask-error "Script missing %s" script-file)))

(defun eask-import (url)
  "Load and eval the script from a URL."
  (with-current-buffer (url-retrieve-synchronously url t nil eask-import-timeout)
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (forward-char)
    (delete-region (point-min) (point))
    (eval-buffer)))

(defun eask-always (&rest _arguments)
  "The function `always' is supported after Emacs 28.1."
  t)

(defun eask-2str (obj)
  "Convert OBJ to string."
  (format "%s" obj))

(defun eask-2url (url)
  "Convert secure/insecure URL."
  (if (and url
           (gnutls-available-p)
           (eask-network-insecure-p))
      (eask-s-replace "https://" "http://" url)
    url))

(defun eask-listify (obj)
  "Turn OBJ to list."
  (if (listp obj) obj (list obj)))

(defun eask-intern (obj)
  "Safely intern OBJ."
  (if (stringp obj) (intern obj) obj))

(defun eask--sinr (len-or-list form-1 form-2)
  "If LEN-OR-LIST has length of 1; return FORM-1, else FORM-2."
  (let ((len (if (numberp len-or-list) len-or-list (length len-or-list))))
    (if (<= len 1) form-1 form-2)))

(defun eask-current-time ()
  "Return current time."
  (let ((now (current-time))) (logior (ash (car now) 16) (cadr now))))

(defun eask-seq-str-max (sequence)
  "Return max length in SEQUENCE of strings."
  (let ((result 0))
    (mapc (lambda (elm) (setq result (max result (length (eask-2str elm))))) sequence)
    result))

(defun eask-s-replace (old new s)
  "Replace OLD with NEW in S each time it occurs."
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun eask-f-filename (path)
  "Return the name of PATH."
  (file-name-nondirectory (directory-file-name path)))

(defun eask-directory-empty-p (dir)
  "Return t if DIR names an existing directory containing no other files.

The function `directory-empty-p' only exists 28.1 or above; copied it."
  (if (fboundp #'directory-empty-p)
      (directory-empty-p dir)
    (and (file-directory-p dir)
         ;; XXX: Do not pass in the 5th argument COUNT; it doesn't compatbile to
         ;; 27.2 or lower!
         (null (directory-files dir nil directory-files-no-dot-files-regexp t)))))

(defun eask--guess-package-name (basename)
  "Convert the BASENAME to a valid, commonly seen package name."
  (when-let* ((name (ignore-errors (downcase basename))))
    (setq name (eask-s-replace "emacs-" "" name)
          name (eask-s-replace "-emacs" "" name)
          name (replace-regexp-in-string "[.-]el$" "" name))
    name))

(defun eask-guess-package-name (&optional basename)
  "Return the possible package name.

Optional argument BASENAME accepts a string; it will convert the string to a
valid, commonly seen package name."
  (or (eask--guess-package-name basename)
      (eask-package-name)
      (eask--guess-package-name
       (ignore-errors (file-name-nondirectory
                       (file-name-sans-extension eask-package-file))))))

(defun eask-guess-entry-point (&optional basename)
  "Return the guess entry point by its BASENAME."
  (let ((name (eask-guess-package-name basename)))
    (format "%s.el" name)))

(defun eask-read-string (prompt &optional
                                initial-input
                                history
                                default-value
                                inherit-input-method)
  "Wrapper for function `read-string'.

Argument PROMPT and all optional arguments INITIAL-INPUT, HISTORY, DEFAULT-VALUE
and INHERIT-INPUT-METHOD see function `read-string' for more information."
  (let ((str (read-string prompt initial-input history default-value inherit-input-method)))
    (eask-s-replace "\"" "" str)))

(defun eask--goto-line (line)
  "Go to LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun eask--column-at-point (point)
  "Get column at POINT."
  (save-excursion (goto-char point) (current-column)))

(defconst eask-buffer-name "*eask*"
  "Buffer name is used for temporary storage throughout the life cycle.")

(defun eask-re-seq (regexp string)
  "Get a list of all REGEXP matches in a STRING."
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      (reverse matches))))

(defun eask-ansi-codes (s)
  "Return a list of ansi codes from S."
  (eask-re-seq ansi-color-control-seq-regexp s))

(defun eask-s-replace-ansi (old new s)
  "Like the function `eask-s-replace' but work with ansi.

For arguments OLD, NEW and S; see the function `eask-s-replace'
for more information."
  (if-let* ((splits (split-string s (regexp-quote old))))
      (let* ((reset "\e[0m")
             (result (nth 0 splits))
             (index  1)
             (last))
        (while (< index (length splits))
          (let* ((data (eask-ansi-codes result)))
            (setq last (car (last data))
                  result (concat result
                                 (if (null last) "" reset) new last
                                 (nth index splits))))
          (cl-incf index))
        ;; Just ensure the last character is reset.
        (concat result (if (null last) "" reset)))
    (eask-s-replace old new s)))

(defun eask-progress-seq (prefix sequence suffix func)
  "Shorthand to progress SEQUENCE of task.

Arguments PREFIX and SUFFIX are strings to print before and after each progress.
Argument FUNC are execution for eash progress; this is generally the actual
task work."
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
  "Loop through each line and print each line with corresponds log level.

You can pass BUFFER-OR-NAME to replace current buffer."
  (with-current-buffer (or buffer-or-name (current-buffer))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                  (line-end-position))))
        ;; The variable `line' can contains format specifier, avoid it with `%s'!
        (cond ((string-match-p "[: ][Ee]rror: " line)
               (eask-error "%s" line))
              ((string-match-p "[: ][Ww]arning: " line)
               (eask-warn "%s" line))
              (t
               (eask-log "%s" line))))
      (forward-line 1))))

(defun eask-delete-file (filename)
  "Delete a FILENAME from disk."
  (let (deleted)
    (eask-with-progress
      (format "Deleting %s... " filename)
      (eask-with-verbosity 'log
        (setq deleted (file-exists-p filename))
        (ignore-errors (delete-file filename))
        (setq deleted (and deleted (not (file-exists-p filename)))))
      (if deleted "done ✓" "skipped ✗"))
    deleted))

(defun eask--action-format (len)
  "Construct action format by LEN."
  (setq len (eask-2str len))
  (concat "[%" (eask-2str (length len)) "d/" len "] "))

(defconst eask-package-archives-url-format
  "https://raw.githubusercontent.com/emacs-eask/archives/master/%s/"
  "The backup package archives url.")

(defun eask--locate-archive-contents (archive)
  "Locate ARCHIVE's contents file."
  (let* ((name (cond ((consp archive) (car archive))
                     (t archive)))
         (file "archive-contents")
         (dir (expand-file-name (concat "archives/" name) package-user-dir)))
    (expand-file-name file dir)))

(defun eask--package-download-one-archive (fnc &rest args)
  "Execution around function `package-download-one-archive'.

Arguments FNC and ARGS are used for advice `:around'."
  (cl-incf eask--action-index)
  (let* ((archive (nth 0 args))
         (name (car archive))
         (url (cdr archive))
         (fmt (eask--action-format (length package-archives)))
         (download-p))
    (eask-with-verbosity-override 'log
      (when (= 1 eask--action-index) (eask-msg ""))
      (eask-with-progress
        (format "  - %sDownloading %s (%s)... "
                (format fmt eask--action-index)
                (ansi-green (eask-2str name))
                (ansi-yellow (eask-2str url)))
        (eask-with-verbosity 'debug
          (apply fnc args)
          (setq download-p t))
        (cond (download-p "done ✓")
              (t          "failed ✗"))))))

(defun eask--download-archives ()
  "If archives download failed; download it manually."
  (let ((archives (cl-remove-if (lambda (archive)
                                  ;; First, exclude the `local' archive.
                                  (equal (car archive) eask--local-archive-name))
                                package-archives))
        (one-download-p))
    (dolist (archive archives)
      (cl-incf eask--action-index)
      (let* ((name (car archive))
             (local-file (eask--locate-archive-contents archive))
             (dir (file-name-directory local-file))      ; ~/.emacs.d/elpa/archives/{name}/
             (contents (file-name-nondirectory local-file))  ; archive-contents
             (url (format eask-package-archives-url-format name))
             (url-file (concat url contents))
             (download-p)
             (fmt (eask--action-format (length archives))))
        (unless (file-exists-p local-file)
          (eask-with-verbosity-override 'log
            (when (= 1 eask--action-index) (eask-println ""))
            (eask-with-progress
              (format "  - %sDownloading %s (%s) manually... "
                      (format fmt eask--action-index)
                      (ansi-green name)
                      (ansi-yellow url))
              (eask-with-verbosity 'debug
                (if (url-file-exists-p url-file)
                    (progn
                      (ignore-errors (make-directory dir t))
                      (url-copy-file url-file local-file t)
                      (setq download-p t
                            one-download-p t))
                  (eask-debug "No archive-contents found in `%s'" (ansi-green name))))
              (cond (download-p "done ✓")
                    (t          "failed ✗")))))))
    (when one-download-p (eask-pkg-init t))))

(defun eask-use-legacy-package-build ()
  "Return t if using the legacy `package-build' package."
  (version< emacs-version "27.1"))

(defun eask-load-legacy-package-build ()
  "Load the legacy `package-build' package."
  (when (eask-use-legacy-package-build)
    (add-to-list 'load-path
                 (format "%sextern/package-build/%s/"
                         eask-lisp-root
                         emacs-major-version)
                 t)))

(defun eask--update-exec-path ()
  "Add all bin directory to the variable `exec-path'."
  (dolist (entry (directory-files package-user-dir t directory-files-no-dot-files-regexp))
    (when-let* ((bin (expand-file-name "bin" entry))
                ((file-directory-p bin)))
      (add-to-list 'exec-path bin t)))
  (delete-dups exec-path))

(defun eask--update-load-path ()
  "Add all .el files to the variable `load-path'."
  (dolist (filename (eask-package-el-files))
    (add-to-list 'load-path (file-name-directory filename) t))
  (delete-dups load-path))

(defun eask-dependencies ()
  "Return list of dependencies."
  (append eask-depends-on (and (eask-dev-p) eask-depends-on-dev)))

(defun eask--package-mapc (func deps)
  "Like function `mapc' but for process package transaction specifically.

For arguments FUNC and DEPS, see function `mapc' for more information."
  (let* ((eask--action-prefix)  ; remain untouch
         (len (length deps))
         (fmt (eask--action-format len))
         (count 0))
    (dolist (dep deps)
      (cl-incf count)
      (setq eask--action-prefix (format fmt count))
      (funcall func dep))))

(defun eask--install-dep (dep)
  "Custom install DEP."
  (let ((name (car dep)))
    (cond
     ;; Install through the function `package-install-file'.
     ((memq :file dep)
      (let ((file (nth 2 dep)))
        (eask-package-install-file name file)))
     ;; Install through the function `package-vc-install'.
     ((memq :vc dep)
      (let ((spec (cdr (memq :vc dep))))
        (eask-package-vc-install name spec)))
     ;; Try out packages using `try'.
     ((memq :try dep)
      (let ((url-or-package (nth 2 dep)))
        (eask-package-try name url-or-package)))
     ;; Fallback to archive install.
     (t (eask-package-install name)))))

(defun eask--install-deps (dependencies msg)
  "Install DEPENDENCIES.

Argument MSG are extra information to display in the header; mainly define the
scope of the dependencies (it's either `production' or `development')."
  (let* ((names (mapcar #'car dependencies))
         (names (mapcar #'eask-intern names))
         (len (length dependencies))
         (ies (eask--sinr len "y" "ies"))
         (pkg-installed (cl-remove-if #'package-installed-p names))
         (installed (length pkg-installed))
         (skipped (- len installed)))
    (eask-log "Installing %s %s dependenc%s..." len msg ies)
    (eask-msg "")
    (eask--package-mapc #'eask--install-dep dependencies)
    (eask-msg "")
    (eask-info "(Total of %s dependenc%s installed, %s skipped)"
               installed ies skipped)))

(defun eask-install-dependencies ()
  "Install dependencies defined in Eask file."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (when eask-depends-on-recipe-p
    (eask-log "Installing required external packages...")
    (eask-archive-install-packages '("gnu" "melpa")
                                   'package-build)
    (eask-with-progress
      "Building temporary archives (this may take a while)... "
      (eask-with-verbosity 'debug (github-elpa-build))
      "done ✓")
    (eask-pkg-init t))
  (when eask-depends-on
    (eask--install-deps eask-depends-on "package"))
  (when (and eask-depends-on-dev (eask-dev-p))
    (eask-msg "")
    (eask--install-deps eask-depends-on-dev "development")))

(defun eask-setup-paths ()
  "Setup both variables `exec-path' and `load-path'."
  (eask-with-progress
    (ansi-green "Updating environment variables... ")
    (eask-with-verbosity 'debug
      (eask--update-exec-path) (eask--update-load-path)
      (setenv "PATH" (string-join exec-path path-separator))
      (setenv "EMACSLOADPATH" (string-join load-path path-separator)))
    (ansi-green "done ✓")))

(defun eask-pkg-init (&optional force)
  "Package initialization.

If the argument FORCE is non-nil, force initialize packages in this session."
  (when (or (not package--initialized) (not package-archive-contents) force
            ;; XXX: we need to initialize once in global scope since most Emacs
            ;; configuration would likely to set `package-archives' variable
            ;; themselves.
            (and (eask-config-p) (not eask--package-initialized)))
    (setq eask--package-initialized t)
    (eask-with-progress
      (ansi-green "Loading package information... ")
      (eask-with-verbosity 'debug
        (package-initialize t)
        (let ((eask--action-index 0)) (package-refresh-contents))
        (let ((eask--action-index 0)) (eask--download-archives)))
      (ansi-green "done ✓"))))

(defun eask--pkg-transaction-vars (pkg)
  "Return 1 symbol and 2 strings.

Argument PKG is the name of the package."
  (let* (;; Ensure symbol
         (pkg (eask-intern pkg))
         ;; Wrap package name with color
         (pkg-string (ansi-green (eask-2str pkg)))
         ;; Wrap version number with color
         (pkg-version (ansi-yellow (eask-package--version-string pkg))))
    (list pkg pkg-string pkg-version)))

(defun eask-archive-install-packages (archives &rest names)
  "Install package NAMES with ARCHIVES setup."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (when (cl-some (lambda (pkg) (not (package-installed-p pkg))) names)
    (eask-with-archives archives
      (eask--package-mapc #'eask-package-install names))))

(defun eask-package-installable-p (pkg)
  "Return non-nil if package (PKG) is installable."
  (assq (eask-intern pkg) package-archive-contents))

(defun eask-package-try (pkg &optional url-or-package)
  "To try a package (PKG) without actually install it.

The optional argument URL-OR-PACKAGE is used in the function `try'."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask-with-progress
    (format "  - %sTrying out %s%s... " eask--action-prefix
            (ansi-green (eask-2str pkg))
            (if url-or-package
                (concat " in " (ansi-yellow url-or-package))
              ""))
    (eask-with-verbosity 'debug
      (eask-archive-install-packages '("gnu" "melpa") 'try)
      (require 'try)
      (try (or url-or-package pkg)))
    "done ✓"))

(defun eask--package-delete-before-install (pkg force)
  "Make sure PKG is not presented before installing the latest.

The argument FORCE is passed through to the `package-delete' function."
  ;; Recipe can be `nil', handle it.
  (when-let* ((rcp (eask-package-desc pkg t)))
    (package-delete rcp force)
    t))

(defun eask-package-vc-install (pkg spec)
  "To vc install the package (PKG) by argument SPEC."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask--pkg-process pkg
    (cond
     ((and installed-p (not should-reinstall-p))
      (eask-with-progress
        (format "  - %sSkipping %s (%s)... " eask--action-prefix name version)
        (progn )  ; no operation needed
        "already installed ✗"))
     (t
      (eask-with-progress
        (format "  - %s%snstalling %s (%s)... " eask--action-prefix
                (if should-reinstall-p "Rei" "I")
                name version)
        (eask-with-verbosity 'debug
          ;; Handle `--force` flag.
          (when should-reinstall-p
            (eask--package-delete-before-install pkg t))
          ;; Install it.
          (apply #'package-vc-install spec))
        "done ✓")))))

(defun eask-package-install-file (pkg file)
  "To FILE install the package (PKG)."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask--pkg-process pkg
    (cond
     ((and installed-p (not should-reinstall-p))
      (eask-with-progress
        (format "  - %sSkipping %s (%s)... " eask--action-prefix name version)
        (progn )  ; no operation needed
        "already installed ✗"))
     (t
      (eask-with-progress
        (format "  - %s%snstalling %s (%s)... " eask--action-prefix
                (if should-reinstall-p "Rei" "I")
                name version)
        (eask-with-verbosity 'debug
          ;; Handle `--force` flag.
          (when should-reinstall-p
            (eask--package-delete-before-install pkg t))
          ;; Install it.
          (package-install-file (expand-file-name file)))
        "done ✓")))))

(defun eask-package-install (pkg)
  "Install the package (PKG)."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask--pkg-process pkg
    (cond
     ((and installed-p (not should-reinstall-p))
      (eask-with-progress
        (format "  - %sSkipping %s (%s)... " eask--action-prefix name version)
        (progn )  ; no operation needed
        "already installed ✗"))
     ((progn
        (eask-pkg-init)
        (unless (eask-package-installable-p pkg)
          (eask-error "Package not installable `%s'; make sure the package archive (source) is included" pkg))))
     (t
      (eask--pkg-process pkg  ; Second call to force refresh the data.
        (eask-with-progress
          (format "  - %s%snstalling %s (%s)... " eask--action-prefix
                  (if should-reinstall-p "Rei" "I")
                  name version)
          (eask-with-verbosity 'debug
            ;; Handle `--force` flag.
            (when should-reinstall-p
              (eask--package-delete-before-install pkg t))
            ;; Install it.
            (let ((current-prefix-arg (eask-force-p)))
              (package-install pkg)))
          "done ✓"))))))

(defun eask-package-delete (pkg)
  "Delete the package (PKG)."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask--pkg-process pkg
    (cond
     ((not installed-p)
      (eask-with-progress
        (format "  - %sSkipping %s (%s)... " eask--action-prefix name version)
        (progn )  ; no operation needed
        "not installed ✗"))
     (t
      (eask--pkg-process pkg  ; Second call to force refresh the data.
        (let ((success))
          (eask-with-progress
            (format "  - %sUninstalling %s (%s)... " eask--action-prefix name version)
            (eask-with-verbosity 'debug
              (when (eask--package-delete-before-install pkg (eask-force-p))
                (setq success t)))
            (if success "done ✓" "skipped ✗"))))))))

(defun eask-package-reinstall (pkg)
  "Reinstall the package (PKG)."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask--pkg-process pkg
    (cond
     ;; You cannot reinstall the package that are not installed.
     ((not installed-p)
      (eask-with-progress
        (format "  - %sSkipping %s (%s)... " eask--action-prefix name version)
        (progn )  ; no operation needed
        "not installed ✗"))
     (t
      (eask-pkg-init)
      (eask--pkg-process pkg  ; Second call to force refresh the data.
        (eask-with-progress
          (format "  - %sReinstalling %s (%s)... " eask--action-prefix name version)
          (eask-with-verbosity 'debug
            (eask--package-delete-before-install pkg t)
            (let ((current-prefix-arg (eask-force-p)))
              (package-install pkg)))
          "done ✓"))))))

(defun eask-package-desc (name &optional current)
  "Build package description by its NAME.

The argument NAME must be the package's name.  If the argument CURRENT is
non-nil, we retrieve version number from the variable `package-alist';
otherwise, we retrieve it from the variable `package-archive-contents'."
  (cadr (assq name (if current package-alist
                     (or package-archive-contents package-alist)))))

(defun eask-package--version (name &optional current)
  "Return package's version.

For arguments NAME and CURRENT, please see function `eask-package-desc' for
full detials."
  (when-let* ((desc (eask-package-desc name current)))
    (package-desc-version desc)))

(defun eask-package--version-string (pkg)
  "Return PKG's version."
  (if-let* ((version (or (eask-package--version pkg t)
                         (eask-package--version pkg nil))))
      (package-version-join version)
    ;; Just in case, but this should never happens!
    "0"))

(defun eask-package-desc-url ()
  "Return url from package descriptor."
  (when eask-package-desc
    (when-let* ((extras (package-desc-extras eask-package-desc)))
      (cdr (assoc :url extras)))))

(defun eask-package-desc-keywords ()
  "Return keywords from package descriptor."
  (when eask-package-desc
    (or (package-desc--keywords eask-package-desc)
        ;; XXX: Handle Emacs 26.x keywords cannot be parsed issue.
        (and eask-package-file
             (with-temp-buffer
               (insert-file-contents eask-package-file)
               (lm-keywords-list))))))

(defun eask-pkg-el ()
  "Return package description file if exists."
  (let ((pkg-el (package--description-file default-directory)))
    (when (file-readable-p pkg-el) pkg-el)))

(defun eask--str2num (str)
  "Convert string (STR) to number."
  (ignore-errors (string-to-number str)))

(defun eask--flag (flag)
  "Return non-nil if FLAG exists."
  (member (concat "--eask" flag) eask-argv))

(defun eask--flag-value (flag)
  "Return value for FLAG."
  (nth 1 (eask--flag flag)))

(defun eask-global-p ()
  "Non-nil when in global space (`-g', `--global')."
  (eask--flag "-g"))

(defun eask-config-p ()
  "Non-nil when in config space (`-c', `--config')."
  (eask--flag "-c"))

(defun eask-local-p ()
  "Non-nil when in local space (default)."
  (and (not (eask-global-p))
       (not (eask-config-p))))

(defun eask-all-p ()
  "Non-nil when flag is on (`-a', `--all')."
  (eask--flag "-a"))

(defun eask-quick-p ()
  "Non-nil when flag is on (`-q', `--quick')."
  (eask--flag "-q"))

(defun eask-force-p ()
  "Non-nil when flag is on (`-f', `--force')."
  (eask--flag "-f"))

(defun eask-dev-p ()
  "Non-nil when flag is on (`--dev', `--development')."
  (eask--flag "--dev"))

(defun eask-debug-p ()
  "Non-nil when flag is on (`--debug')."
  (eask--flag "--debug"))

(defun eask-strict-p ()
  "Non-nil when flag is on (`--strict')."
  (eask--flag "--strict"))

(defun eask-timestamps-p ()
  "Non-nil when flag is on (`--timestamps')."
  (eask--flag "--timestamps"))

(defun eask-log-level-p ()
  "Non-nil when flag is on (`--log-level')."
  (eask--flag "--log-level"))

(defun eask-log-file-p ()
  "Non-nil when flag is on (`--log-file', `--lf')."
  (eask--flag "--log-file"))

(defun eask-elapsed-time-p ()
  "Non-nil when flag is on (`--elapsed-time', `--et')."
  (eask--flag "--elapsed-time"))

(defun eask-allow-error-p ()
  "Non-nil when flag is on (`--allow-error')."
  (eask--flag "--allow-error"))

(defun eask-insecure-p ()
  "Non-nil when flag is on (`--insecure')."
  (eask--flag "--insecure"))

(defun eask-no-color-p ()
  "Non-nil when flag is on (`--no-color')."
  (eask--flag "--no-color"))

(defun eask-clean-p ()
  "Non-nil when flag is on (`-c', `--clean')."
  (eask--flag "--clean"))

(defun eask-json-p ()
  "Non-nil when flag is on (`--json')."
  (eask--flag "--json"))

(defun eask-number-p ()
  "Non-nil when flag is on (`-n', `--number')."
  (eask--flag "--number"))

(defun eask-yes-p ()
  "Non-nil when flag is on (`--yes')."
  (eask--flag "--yes"))

(defun eask-output ()
  "Non-nil when flag has value (`--o', `--output')."
  (eask--flag-value "--output"))

(defun eask-proxy ()
  "Non-nil when flag has value (`--proxy')."
  (eask--flag-value "--proxy"))

(defun eask-http-proxy ()
  "Non-nil when flag has value (`--http-proxy')."
  (eask--flag-value "--http-proxy"))

(defun eask-https-proxy ()
  "Non-nil when flag has value (`--https-proxy')."
  (eask--flag-value "--https-proxy"))

(defun eask-no-proxy ()
  "Non-nil when flag has value (`--no-proxy')."
  (eask--flag-value "--no-proxy"))

(defun eask-destination ()
  "Non-nil when flag has value (`--dest', `--destination')."
  (eask--flag-value "--dest"))

(defun eask-from ()
  "Non-nil when flag has value (`--from')."
  (eask--flag-value "--from"))

(defun eask-depth ()
  "Non-nil when flag has value (`--depth')."
  (eask--str2num (eask--flag-value "--depth")))

(defun eask-verbose ()
  "Non-nil when flag has value (`-v', `--verbose')."
  (eask--str2num (eask--flag-value "--verbose")))

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

(defun eask--add-proxy (protocal host)
  "Add proxy.

Argument PROTOCAL and HOST are used to construct scheme."
  (when host (push (cons protocal (eask-proxy)) url-proxy-services)))

(defun eask--form-options (options)
  "Add --eask to all OPTIONS."
  (mapcar (lambda (elm) (concat "--eask" elm)) options))

(defconst eask--option-switches
  (eask--form-options
   '("-g" "-c" "-a" "-q" "-f" "--dev"
     "--debug" "--strict"
     "--allow-error"
     "--insecure"
     "--timestamps" "--log-level"
     "--log-file"
     "--elapsed-time"
     "--no-color"
     "--clean"
     "--json"
     "--number"
     "--yes"))
  "List of boolean type options.")

(defconst eask--option-args
  (eask--form-options
   '("--output"
     "--proxy" "--http-proxy" "--https-proxy" "--no-proxy"
     "--verbose" "--silent"
     "--depth" "--dest" "--from"))
  "List of arguments (number/string) type options.")

(defconst eask--command-list
  (append eask--option-switches eask--option-args)
  "List of commands to accept, so we can avoid unknown option error.")

(defun eask-self-command-p (arg)
  "Return non-nil if ARG is known internal command."
  (member arg eask--command-list))

(defun eask-argv (index)
  "Return argument value by INDEX."
  (elt eask-argv index))

(defun eask-argv-out ()
  "Convert all internal arguments to external arguments.

Simply remove `--eask' for each option, like `--eask--strict' to `--strict'."
  (mapcar (lambda (arg)
            (eask-s-replace "--eask" "" arg))
          eask-argv))

(defun eask-args (&optional index)
  "Get all arguments except options

If the optional argument INDEX is non-nil, return the element."
  (let ((argv (cl-remove-if (lambda (arg) (member arg eask--option-switches)) eask-argv))
        (args) (skip-next))
    (dolist (arg argv)
      (if skip-next (setq skip-next nil)
        (if (member arg eask--option-args)
            (setq skip-next t)
          (push arg args))))
    (setq args (reverse args))
    (if index (nth 0 args) args)))

(defconst eask-file-keywords
  '("package" "website-url" "keywords"
    "author" "license"
    "package-file" "package-descriptor" "files"
    "script"
    "source" "source-priority"
    "depends-on" "development"
    "exec-paths" "load-paths")
  "List of Eask file's DSL keywords.")

(defun eask--loop-file-keywords (func)
  "Loop through Eask file keywords for environment replacement.

Argument FUNC is a function we execute while this function will provide the new
and old function name.

Internal used for function `eask--alias-env'."
  (dolist (keyword eask-file-keywords)
    (let ((keyword-sym (intern keyword))
          (api (intern (concat "eask-f-" keyword)))    ; existing function
          (old (intern (concat "eask--f-" keyword))))  ; variable that holds function pointer
      (funcall func keyword-sym api old))))

(defun eask-working-directory ()
  "Return the working directory of the program going to be executed."
  (cond ((eask-config-p) user-emacs-directory)
        ((eask-global-p) (expand-file-name "../../" user-emacs-directory))
        (t default-directory)))

(defun eask-root-del (filename)
  "Remove Eask file root path from FILENAME."
  (when (stringp filename)
    (eask-s-replace (or eask-file-root default-directory) "" filename)))

(defun eask-file-load (location &optional noerror)
  "Load Eask file in the LOCATION.

Argument NOERROR is passed through function `load'; therefore, please see the
function `load' for more detials."
  (when-let* ((target-eask-file (expand-file-name location user-emacs-directory))
              (result (eask--alias-env (load target-eask-file noerror t t))))
    (setq eask-file target-eask-file  ; assign eask file only if success
          eask-file-root (file-name-directory target-eask-file))
    (run-hooks 'eask-file-loaded-hook)
    result))

(defun eask--match-file (name)
  "Check to see if NAME is our target Eask-file, then return it."
  (let (;; Ensure path to filename
        (name             (file-name-nondirectory (directory-file-name name)))
        ;; `p-' stands for pattern
        (p-easkfile-full  (format "Easkfile.%s" emacs-version))
        (p-easkfile-major (format "Easkfile.%s" emacs-major-version))
        (p-easkfile       "Easkfile")
        (p-eask-full      (format "Eask.%s" emacs-version))
        (p-eask-major     (format "Eask.%s" emacs-major-version))
        (p-eask           "Eask"))
    (car (member name (list p-easkfile-full p-easkfile-major p-easkfile
                            p-eask-full p-eask-major p-eask)))))

(defun eask--all-files (&optional dir)
  "Return a list of Eask files from DIR.

If argument DIR is nil, we use `default-directory' instead."
  (setq dir (or dir default-directory))
  (when-let* ((files (append
                      (ignore-errors (directory-files dir t "Easkfile[.0-9]*\\'"))
                      (ignore-errors (directory-files dir t "Eask[.0-9]*\\'"))))
              (files (cl-remove-if #'file-directory-p files)))
    (cl-remove-if-not #'eask--match-file files)))

(defun eask--find-files (start-path)
  "Find the Eask-file from START-PATH.

This uses function `locate-dominating-file' to look up directory tree."
  (when-let*
      (;; XXX: This is redundant, but the simplest way to find the root path!
       (root (locate-dominating-file start-path #'eask--all-files))
       (files (eask--all-files root))  ; get all available Eask-files
       ;; Filter it to restrict to this Emacs version!
       (files (cl-remove-if-not #'eask--match-file files))
       ;; Make `Easkfile.29.1' > `Easkfile.29' > `Easkfile' (same with `Eask' file)
       (files (sort files #'string-greaterp))
       ;; Make `Easkfile' > `Eask' higher precedent!
       (files (sort files (lambda (item1 item2)
                            (and (string-prefix-p "Easkfile" item1)
                                 (not (string-prefix-p "Easkfile" item2)))))))
    files))

(defun eask-file-try-load (start-path)
  "Try load the Eask-file in START-PATH."
  (when-let* ((files (eask--find-files start-path))
              (file (car files)))
    ;; Revert printing behaviour when loading Eask-file.
    (eask--unsilent (eask-file-load file))))

(defun eask--load-config ()
  "Load configuration if valid."
  (let ((inhibit-config (eask-quick-p)))
    (eask-with-progress
      (ansi-green "Loading configuration... ")
      ;; Revert printing behaviour when loading user files.
      (eask--unsilent
        (unless inhibit-config
          ;; `early-init.el' is supported after 27.1
          (when (version<= "27" emacs-version)
            (load early-init-file t t))
          (load eask-dot-emacs-file t t)
          (load user-init-file t t)))
      (ansi-green (if inhibit-config "skipped ✗" "done ✓")))))

(defun eask--resolve-exit-status ()
  "Resolve current exit status."
  (when (memq 'error (eask--error-status))
    (eask--exit 'failure)))

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
    (shmelpa      . "https://shmelpa.commandlinesystems.com/packages/")
    (ublt         . "https://elpa.ubolonton.org/packages/")
    ;; Devel
    (gnu-devel    . "https://elpa.gnu.org/devel/")
    (nongnu-devel . "https://elpa.nongnu.org/nongnu-devel/"))
  "Mapping of source name and url.")

(defun eask-source-url (name &optional location)
  "Get the source url by it's NAME and LOCATION."
  (setq location (or location (cdr (assq (intern (eask-2str name)) eask-source-mapping)))
        location (eask-2url location))
  location)

(defun eask-package--get (key)
  "Return package info by KEY."
  (plist-get eask-package key))

(defun eask-package-name ()
  "Return current package's name."
  (eask-package--get :name))

(defun eask-package-version ()
  "Return current package's version number."
  (eask-package--get :version))

(defun eask-package-description ()
  "Return current package's description."
  (eask-package--get :description))

(defun eask-depends-emacs-version ()
  "Get Eask-file Emacs version string."
  (nth 0 (cdar eask-depends-on-emacs)))

(defun eask-f-package (name version description)
  "Set the package information.

Argument NAME is the name of the package.  VERSION is the string contains valid
version number.  DESCRIPTION is the package description."
  (if eask-package
      (eask-error "✗ Multiple definition of `package'")
    (setq eask-package `(:name ,name :version ,version :description ,description))
    (progn  ; Run checker
      (eask--checker-string "Name" name)
      (version= version "0.1.0")
      (eask--checker-string "Description" description))))

(defun eask-f-website-url (url)
  "Set website URL."
  (if eask-website-url
      (eask-error "✗ Multiple definition of `website-url'")
    (setq eask-website-url url)))

(defun eask-f-keywords (&rest keywords)
  "Set package KEYWORDS."
  (if eask-keywords
      (eask-error "✗ Multiple definition of `keywords'")
    (setq eask-keywords keywords)))

(defun eask-f-author (name &optional email)
  "Set package author's NAME and EMAIL."
  (if (member name (mapcar #'car eask-authors))
      (eask-warn "💡 Warning regarding duplicate author name, %s" name)
    (when (and email
               (not (string-match-p "@" email)))
      (eask-warn "💡 Email seems to be invalid, %s" email))
    (push (cons name email) eask-authors)))

(defun eask-f-license (name)
  "Set package license NAME."
  (if (member name eask-licenses)
      (eask-warn "💡 Warning regarding duplicate license name, %s" name)
    (push name eask-licenses)))

(defun eask--try-construct-package-desc (file)
  "Try construct the package descriptor from FILE."
  (let (skipped)
    (with-temp-buffer
      (insert-file-contents file)
      (setq eask-package-desc
            (ignore-errors
              (cond ((string-suffix-p "-pkg.el" file)  ; if ensure -pkg.el
                     (package--read-pkg-desc 'dir))
                    ((eask-pkg-el)                     ; if -pkg.el is presented,
                     (setq skipped t) nil)             ; skip it
                    (t (package-buffer-info))))))      ; default read main package file
    (eask-with-verbosity 'debug
      (eask-msg (concat
                 (if eask-package-desc "✓ " "✗ ")
                 "Try constructing the package-descriptor (%s)... "
                 (cond (eask-package-desc "succeeded!")
                       (skipped           "skipped!")
                       (t                 "failed!")))
                (file-name-nondirectory file)))))

(defun eask-f-package-file (file)
  "Set package FILE."
  (if eask-package-file
      (eask-error "✗ Multiple definition of `package-file'")
    (setq eask-package-file (expand-file-name file))
    (if (file-exists-p eask-package-file)
        (eask--try-construct-package-desc eask-package-file)
      (eask-warn "💡 Package-file seems to be missing `%s'" file))
    (when-let*
        (((and (not eask-package-descriptor)  ; prevent multiple definition error
               (not eask-package-desc)))      ; check if constructed
         (pkg-file (eask-pkg-el)))
      (eask-f-package-descriptor pkg-file)
      ;; XXX: Make sure DSL package descriptor is set back to `nil'
      (setq eask-package-descriptor nil))))

(defun eask-f-package-descriptor (pkg-file)
  "Set package PKG-FILE."
  (cond
   (eask-package-descriptor
    (eask-error "✗ Multiple definition of `package-descriptor'"))
   ((and eask-package-desc                ; check if construct successfully
         (equal (eask-pkg-el) pkg-file))  ; check filename the same
    )                                     ; ignore
   (t
    (setq eask-package-descriptor (expand-file-name pkg-file))
    (cond ((not (string-suffix-p "-pkg.el" eask-package-descriptor))
           (eask-error "✗ Pkg-file must end with `-pkg.el'"))
          ((not (file-exists-p eask-package-descriptor))
           (eask-warn "💡 Pkg-file seems to be missing `%s'" pkg-file))
          (t
           (eask--try-construct-package-desc eask-package-descriptor))))))

(defun eask-f-files (&rest patterns)
  "Set files PATTERNS."
  (setq eask-files (append eask-files patterns)))

(defun eask-f-script (name command &rest args)
  "Add a script command.

Argument NAME is the command id, and cannot be repeated.  Argument COMMAND is
a string contain shell commands.  The rest arguments ARGS is a list of string
contains extra shell commands, and it will eventually be concatenate with the
argument COMMAND."
  (when (symbolp name) (setq name (eask-2str name)))  ; ensure to string, accept symbol
  (when (assoc name eask-scripts)
    (eask-error "✗ Run-script with the same key name is not allowed: `%s`" name))
  (push (cons name
              (mapconcat #'identity (append (list command) args) " "))
        eask-scripts))

(defun eask-f-source (name &optional location)
  "Add archive NAME with LOCATION."
  (when (symbolp name) (setq name (eask-2str name)))  ; ensure to string, accept symbol
  ;; Handle local archive.
  (when (equal name eask--local-archive-name)
    (eask-error "✗ Invalid archive name `%s'" name))
  ;; Handle multiple same archive name!
  (when (assoc name package-archives)
    (eask-error "✗ Multiple definition of source `%s'" name))
  (setq location (eask-source-url name location))
  (unless location (eask-error "✗ Unknown package archive `%s'" name))
  (add-to-list 'package-archives (cons name location) t))

(defun eask-f-source-priority (name &optional priority)
  "Add PRIORITY for to NAME."
  (when (symbolp name) (setq name (eask-2str name)))  ; ensure to string, accept symbol
  (add-to-list 'package-archive-priorities (cons name priority) t))

(defun eask--setup-dependencies ()
  "Setup dependencies list."
  (setq eask-depends-on (reverse eask-depends-on)
        eask-depends-on-dev (reverse eask-depends-on-dev))
  ;; On recipe
  (when eask-depends-on-recipe-p
    (eask-with-progress
      (concat (ansi-green "✓ Checking local archives `")
              (ansi-magenta eask--local-archive-name)
              (ansi-green "`... "))
      (eask-with-verbosity 'debug
        ;; Make sure can be customized by `source'
        (unless (assoc eask--local-archive-name package-archives)
          (add-to-list 'package-archives
                       `(,eask--local-archive-name . ,github-elpa-archive-dir) t))
        ;; Make sure can be customized by `source-priority'
        (unless (assoc eask--local-archive-name package-archive-priorities)
          ;; If the local archives is added, we set the priority to a very
          ;; high number so user we always use the specified dependencies!
          (add-to-list 'package-archive-priorities
                       `(,eask--local-archive-name . 90) t)))
      (ansi-green "done!"))))

(defun eask--check-depends-on (recipe)
  "Return non-nil if RECIPE is invalid."
  (let ((pkg (car recipe))
        (minimum-version (cdr recipe)))
    (cond ((member recipe eask-depends-on)
           (eask-error "✗ Define dependencies with the same name `%s'" pkg))
          ((cl-some (lambda (rcp)
                      (string= (car rcp) pkg))
                    eask-depends-on)
           (eask-error "✗ Define dependencies with the same name `%s' with different version" pkg)))))

(defun eask-f-depends-on (pkg &rest args)
  "Specify a dependency (PKG) of this package.

Argument PKG is the name of that dependency.  ARGS can either be a string
contains the version number or a list contains recipe information (for local
ELPA)."
  (cond
   ((string= pkg "emacs")
    (if eask-depends-on-emacs
        (eask-error "✗ Define dependencies with the same name `%s'" pkg)
      (let* ((minimum-version (car args))
             (recipe (list pkg minimum-version)))
        (if (version< emacs-version minimum-version)
            (eask-error "✗ This requires Emacs %s and above!" minimum-version)
          (push recipe eask-depends-on-emacs))
        recipe)))
   ;; Specified packages
   ((or (memq :file args)  ; File packages
        (memq :vc args)    ; VC packages
        (memq :try args))  ; Try packages
    (let* ((recipe (append (list (intern pkg)) args)))
      (unless (eask--check-depends-on recipe)
        (push recipe eask-depends-on))
      recipe))
   ;; No argument specify
   ((<= (length args) 1)
    (let* ((minimum-version (car args))
           (recipe (list pkg minimum-version)))
      (unless (eask--check-depends-on recipe)
        (push recipe eask-depends-on))
      recipe))
   ;; recipe are entered
   (t
    (let ((recipe (append (list (intern pkg)) args)))
      (unless (eask--check-depends-on recipe)
        (push recipe eask-depends-on)
        (eask-load "extern/github-elpa")
        (eask-with-verbosity 'debug
          (eask-with-progress
            (ansi-blue (format "Generating recipe for package %s... " (ansi-yellow pkg)))
            (write-region (pp-to-string recipe) nil (expand-file-name pkg github-elpa-recipes-dir))
            (ansi-blue "done ✓")))
        (setq eask-depends-on-recipe-p t))
      recipe))))

(defun eask-f-development (&rest dep)
  "Development scope with list of DEP."
  (setq dep (cl-remove-if #'null dep))  ; make sure no `nil', see #143
  (dolist (pkg dep)
    (push pkg eask-depends-on-dev)
    (delete-dups eask-depends-on-dev)
    (setq eask-depends-on (remove pkg eask-depends-on))))

(defun eask-f-exec-paths (&rest dirs)
  "Add all DIRS to the variable `exec-path'."
  (dolist (dir dirs) (add-to-list 'exec-path (expand-file-name dir) t)))

(defun eask-f-load-paths (&rest dirs)
  "Add all DIRS to to the variable `load-path'."
  (dolist (dir dirs) (add-to-list 'load-path (expand-file-name dir) t)))

(defun eask--verb2lvl (symbol)
  "Convert verbosity SYMBOL to level."
  (cl-case symbol
    (`all   5)
    (`debug 4)
    (`log   3)
    (`info  2)
    (`warn  1)
    (`error 0)
    (t symbol)))

(defun eask-reach-verbosity-p (symbol)
  "Return t if SYMBOL reach verbosity (should be printed)."
  (>= eask-verbosity (eask--verb2lvl symbol)))

(defun eask--ansi (symbol string)
  "Paint STRING with color defined by log level (SYMBOL)."
  (if-let* ((ansi-function (cdr (assq symbol eask-level-color))))
      ;; The `%s` is use to avoid `not enough arguments for string` error.
      (funcall ansi-function "%s" string)
    string))

(defun eask--format (prefix fmt &rest args)
  "Format Eask messages.

Argument PREFIX is a string identify the type of this messages.  Arguments FMT
and ARGS are used to pass through function `format'."
  (concat (when eask-timestamps (format-time-string "%Y-%m-%d %H:%M:%S "))
          (when eask-log-level (concat prefix " "))
          (apply #'format fmt args)))

(defun eask--msg (symbol prefix fmt &rest args)
  "If level (SYMBOL) is at or below `eask-verbosity'; then, log the message.

For arguments PREFIX, FMT and ARGS, please see funtion `eask--format' for the
detials."
  (eask-with-verbosity symbol
    (let* ((output (apply #'eask--format prefix fmt args))
           (output (eask--ansi symbol output))
           (output (eask--msg-displayable-kwds output))  ; Don't color, but replace it!
           (func (cl-case symbol
                   ((or error warn) symbol)
                   (t               #'message))))
      (funcall func "%s" output))))

(defun eask-debug (fmt &rest args)
  "Send debug message; see function `eask--msg' for arguments FMT and ARGS."
  (apply #'eask--msg 'debug "[DEBUG]" fmt args))

(defun eask-log (fmt &rest args)
  "Send log message; see function `eask--msg' for arguments FMT and ARGS."
  (apply #'eask--msg 'log   "[LOG]" fmt args))

(defun eask-info (fmt &rest args)
  "Send info message; see function `eask--msg' for arguments FMT and ARGS."
  (apply #'eask--msg 'info  "[INFO]" fmt args))

(defun eask-warn (fmt &rest args)
  "Send warn message; see function `eask--msg' for arguments FMT and ARGS."
  (apply #'eask--msg 'warn  "[WARNING]" fmt args))

(defun eask-error (fmt &rest args)
  "Send error message; see function `eask--msg' for arguments FMT and ARGS."
  (apply #'eask--msg 'error "[ERROR]" fmt args))

(defun eask--msg-char-displayable (char replacement s)
  "Ensure CHAR is displayable in S; if not, we fallback to REPLACEMENT
character."
  (if (char-displayable-p (string-to-char char))
      s
    (eask-s-replace char replacement s)))

(defun eask--msg-displayable-kwds (s)
  "Make sure all keywords is displayable in S."
  (let* ((s (eask--msg-char-displayable "✓" "v" s))
         (s (eask--msg-char-displayable "✗" "X" s))
         (s (eask--msg-char-displayable "💡" "<?>" s)))
    s))

(defun eask--msg-paint-kwds (s)
  "Paint keywords from S."
  (let* ((s (eask-s-replace-ansi "✓" (ansi-green "✓") s))
         (s (eask-s-replace-ansi "✗" (ansi-red "✗") s))
         (s (eask-s-replace-ansi "💡" (ansi-yellow "💡") s)))
    s))

(defun eask--format-paint-kwds (msg &rest args)
  "Paint keywords after format MSG and ARGS."
  (let* ((s (apply #'format msg args))
         (s (eask--msg-paint-kwds s))
         (s (eask--msg-displayable-kwds s)))
    s))

(defun eask-princ (object &optional stderr)
  "Like function `princ'; with flag STDERR.

For argument OBJECT, please see function `princ' for the detials.

If optional argument STDERR is non-nil; use stderr instead."
  (unless inhibit-message
    (princ object (when stderr #'external-debugging-output))))

(defun eask-print (msg &rest args)
  "Standard output printing without newline.

For arguments MSG and ARGS, please see function `eask--format-paint-kwds' for
the detials."
  (eask-princ (apply #'eask--format-paint-kwds msg args)))

(defun eask-println (msg &rest args)
  "Like the function `eask-print' but contains the newline at the end.

For arguments MSG and ARGS, please see function `eask-print' for the detials."
  (apply #'eask-print (concat msg "\n") args))

(defun eask-msg (msg &rest args)
  "Like the function `message' but replace unicode with color.

For arguments MSG and ARGS, please see function `eask--format-paint-kwds' for
the detials."
  (apply #'eask-write msg args)
  (eask-princ "\n" t))

(defun eask-write (msg &rest args)
  "Like the function `eask-msg' but without newline at the end.

For arguments MSG and ARGS, please see function `eask-msg' for the detials."
  (eask-princ (apply #'eask--format-paint-kwds msg args) t))

(defun eask-report (&rest args)
  "Report error/warning depends on strict flag.

Argument ARGS are direct arguments for functions `eask-error' or `eask-warn'."
  (apply (if (eask-strict-p) #'eask-error #'eask-warn) args))

(defconst eask--exit-code
  `((success . 0)   ; Unused
    (failure . 1)   ; Catchall for general errors
    (misuse  . 2))
  "The exit code specification.")

(defun eask-exit-code (key)
  "Return the exit code by KEY symbol."
  (alist-get key eask--exit-code))

(defun eask--exit (&optional exit-code &rest _)
  "Kill Emacs with EXIT-CODE (default 1)."
  (kill-emacs (or (cond ((numberp exit-code) exit-code)
                        ((symbolp exit-code) (eask-exit-code exit-code)))
                  (eask-exit-code 'failure))))

(defun eask--error-status ()
  "Return error status."
  (let ((result))
    ;; Error.
    (when eask--has-error-p
      (push 'error result))
    ;; Warning.
    (when eask--has-warn-p
      (push (if (eask-strict-p)
                'error
              'warn)
            result))
    ;; No repeat.
    (delete-dups result)))

(defun eask--trigger-error (args)
  "Trigger error event.

The argument ARGS is passed from the function `eask--error'."
  (cond ((< emacs-major-version 28)
         ;; But we can remove this after Emacs 28, since function `find-library-name'
         ;; has replaced the function `signal' instead of the `error'.
         ;;
         ;; Handle https://github.com/emacs-eask/cli/issues/11.
         (unless (string-prefix-p "Can't find library " (car args))
           (setq eask--has-error-p t)))
        (t
         (setq eask--has-error-p t)))  ; Just a record.

  (when (and eask--has-error-p
             (not eask--ignore-error-p)
             (not (eask-allow-error-p))
             ;; Ignore when checking Eask-file.
             (not (eask-checker-p)))
    ;; Stop immediately.
    (eask--exit 'failure)))

(defun eask--error (fnc &rest args)
  "On error.

Arguments FNC and ARGS are used for advice `:around'."
  (let ((msg (eask--ansi 'error (apply #'format-message args))))
    (unless eask-inhibit-error-message
      (eask--unsilent (eask-msg "%s" msg)))
    (run-hook-with-args 'eask-on-error-hook 'error msg)
    (eask--trigger-error args))
  (when debug-on-error (apply fnc args)))

(defun eask--trigger-warn ()
  "Trigger warning event."
  (setq eask--has-warn-p t))

(defun eask--warn (fnc &rest args)
  "On warn.

Arguments FNC and ARGS are used for advice `:around'."
  (let ((msg (eask--ansi 'warn (apply #'format-message args))))
    (unless eask-inhibit-error-message
      (eask--unsilent (eask-msg "%s" msg)))
    (run-hook-with-args 'eask-on-warning-hook 'warn msg)
    (eask--trigger-warn))
  (eask--silent (apply fnc args)))

(defconst eask-log-path ".log"
  "Directory path to create log files.")

(defun eask-files-spec ()
  "Return files spec."
  (or eask-files package-build-default-files-spec))

(defun eask-expand-file-specs (specs)
  "Expand file SPECS."
  (mapcar (lambda (elm) (expand-file-name (car elm) default-directory))
          (ignore-errors  ; The new files spec will trigger error, wrap it
            (package-build-expand-files-spec nil nil default-directory specs))))

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
  (when-let* ((elcs (mapcar (lambda (elm) (concat elm "c")) (eask-package-el-files))))
    (setq elcs (cl-remove-if-not (lambda (elm) (file-exists-p elm)) elcs))
    elcs))

(defun eask-package-multi-p ()
  "Return t if multi-files package."
  (or (bound-and-true-p package-build-build-function)
      (< 1 (length (eask-package-files)))))

(defun eask-package-single-p ()
  "Return t if single file package."
  (not (eask-package-multi-p)))

(defun eask-unpacked-size ()
  "Return unpacked size."
  (let ((size 0))
    (dolist (filename (eask-package-files))
      (cl-incf size (file-attribute-size (file-attributes filename))))
    (string-trim (ls-lisp-format-file-size size t))))

(defun eask--help-display ()
  "Display help instruction."
  (goto-char (point-min))
  (let ((max-column 0))
    (while (not (eobp))
      (forward-line 1)
      (beginning-of-line)
      (insert "    ")
      (end-of-line)
      (setq max-column (max (current-column) max-column)))
    (eask-msg (concat "''" (spaces-string max-column) "''"))
    (eask-msg (buffer-string))
    (eask-msg (concat "''" (spaces-string max-column) "''"))))

(defun eask-help (command &optional print-or-exit-code)
  "Show COMMAND's help instruction.

When the optional variable PRINT-OR-EXIT-CODE is a number, it will exit with
that code.  Set to non-nil would just print the help message without sending
the exit code.  The default value `nil' will be replaced by `1'; therefore
would send exit code of `1'."
  (let* ((command (eask-2str command))  ; convert to string
         (help-file (concat eask-lisp-root "help/" command))
         ;; The default exit code is `2' since `eask-help' prints the help
         ;; message on user error 99% of the time.
         (print-or-exit-code (or print-or-exit-code
                                 (eask-exit-code 'misuse))))
    (if (file-exists-p help-file)
        (with-temp-buffer
          (insert-file-contents help-file)
          (unless (string-empty-p (buffer-string))
            (let ((buf-str (eask--msg-displayable-kwds (buffer-string))))
              (erase-buffer)
              (insert buf-str))
            (eask--help-display))
          ;; Exit with code if needed
          (cond ((numberp print-or-exit-code)
                 (eask--exit print-or-exit-code))
                (t )))  ; Don't exit with anything else.
      (eask-error "✗ Help manual missing `%s`" help-file))))

(defun eask--checker-existence ()
  "Return errors if required metadata is missing."
  (unless eask-package
    (eask-error
     (concat "✗ Missing metadata package; make sure you have created "
             "an Eask-file with `$ eask init`!"))))

(defun eask--check-strings (fmt f p &rest args)
  "Test strings (F and P); then print FMT and ARGS if not equal."
  (unless (string= f p) (apply #'eask-warn (append (list fmt f p) args))))

(defun eask--check-optional (f p msg1 msg2 msg3 msg4)
  "Conditional way to check optional headers, URL and KEYWORDS.

For arguments F and P, please see function `eask--check-strings' for more
information.

Arguments MSG1, MSG2, MSG3 and MSG4 are conditional messages."
  (cond ((and f p) (eask--check-strings msg1 f p))
        (f (eask-warn msg2))
        (p (eask-warn msg3))
        (t (eask-warn msg4))))

(defun eask--checker-metadata ()
  "Report warnings if metadata doesn't match."
  (when-let* (((and eask-package eask-package-desc))
              (def-point (if (eask-pkg-el) "-pkg.el file" "package-file")))
    (eask--check-strings
     "💡 Unmatched package name `%s`; it should be `%s`"
     (eask-package-name) (package-desc-name eask-package-desc))
    (when-let* ((ver-eask (eask-package-version))
                (ver-pkg (package-desc-version eask-package-desc))
                ;; `package-version-join' returns only one of the possible
                ;; inverses, since `version-to-list' is a many-to-one operation
                ((not (equal (version-to-list ver-eask) ver-pkg))))
      (eask--check-strings
       "💡 Unmatched version `%s`; it should be `%s`"
       ver-eask (package-version-join ver-pkg)))
    (eask--check-strings
     "💡 Unmatched summary `%s`; it should be `%s`"
     (eask-package-description) (package-desc-summary eask-package-desc))
    (let ((url (eask-package-desc-url)))
      (eask--check-optional
       eask-website-url url
       "💡 Unmatched website URL `%s`; it should be `%s`"
       (format "💡 Unmatched website URL `%s`; add `%s` to %s" eask-website-url
               (if (string-prefix-p "-pkg.el" def-point)
                   (format ":url \"%s\"" eask-website-url)
                 (format ";; URL: %s" eask-website-url))
               def-point)
       (format "💡 Unmatched website URL `%s`; add `(website-url \"%s\")` to Eask-file" url url)
       (format "💡 URL header is optional, but it's often recommended")))
    (let ((keywords (eask-package-desc-keywords)))
      (cond
       ((or keywords eask-keywords)
        (dolist (keyword keywords)
          (unless (member keyword eask-keywords)
            (eask-warn "💡 Unmatched keyword `%s`; add `(keywords \"%s\")` to Eask-file or consider removing it" keyword keyword)))
        (dolist (keyword eask-keywords)
          (unless (member keyword keywords)
            (eask-warn "💡 Unmatched keyword `%s`; add `%s` to %s or consider removing it"
                       keyword
                       (if (string-prefix-p "-pkg.el" def-point)
                           (format ":keywords '(\"%s\")" keyword)
                         (format ";; Keywords: %s" keyword))
                       def-point))))
       (t
        (eask-warn "💡 Keywords header is optional, but it's often recommended"))))
    (let* ((dependencies (append eask-depends-on-emacs eask-depends-on))
           (dependencies (mapcar #'car dependencies))
           (dependencies (mapcar (lambda (elm) (eask-2str elm)) dependencies))
           (requirements (package-desc-reqs eask-package-desc))
           (requirements (mapcar #'car requirements))
           (requirements (mapcar (lambda (elm) (eask-2str elm)) requirements)))
      (dolist (req requirements)
        (unless (member req dependencies)
          (eask-warn "💡 Unmatched dependency `%s`; add `(depends-on \"%s\")` to Eask-file or consider removing it" req req)))
      (dolist (dep dependencies)
        (unless (member dep requirements)
          (eask-warn "💡 Unmatched dependency `%s`; add `(%s \"VERSION\")` to %s or consider removing it" dep dep def-point))))))

(defun eask--checker-string (name var)
  "Run checker for package's metadata.

Argument NAME represent the name of that package's metadata.  VAR is the actual
variable we use to test validation."
  (unless (stringp var)
    (eask-error "✗ %s must be a string" name))
  (when (string-empty-p var)
    (eask-warn "💡 %s cannot be an empty string" name)))

(defun eask-lint-first-newline ()
  "Built-in linters will create extra newline, prevent that!"
  (when eask-lint-first-file-p
    (eask-msg ""))
  (setq eask-lint-first-file-p t))

;; ~/lisp/clean/all.el

(defvar eask-no-cleaning-operation-p nil
  "Set to non-nil if there is no cleaning operation done.")

(defvar eask-clean-all--tasks-count 0
  "Count cleaning task.")

(defvar eask-clean-all--tasks-cleaned 0
  "Total cleaned tasks.")

(defmacro eask--clean-section (title &rest body)
  "Print clean up TITLE and execute BODY."
  (declare (indent 1))
  `(let (eask-no-cleaning-operation-p)
     (cl-incf eask-clean-all--tasks-count)
     (eask-with-progress
       (concat (format "  - [%s/%s] " eask-clean-all--tasks-count eask-clean-all--tasks-total)
               (format "%s... " ,title))
       (eask-with-verbosity 'debug ,@body)
       (if eask-no-cleaning-operation-p
           "skipped ✗"
         (cl-incf eask-clean-all--tasks-cleaned)
         "done ✓"))))

(defconst eask-clean-all--tasks-total 6
  "Count cleaning task.")

;; ~/lisp/clean/autoloads.el

;; ~/lisp/clean/dist.el

(defun eask-clean-dist (path)
  "Clean up dist PATH."
  (let* ((name (eask-guess-package-name))
         (version (eask-package-version))
         (readme (expand-file-name (format "%s-readme.txt" name) path))
         (entry (expand-file-name (format "%s-%s.entry" name version) path))
         (packaged (eask-package-packaged-file))
         (deleted 0)
         (delete-dir))
    (when (eask-delete-file readme)   (cl-incf deleted))
    (when (eask-delete-file entry)    (cl-incf deleted))
    (when (eask-delete-file packaged) (cl-incf deleted))
    (when (and (not (zerop deleted)) (eask-directory-empty-p path))
      (eask-with-progress
        (format "The dist folder %s seems to be empty, delete it as well... " path)
        (ignore-errors (delete-directory path))
        "done ✓")
      (setq delete-dir t))
    (eask-msg "")
    (eask-info "(Total of %s file%s and %s directory deleted, %s skipped)" deleted
               (eask--sinr deleted "" "s")
               (if delete-dir "1" "0")
               (- 3 deleted))))

;; ~/lisp/clean/elc.el

;; ~/lisp/clean/log-file.el

(defun eask-clean-log-file (path)
  "Clean up .log PATH."
  (let ((log-files '("messages.log"
                     "warnings.log"
                     "backtrace.log"
                     "compile-log.log"))
        (deleted 0)
        (delete-dir))
    (dolist (log-file log-files)
      (when (eask-delete-file (expand-file-name log-file path))
        (cl-incf deleted)))
    (when (and (not (zerop deleted)) (eask-directory-empty-p path))
      (eask-with-progress
        (format "The dist folder %s seems to be empty, delete it as well... " path)
        (ignore-errors (delete-directory path))
        "done ✓")
      (setq delete-dir t))
    (eask-msg "")
    (eask-info "(Total of %s log file%s and %s directory deleted, %s skipped)"
               deleted
               (eask--sinr deleted "" "s")
               (if delete-dir "1" "0")
               (- (length log-files) deleted))))

;; ~/lisp/clean/pkg-file.el

;; ~/lisp/clean/workspace.el

;; ~/lisp/core/analyze.el
(defvar eask-analyze--log nil)
(defvar eask-analyze--warnings nil)
(defvar eask-analyze--errors nil)
(defvar eask-analyze--warning-p nil)
(defvar eask-analyze--error-p nil)

(defun eask-analyze--pretty-json (json)
  "Return pretty JSON."
  (with-temp-buffer (insert json) (json-pretty-print-buffer) (buffer-string)))

(defun eask-analyze--load-buffer ()
  "Return the current file loading session."
  (car (cl-remove-if-not (lambda (elm)
                           (string-prefix-p " *load*-" (buffer-name elm)))
                         (buffer-list))))

(defun eask-analyze--write-json-format (level msg)
  "Prepare log for JSON format.

For arguments LEVEL and MSG, please see function `eask-analyze--write-log' for more
information."
  (let* ((bounds (bounds-of-thing-at-point 'sexp))
         (filename (or load-file-name eask-file))
         (start (car bounds))
         (end (cdr bounds))
         (start-line (if load-file-name (line-number-at-pos start) 0))
         (start-col  (if load-file-name (eask--column-at-point start) 0))
         (start-pos  (if load-file-name start 0))
         (end-line   (if load-file-name (line-number-at-pos end) 0))
         (end-col    (if load-file-name (eask--column-at-point end) 0))
         (end-pos    (if load-file-name end 0))
         (msg (ansi-color-filter-apply msg)))
    (push `((range . ((start . ((line . ,start-line)
                                (col  . ,start-col)
                                (pos  . ,start-pos)))
                      (end . ((line . ,end-line)
                              (col  . ,end-col)
                              (pos  . ,end-pos)))))
            (filename . ,filename)
            (message  . ,msg))
          (cl-case level
            (`error eask-analyze--errors)
            (`warn  eask-analyze--warnings)))))

(defun eask-analyze--write-plain-text (level msg)
  "Prepare log for plain text format.

For arguments LEVEL and MSG, please see function `eask-analyze--write-log' for more
information."
  (let* ((level-string (cl-case level
                         (`error "Error")
                         (`warn  "Warning")))
         (log (format "%s:%s:%s %s: %s"
                      (or load-file-name eask-file)
                      (if load-file-name (line-number-at-pos) 0)
                      (if load-file-name (current-column) 0)
                      level-string
                      msg)))
    (push (ansi-color-filter-apply log) eask-analyze--log)))

(defun eask-analyze--write-log (level msg)
  "Write the log.

Argument LEVEL and MSG are data from the debug log signal."
  (unless (string= " *temp*" (buffer-name))  ; avoid error from `package-file' directive
    (cl-case level
      (`error (setq eask-analyze--error-p t))
      (`warn  (setq eask-analyze--warning-p t)))
    (with-current-buffer (or (eask-analyze--load-buffer) (buffer-name))
      (funcall
       (cond ((eask-json-p) #'eask-analyze--write-json-format)
             (t             #'eask-analyze--write-plain-text))
       level msg))))

(defun eask-analyze--file (files)
  "Lint list of Eask FILES."
  (let (checked-files content)
    ;; Linting
    (dolist (file files)
      (eask--silent-error
        (eask--save-load-eask-file file
            (push file checked-files)
          ;; also count files with errors in the total count
          (push file checked-files))))

    ;; Print result
    (eask-msg "")
    (cond ((eask-json-p)  ; JSON format
           ;; Fill content with result.
           (when (or eask-analyze--warnings eask-analyze--errors)
             (setq content
                   (eask-analyze--pretty-json (json-encode
                                               `((warnings . ,eask-analyze--warnings)
                                                 (errors   . ,eask-analyze--errors))))))
           ;; XXX: When printing the result, no color allow.
           (eask--with-no-color
             (eask-msg (or content "{}"))))
          (eask-analyze--log  ; Plain text
           (setq content
                 (with-temp-buffer
                   (dolist (msg (reverse eask-analyze--log))
                     (insert msg "\n"))
                   (buffer-string)))
           ;; XXX: When printing the result, no color allow.
           (eask--with-no-color
             (mapc #'eask-msg (reverse eask-analyze--log)))))

    (eask-info "(Checked %s file%s)"
               (length checked-files)
               (eask--sinr checked-files "" "s"))

    ;; Output file
    (when (and content (eask-output))
      (write-region content nil (eask-output)))))

;; ~/lisp/core/archives.el
(defvar eask-archive--length-name)
(defvar eask-archive--length-url)
(defvar eask-archive--length-priority)

(defun eask-archive--print (archive)
  "Print the ARCHIVE."
  (let* ((name (car archive))
         (url (cdr archive))
         (priority (assoc name package-archive-priorities))
         (priority (cdr priority)))
    (eask-println
     (concat "  %-" eask-archive--length-name "s  %-" eask-archive--length-url
             "s  %-" eask-archive--length-priority "s")
     name (eask-2url url) (or priority 0))))

(defun eask-archive--print-alist (alist)
  "Print the archvie ALIST."
  (let* ((names (mapcar #'car alist))
         (eask-archive--length-name (eask-2str (eask-seq-str-max names)))
         (urls (mapcar #'cdr alist))
         (eask-archive--length-url (eask-2str (eask-seq-str-max urls)))
         (priorities (mapcar #'cdr package-archive-priorities))
         (eask-archive--length-priority (eask-2str (eask-seq-str-max priorities))))
    (mapc #'eask-archive--print alist)))

;; ~/lisp/core/bump.el

(defun eask-bump--version (version index)
  "Bump VERSION with INDEX."
  (let ((lst (if (stringp version) (version-to-list version) version)))
    (setf (nth index lst) (cl-incf (nth index lst)))
    (mapconcat #'eask-2str lst version-separator)))

(defun eask-bump--version-major-version (version)
  "Bump VERSION major level."
  (eask-bump--version version 0))

(defun eask-bump--version-minor-version (version)
  "Bump VERSION minor level."
  (eask-bump--version version 1))

(defun eask-bump--version-patch-level (version)
  "Bump VERSION patch level."
  (eask-bump--version version 2))

;; ~/lisp/core/cat.el

;; ~/lisp/core/compile.el
(require 'bytecomp nil t)

(defun eask-compile--print-log ()
  "Print `*Compile-Log*' buffer."
  (when (get-buffer byte-compile-log-buffer)
    (with-current-buffer byte-compile-log-buffer
      (if (and (eask-clean-p) (eask-strict-p))
          (eask-error (buffer-string))  ; Exit with error code!
        (eask-print-log-buffer))
      (eask-msg ""))))

(defun eask-compile--byte-compile-file-external-content (filename cmd)
  "Extract result after executing byte-compile the FILENAME.

The CMD is the command to start a new Emacs session."
  (with-temp-buffer
    (insert (shell-command-to-string cmd))
    (goto-char (point-min))
    (search-forward filename nil t)
    (re-search-forward "[ \t\r\n]" nil t)
    (let ((line (string-trim (thing-at-point 'line))))
      (if (and (string-prefix-p "Compiling " line)
               (or (string-match-p "... skipped" line)
                   (string-match-p "... done" line)))
          (delete-region (point-min) (line-end-position 1))
        (delete-region (point-min) (point))))
    (when (search-forward "(Total of " nil t)
      (goto-char (point-max))
      (delete-region (line-beginning-position -1) (point-max)))
    (string-trim (buffer-string))))

(defun eask-compile--byte-compile-file-external (filename)
  "Byte compile FILENAME with clean environment by opening a new Emacs session."
  (let* ((cmd (split-string eask-invocation "\n" t))
         (cmd (format "\"%s\""(mapconcat #'identity cmd "\" \"")))
         (args (eask-args))
         (argv (cl-remove-if
                (lambda (arg)
                  (or (string= "--clean" arg)  ; prevent infinite call
                      (member arg args)))      ; remove repeated arguments
                (eask-argv-out)))
         (args (append `(,(eask-command) ,(concat "\"" filename "\"")) argv))
         (args (mapconcat #'identity args " "))
         (cmd (concat cmd " " args))
         (content (eask-compile--byte-compile-file-external-content filename cmd)))
    (if (string-empty-p content)
        t  ; no error, good!
      (with-current-buffer (get-buffer-create byte-compile-log-buffer)
        (insert content)))))

(defun eask-compile--byte-compile-file (filename)
  "Byte compile FILENAME."
  ;; *Compile-Log* does not kill itself. Make sure it's clean before we do
  ;; next byte-compile task.
  (ignore-errors (kill-buffer byte-compile-log-buffer))
  (let* ((filename (expand-file-name filename))
         (result))
    (eask-with-progress
      (unless byte-compile-verbose (format "Compiling %s... " filename))
      (eask-with-verbosity 'debug
        (setq result (if (eask-clean-p)
                         (eask-compile--byte-compile-file-external filename)
                       (byte-compile-file filename))
              result (eq result t)))
      (unless byte-compile-verbose (if result "done ✓" "skipped ✗")))
    (eask-compile--print-log)
    result))

(defun eask-compile--files (files)
  "Compile sequence of FILES."
  (let* ((compiled (cl-remove-if-not #'eask-compile--byte-compile-file files))
         (compiled (length compiled))
         (skipped (- (length files) compiled)))
    ;; XXX: Avoid last newline from the log buffer!
    (unless (get-buffer byte-compile-log-buffer)
      (eask-msg ""))
    (eask-info "(Total of %s file%s compiled, %s skipped)" compiled
               (eask--sinr compiled "" "s")
               skipped)))

;; ~/lisp/core/concat.el

;; ~/lisp/core/docs.el
(require 'el2org nil t)

(defun eask-docs--to-html (el-file)
  "Generate html file from EL-FILE."
  (interactive)
  (let* ((filename (file-name-nondirectory el-file))
         (html-file (expand-file-name (concat (file-name-sans-extension filename)
                                              ".html")
                                      eask-docs-path)))
    (eask-with-verbosity 'debug
      (el2org-generate-file el-file nil 'html html-file t))))

;; ~/lisp/core/emacs.el

;; ~/lisp/core/eval.el

;; ~/lisp/core/exec-path.el

(defun eask-exec-path--print (path)
  "Print out the PATH."
  (eask-println "%s" path))

;; ~/lisp/core/exec.el

(defconst eask-exec--exec-path-file (expand-file-name "exec-path" eask-homedir)
  "Target file to export the variable `exec-path'.")

(defconst eask-exec--load-path-file (expand-file-name "load-path" eask-homedir)
  "Target file to export the variable `load-path'.")

(defun eask-exec-export-env ()
  "Export environments."
  (ignore-errors (delete-file eask-exec--exec-path-file))
  (ignore-errors (delete-file eask-exec--load-path-file))
  (ignore-errors (make-directory eask-homedir t))  ; generate dir `~/.eask/'
  (write-region (getenv "PATH") nil eask-exec--exec-path-file)
  (write-region (getenv "EMACSLOADPATH") nil eask-exec--load-path-file))

;; ~/lisp/core/files.el

(defun eask-files--print-filename (filename)
  "Print out the FILENAME."
  (eask-println "%s" filename))

;; ~/lisp/core/info.el

(defvar eask-info--max-offset 0
  "The maximum offset to print the info.")

(defun eask-info--print-deps (title dependencies)
  "Print DEPENDENCIES with TITLE identifier."
  (when dependencies
    (eask-println "")
    (eask-println title)
    (let* ((names (mapcar (lambda (dep)
                            (ansi-green (eask-2str (car dep))))
                          dependencies))
           (offset (eask-seq-str-max names)))
      (setq offset (if (eask-no-color-p) offset
                     ;; XXX: I'm not sure why we need to plus 2 here.
                     ;; My guess is regarding the ansi escape characters.
                     (+ offset 2))
            eask-info--max-offset (max offset eask-info--max-offset)
            offset (eask-2str eask-info--max-offset))
      (dolist (dep dependencies)
        (let* ((target-version (cdr dep))
               (target-version (cond ((memq :file dep) "file")
                                     ((memq :vc dep)   "vc")
                                     ((memq :try dep)  "try")
                                     ((= (length target-version) 1)
                                      (or (nth 0 target-version)  ; verison number
                                          "archive"))
                                     (t                "recipe"))))
          (eask-println (concat "  %-" offset "s (%s) %s")
                        (ansi-green (eask-2str (car dep)))
                        (ansi-yellow target-version)
                        ;; Debug print the recipe format.
                        (if (eask-reach-verbosity-p 'debug)
                            (ansi-blue (eask-2str dep))
                          "")))))))

;; ~/lisp/core/init.el

(defun eask-init--check-filename (name)
  "Return non-nil if NAME is a valid Eask-file."
  (when-let* ((name (file-name-nondirectory (directory-file-name name)))
              (prefix (cond ((string-prefix-p "Easkfile" name) "Easkfile")
                            ((string-prefix-p "Eask" name)     "Eask"))))
    (let ((suffix (car (split-string name prefix t))))
      (or (null suffix)
          (string-match-p "^[.][.0-9]*$" suffix)))))

;; ~/lisp/core/install-deps.el

;; ~/lisp/core/install-file.el

(defun eask-install-file--get-package-name (path)
  "Get the package name from PATH, which is a file, directory or archive."
  (let ((path (expand-file-name path)))
    (cond
     ((not (file-exists-p path))
      (eask-error "✗ File does not exist in `%s`" path))
     ;; TAR file
     ((string-match-p "[.]+tar[.]*" path)
      ;; Note this can throw strange errors if
      ;;
      ;; - there is no -pkg.el in the tar file
      ;; - the tar file was built in a folder with a different name
      ;;
      ;; TAR files created with eask package are fine.
      (require 'tar-mode)
      (let ((pkg-desc (with-temp-buffer
                        (insert-file-contents-literally path)
                        (tar-mode)
                        (ignore-errors (package-tar-file-info)))))
        (unless pkg-desc
          ;; `package-dir-info' will return nil if there is no `-pkg.el'
          ;; and no `.el' files at path
          (eask-error "✗ No package in `%s`" path))
        (package-desc-name pkg-desc)))
     ;; .el file or directory
     (t
      ;; Note `package-dir-info' doesn't work outside of dired mode!
      (let ((pkg-desc (with-temp-buffer
                        (dired path)
                        (ignore-errors (package-dir-info)))))
        (unless pkg-desc
          ;; `package-dir-info' will return nil if there is no `-pkg.el'
          ;; and no `.el' files at path
          (eask-error "✗ No package in `%s`" path))
        (package-desc-name pkg-desc))))))

(defun eask-install-file--packages (files)
  "The file install packages with FILES."
  (let* ((deps (mapcar (lambda (file)
                         (list (eask-install-file--get-package-name file) file))
                       files))
         (names (mapcar #'car deps))
         (len (length deps))
         (s (eask--sinr len "" "s"))
         (not-installed (eask-install--not-installed names))
         (installed (length not-installed))
         (skipped (- len installed)))
    (eask-log "Installing %s specified file package%s..." len s)
    (eask-msg "")
    (eask--package-mapc (lambda (dep &rest _)
                          (apply #'eask-package-install-file dep))
                        deps)
    (eask-msg "")
    (eask-info "(Total of %s file package%s installed, %s skipped)"
               installed s skipped)))

;; ~/lisp/core/install-vc.el

(defun eask-install-vc--guess-name (file)
  "Guess the package name of the install FILE."
  (file-name-sans-extension (file-name-nondirectory (directory-file-name file))))

(defun eask-install-vc--split-sepcs (specs)
  "Split the SPECS and return a list of specification."
  (let ((new-specs)
        (current-spec))
    (dolist (spec specs)
      ;; Detect new specification.
      (cond ((ffap-url-p spec)
             (push (reverse current-spec) new-specs)
             ;; We're using the push, so the order is reversed.
             (setq current-spec (list spec (eask-install-vc--guess-name spec))))
            (t
             (push spec current-spec))))
    ;; Push thes rest of the specification.
    (push (reverse current-spec) new-specs)
    (cl-remove-if #'null (reverse new-specs))))

(defun eask-install-vc--packages (specs)
  "The vc install packages with SPECS."
  (let* ((deps (eask-install-vc--split-sepcs specs))
         (names (mapcar #'car deps))
         (len (length deps))
         (s (eask--sinr len "" "s"))
         (not-installed (eask-install--not-installed names))
         (installed (length not-installed))
         (skipped (- len installed)))
    (eask-log "Installing %s specified vc package%s..." len s)
    (eask-msg "")
    (eask--package-mapc (lambda (dep &rest _)
                          (let ((name (car dep))
                                (spec (cdr dep)))
                            (eask-package-vc-install name spec)))
                        deps)
    (eask-msg "")
    (eask-info "(Total of %s vc package%s installed, %s skipped)"
               installed s skipped)))

;; ~/lisp/core/install.el

(defun eask-install--not-installed (names)
  "Return a list of not installed packages' NAMES."
  (cl-remove-if-not
   (lambda (name)
     (or (eask-force-p)
         (not (package-installed-p (eask-intern name)))))
   names))

(defun eask-install--packages (names)
  "Install packages with their NAMES."
  (let* ((names (mapcar #'eask-intern names))
         (len (length names))
         (s (eask--sinr len "" "s"))
         (not-installed (eask-install--not-installed names))
         (installed (length not-installed))
         (skipped (- len installed)))
    (eask-log "Installing %s specified package%s..." len s)
    (eask-msg "")
    (eask--package-mapc #'eask-package-install names)
    (eask-msg "")
    (eask-info "(Total of %s package%s installed, %s skipped)"
               installed s skipped)))

(defun eask-install--file (file)
  "Old compatible version of function `package-install-file'.

For argument FILE, please see function `package-install-file' for the details."
  ;; Workaround: `package-install-file' fails when FILE is .el and contains CRLF EOLs:
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=48137
  (if (not (string-match "\\.el\\'" file))
      (package-install-file file)

    ;; load package file and check if it contains CRLFs
    (with-temp-buffer
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (if (not (search-forward "\r\n" nil t))
          (package-install-file file)  ; no CRLF

        ;; CRLF found
        (let* ((nondir (file-name-nondirectory file))
               (temp-dir (make-temp-file "eask" t))
               (temp-file (expand-file-name nondir temp-dir)))

          (unwind-protect
              ;; replace CRLFs with LFs and write to new temporary
              ;; package file
              (progn
                (replace-match "\n" nil t)
                (while (search-forward "\r\n" nil t)
                  (replace-match "\n" nil t))
                (write-region (point-min) (point-max) temp-file)

                (package-install-file temp-file))

            ;; clean up temporary file
            (delete-directory temp-dir t)))))))

;; ~/lisp/core/keywords.el
(require 'finder nil t)

;; ~/lisp/core/list.el
(defvar eask-list--pkg-name-offset nil)
(defvar eask-list--pkg-version-offset nil)
(defvar eask-list--pkg-archive-offset nil)

(defun eask-list--format-s (offset)
  "Format OFFSET."
  (concat " %-" (number-to-string offset) "s "))

(defun eask-list--align (depth &optional rest)
  "Format string to align starting from the version number.

Argument DEPTH is used to calculate the indentation.  REST is a list of string
for string concatenation."
  (let ((prefix (if (= depth 0) "[+]" "[+]")))
    (concat (spaces-string (* depth 2))  ; indent for depth
            " " prefix
            (eask-list--format-s (- eask-list--pkg-name-offset (* depth 2)))
            (eask-list--format-s eask-list--pkg-version-offset)
            (eask-list--format-s eask-list--pkg-archive-offset)
            rest)))

(defun eask-list--print-pkg (name depth max-depth pkg-alist)
  "Print NAME package information.

Argument DEPTH is the current dependency nested level.  MAX-DEPTH is the
limitation, so we don't go beyond this deepness.  PKG-ALIST is the archive
contents."
  (when-let*
      ((pkg (assq name pkg-alist))
       (desc (cadr pkg))
       (name (package-desc-name desc))
       (version (package-desc-version desc))
       (version (package-version-join version))
       (archive (or (package-desc-archive desc) ""))
       (summary (package-desc-summary desc)))
    (if (= depth 0)
        (eask-msg (eask-list--align depth " %-80s") name version archive summary)
      (eask-msg (eask-list--align depth) name "" "" ""))
    (when-let* ((reqs (package-desc-reqs desc))
                ((< depth max-depth)))
      (dolist (req reqs)
        (eask-list--print-pkg (car req) (1+ depth) max-depth pkg-alist)))))

(defun eask-list--version-list (pkg-alist)
  "Return a list of versions.

PKG-ALIST is the archive contents."
  (mapcar (lambda (elm)
            (package-version-join (package-desc-version (cadr elm))))
          pkg-alist))

(defun eask-list--archive-list (pkg-alist)
  "Return list of archives.

PKG-ALIST is the archive contents."
  (mapcar (lambda (elm)
            (or (package-desc-archive (cadr elm)) ""))
          pkg-alist))

(defun eask-list (list pkg-alist &optional depth)
  "List packages.

Argument LIST is the list of packages we want to list.  PKG-ALIST is the archive
contents we want to retrieve package's metadate from.  Optional argument DEPTH
is the deepness of the dependency nested level we want to go."
  (let* ((eask-list--pkg-name-offset (eask-seq-str-max list))
         (version-list (eask-list--version-list pkg-alist))
         (eask-list--pkg-version-offset (eask-seq-str-max version-list))
         (archive-list (eask-list--archive-list pkg-alist))
         (eask-list--pkg-archive-offset (eask-seq-str-max archive-list)))
    (dolist (name list)
      (eask-list--print-pkg name 0 (or depth (eask-depth) 999) pkg-alist))))

;; ~/lisp/core/load-path.el

(defun eask-load-path--print (path)
  "Print out the PATH."
  (eask-println "%s" path))

(defun eask-load-path--filter (path)
  "Filter the PATH out by search regex."
  (cl-some (lambda (regex)
             (string-match-p regex path))
           (eask-args)))

;; ~/lisp/core/load.el

;; ~/lisp/core/loc.el
(defvar eask-loc--lines 0)
(defvar eask-loc--chars 0)

(defun eask-loc--file (file)
  "Insert LOC information for FILE."
  (unless (file-directory-p file)
    (let ((lines) (chars))
      (with-temp-buffer
        (insert-file-contents file)
        (setq lines (line-number-at-pos (point-max))
              chars (point-max)))
      (cl-incf eask-loc--lines lines)
      (cl-incf eask-loc--chars chars)
      (insert (format "| %s | %s | %s |\n" file lines chars)))))

;; ~/lisp/core/outdated.el

;; ~/lisp/core/package-directory.el

;; ~/lisp/core/package.el
(declare-function package-directory-recipe "ext:package-recipe.el")

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

(defun eask-package-dir-recipe (version)
  "Form a directory recipe.

Argument VERSION is a string represent the version number of this package."
  (eask-load "extern/package-recipe")
  (let* ((name (eask-guess-package-name))
         (patterns (eask-package-dir--patterns))
         (path default-directory)
         (rcp (package-directory-recipe name :name name :files patterns :dir path)))
    (setf (slot-value rcp 'version) version)
    (setf (slot-value rcp 'time) (eask-current-time))
    rcp))

(defun eask-package-packaged-name ()
  "Find a possible packaged name."
  (let ((name (eask-guess-package-name))
        (version (eask-package-version)))
    (concat name "-" version)))

(defun eask-package--packaged-file (ext)
  "Find a possible packaged file with extension (EXT)."
  (expand-file-name (concat (eask-package-packaged-name) "." ext) eask-dist-path))

(defun eask-package-packaged-file ()
  "Return generated package artifact; it could be a tar or el."
  (if (eask-package-multi-p) (eask-package--packaged-file "tar")
    (eask-package--packaged-file "el")))

;; ~/lisp/core/recipe.el

(defun eask-recipe-string ()
  "Return the recipe format in string."
  (when-let* ((url (eask-package-desc-url)))
    (let* ((fetcher (cond ((string-match-p "github.com" url) 'github)
                          ((string-match-p "gitlab.com" url) 'gitlab)
                          (t 'git)))
           (url-regex (if (eq fetcher 'github)
                          "http[s]://github.com/"
                        "http[s]://gitlab.com/"))
           (repo (replace-regexp-in-string url-regex "" url))
           (name (eask-guess-package-name))
           (recipe `(,(intern name) :fetcher ,fetcher)))
      (cond ((memq fetcher '(git hg))
             (nconc recipe `(:url ,url)))
            ((memq fetcher '(gitlab github))
             (nconc recipe `(:repo ,repo))))
      (when eask-files
        (nconc recipe `(:files ,(append '(:defaults) eask-files))))
      recipe)))

;; ~/lisp/core/recompile.el

;; ~/lisp/core/refresh.el

;; ~/lisp/core/reinstall.el

(defun eask-reinstall--packages (names)
  "Install packages by its NAMES."
  (let* ((names (mapcar #'eask-intern names))
         (len (length names))
         (s (eask--sinr len "" "s"))
         (installed (cl-remove-if-not #'package-installed-p names))
         (installed (length installed))
         (skipped (- len installed)))
    (eask-log "Reinstalling %s specified package%s..." len s)
    (eask-msg "")
    (eask--package-mapc #'eask-package-reinstall names)
    (eask-msg "")
    (eask-info "(Total of %s package%s reinstalled, %s skipped)"
               installed s skipped)))

;; ~/lisp/core/repl.el

(defvar eask-repl--old-pos nil
  "Record the last position to output on the screen for the `ielm' buffer.")

(defun eask-repl--output ()
  "Print the REPL result to screen."
  (unless eask-repl--old-pos (setq eask-repl--old-pos (point-min)))
  (with-current-buffer "*ielm*"
    (goto-char eask-repl--old-pos)
    (while (not (eobp))
      (let ((line (thing-at-point 'line t)))
        (unless (string-prefix-p "ELISP> " line)
          (eask-print line)))
      (forward-line 1)
      (end-of-line))
    ;; Record last output position
    (setq eask-repl--old-pos (point))))

;; ~/lisp/core/search.el

(defun eask-search--packages (query)
  "Filter available packages with QUERY."
  (let ((result))
    (dolist (package (mapcar #'car package-archive-contents))
      (when (string-match-p query (eask-2str package))
        (push package result)))
    result))

;; ~/lisp/core/status.el
(declare-function ansi-bright-black "ext:ansi.el")
(declare-function ansi-underscore "ext:ansi.el")

(defvar eask-status--info-count 0
  "Count of the stratus info.")

(defun eask-status--environment-name ()
  "Get the working environment name."
  (cond ((eask-global-p) "global (~/)")
        ((eask-config-p) (format "configuration (%s)" user-emacs-directory))
        (t               "development (./)")))

(defun eask-status--print-title (title)
  "Print section TITLE."
  (eask-println "")
  (eask-println (ansi-underscore title))
  (eask-println ""))

(defun eask-status--print-info (fmt pair)
  "Print environment info with FMT and PAIR."
  (let ((title   (eask-2str (nth 0 pair)))
        (content (eask-2str (nth 1 pair)))
        (note    (eask-2str (or (nth 2 pair) ""))))
    (eask-println fmt
                  title
                  (ansi-bright-black content)
                  note)))

(defun eask-status--list-max-length (lst index)
  "Return the LST max length by its INDEX."
  (let ((max-len 0)
        (max-current))
    (dolist (data lst)
      (setq max-current (eask-2str (nth index data))
            max-current (pcase index
                          (1 (ansi-bright-black max-current))
                          (_ max-current))
            max-len (max (length max-current) max-len)))
    max-len))

(defun eask-status--print-infos (lst)
  "Print environment info LST."
  (let* ((len-0 (eask-2str (eask-status--list-max-length lst 0)))  ; unused
         (len-1 (eask-2str (+ (eask-status--list-max-length lst 1) 2)))
         (fmt (concat "   %-24s   %-" len-1 "s   %s")))
    (dolist (pair lst)
      (when pair
        (eask-status--print-info fmt pair)
        (cl-incf eask-status--info-count)))))

(defun eask-status--file-dir (path)
  "Return file directory status from PATH."
  (unless (file-exists-p path)
    (ansi-red "(missing)")))

;; ~/lisp/core/uninstall.el

(defun eask-uninstall--packages(names)
  "Uninstall packages by its NAMES."
  (let* ((names (mapcar #'eask-intern names))
         (len (length names)) (s (eask--sinr len "" "s"))
         (pkg-installed (cl-remove-if-not #'package-installed-p names))
         (deleted (length pkg-installed)) (skipped (- len deleted)))
    (eask-log "Uninstalling %s specified package%s..." len s)
    (eask-msg "")
    (eask--package-mapc #'eask-package-delete names)
    (eask-msg "")
    (eask-info "(Total of %s package%s deleted, %s skipped)"
               deleted s skipped)))

;; ~/lisp/core/upgrade.el

(defun eask-upgrade--package-version-string (pkg-desc)
  "Get package version string with color from PKG-DESC."
  (let ((version (package-desc-version pkg-desc)))
    (ansi-yellow (package-version-join version))))

(defun eask-upgrade--package (pkg-desc)
  "Upgrade package using PKG-DESC."
  (let* ((name (package-desc-name pkg-desc))
         (pkg-string (ansi-green (eask-2str name)))
         (version-new (eask-upgrade--package-version-string pkg-desc))
         (old-pkg-desc (eask-package-desc name t))
         (version-old (eask-upgrade--package-version-string old-pkg-desc)))
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

(defun eask-upgrade--package-all ()
  "Upgrade for archive packages."
  (if-let* ((upgrades (eask-package--upgrades)))
      (progn
        (mapc #'eask-upgrade--package upgrades)
        (eask-msg "")
        (eask-info "(Done upgrading all packages)"))
    (eask-msg "")
    (eask-info "(All packages are up to date)")))

;; ~/lisp/create/el-project.el

;; ~/lisp/create/elpa.el

(defconst eask-create-elpa--template-name "template-elpa"
  "Holds template project name.")

;; ~/lisp/create/package.el

(defconst eask-create-package--template-name "template-elisp"
  "Holds template project name.")

(defun eask-create-package--replace-s-in-buffer (old new)
  "Replace OLD to NEW in buffer."
  (let ((str (buffer-string)))
    (setq str (eask-s-replace old new str))
    (delete-region (point-min) (point-max))
    (insert str)))

(defun eask-create-package--get-user ()
  "Return user name."
  (string-trim (shell-command-to-string "git config user.name")))

(defun eask-create-package--get-mail ()
  "Return user email."
  (string-trim (shell-command-to-string "git config user.email")))

;; ~/lisp/format/elfmt.el
(declare-function elfmt-buffer "ext:elisp-autofmt.el")

(defconst eask-format-elfmt--version nil
  "`elfmt' version.")

(defun eask-format-elfmt--file (filename)
  "Run elfmt on FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename)))
    (with-current-buffer (find-file filename)
      (elfmt-buffer)
      (save-buffer)
      (kill-buffer))))

;; ~/lisp/format/elisp-autofmt.el
(declare-function elisp-autofmt-buffer "ext:elisp-autofmt.el")

(defconst eask-format-elisp-autofmt--version nil
  "`elisp-autofmt' version.")

(defun eask-format-elisp-autofmt--file (filename)
  "Run elisp-autofmt on FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename)))
    (with-current-buffer (find-file filename)
      (elisp-autofmt-buffer)
      (save-buffer)
      (kill-buffer))))

;; ~/lisp/generate/test/buttercup.el

(defun eask-generate-test-buttercup--init (&optional name)
  "Create new test buffercup project (optional project NAME)."
  (let ((name (or name (f-filename default-directory)))
        (test-path (expand-file-name "tests" default-directory)))
    (when (f-dir? test-path)
      (eask-error "Directory `tests` already exists."))
    (message "create %s" (ansi-green (f-filename test-path)))
    (f-mkdir "tests")
    (let ((test-file (s-concat  "test-" name ".el")))
      (message "create  %s" (ansi-green test-file))
      (with-temp-file (f-join test-path test-file)
        (insert (format "\
;;; %s --- Buttercup tests for %s  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'buttercup)

;; Example test!
(describe \"A suite\"
  (it \"contains a spec with an expectation\"
    (expect t :to-be t)))

;;; %s ends here
" test-file name test-file))))))

;; ~/lisp/generate/test/ecukes.el

;; ~/lisp/generate/test/ert-runner.el

(defun eask-generate-test-ert-runner--test-helper (&optional name)
  "Generate test helper for NAME."
  (with-temp-file (f-join ert-runner-test-path "test-helper.el")
    (insert (format "\
;;; test-helper.el --- Helpers for %s

;;; test-helper.el ends here
" name))))

;; ~/lisp/generate/test/ert.el

(defvar eask-generate-test-ert-test-path
  (expand-file-name "test" default-directory)
  "The default test path.")

(defun eask-generate-test-ert--init (&optional name)
  "Create new test project (optional project NAME)."
  (let* ((name (or name (file-name-nondirectory default-directory)))
         (dir (file-directory-p eask-generate-test-ert-test-path)))
    (eask-with-progress
      (format "create %s folder... " (ansi-green "test"))
      (ignore-errors (make-directory eask-generate-test-ert-test-path t))
      (if dir "skipped ✗" "done ✓"))
    (eask-generate-test-ert--create-test-file name)))

(defun eask-generate-test-ert--create-test-file (name)
  "Generate test file by NAME."
  (let* ((test-file (concat name "-test.el"))
         (full-test-file (expand-file-name test-file eask-generate-test-ert-test-path))
         (ext (file-exists-p full-test-file)))
    (eask-with-progress
      (format "  create %s... " (ansi-green test-file))
      (with-temp-file full-test-file
        (insert (format "\
;;; %s --- Tests for %s

;;; %s ends here
" test-file name test-file)))
      (if ext "skipped ✗" "done ✓"))))

;; ~/lisp/generate/workflow/circle-ci.el

(defun eask-generate-workflow-circle-ci--insert-jobs (version)
  "Insert Circle CI's jobs instruction for specific Emacs' VERSION."
  (insert "  test-ubuntu-emacs-" version ":" "\n")
  (insert "    docker:" "\n")
  (insert "      - image: silex/emacs:" version "-ci" "\n")
  (insert "        entrypoint: bash" "\n")
  (insert "    steps:" "\n")
  (insert "      - setup" "\n")
  (insert "      - test" "\n"))

;; ~/lisp/generate/workflow/github.el

;; ~/lisp/generate/workflow/gitlab.el

(defun eask-generate-workflow-gitlab--insert-jobs (version)
  "Insert GitLab Runner's jobs instruction for specific Emacs' VERSION."
  (insert "test-" version ":" "\n")
  (insert "  image: silex/emacs:" version "-ci" "\n")
  (insert "  script:" "\n")
  (insert "    - eask clean all" "\n")
  (insert "    - eask package" "\n")
  (insert "    - eask install" "\n")
  (insert "    - eask compile" "\n")
  (insert "\n"))

;; ~/lisp/generate/workflow/travis-ci.el

;; ~/lisp/generate/autoloads.el

;; ~/lisp/generate/ignore.el
(declare-function gitignore-templates-names "ext:license-templates.el")

(defun eask-generate-ignore--print-menu ()
  "Print all available ignore."
  (eask-msg "available via `eask generate ignore`")
  (eask-msg "")
  (let ((names (gitignore-templates-names)))
    (dolist (data names)
      (eask-msg "  %s" data))
    (eask-msg "")
    (eask-info "(Total of %s available ignore file%s)" (length names)
               (eask--sinr names "" "s"))))

;; ~/lisp/generate/license.el
(defvar license-templates--data)
(declare-function license-templates-keys "ext:license-templates.el")

(defun eask-generate-license--print-menu ()
  "Print all available license."
  (eask-msg "available via `eask generate license`")
  (eask-msg "")
  (let* ((names (license-templates-keys))
         (offset (eask-seq-str-max names))
         (fmt (concat "  %-" (eask-2str offset) "s  %s")))
    (dolist (data license-templates--data)
      (eask-msg fmt (plist-get data :key) (plist-get data :name)))
    (eask-msg "")
    (eask-info "(Total of %s available license%s)" (length names)
               (eask--sinr names "" "s"))))

;; ~/lisp/generate/pkg-file.el
(defvar eask-generate-pkg-file--filename)

(defun eask-generate-pkg-file--from-pkg-desc ()
  "Generate pkg-file from a package-descriptor."
  (let* ((name (package-desc-name eask-package-desc))
         (pkg-file (expand-file-name (format "%s-pkg.el" name))))
    (setq eask-generate-pkg-file--filename pkg-file)
    (package-generate-description-file eask-package-desc pkg-file)))

(defun eask-generate-pkg-file--from-eask-file ()
  "Generate pkg-file from Eask file."
  (let* ((name (eask-guess-package-name))
         (pkg-file (expand-file-name (concat name "-pkg.el")))
         (version (eask-package-version))
         (description (eask-package-description))
         (reqs (mapcar (lambda (elm)
                         (list (eask-intern (car elm))
                               (if (= (length (cdr elm)) 1)
                                   (nth 0 (cdr elm))
                                 "0")))
                       (append eask-depends-on-emacs eask-depends-on))))
    (setq eask-generate-pkg-file--filename pkg-file)
    (write-region
     (pp-to-string `(define-package ,name ,version ,description ',reqs))
     nil pkg-file)))

;; ~/lisp/generate/recipe.el

;; ~/lisp/init/cask.el
(declare-function ansi-green "ext:ansi.el")
(declare-function ansi-yellow "ext:ansi.el")
(declare-function ansi-white "ext:ansi.el")
(declare-function -flatten "ext:dash.el")
(declare-function cask--read "ext:cask.el")

(defvar eask--cask-contents nil
  "Store Cask-file contents.")

(defun eask--cask-filter-contents (name &optional contents)
  "Filter directives by NAME.

Optional argument CONTENTS is used for nested directives.  e.g. development."
  (cl-remove-if-not (lambda (prop)
                      (eq (car prop) name))
                    (or contents eask--cask-contents)))

(defun eask--cask-package-name ()
  "Return package name from Cask-file."
  (nth 0 (alist-get 'package eask--cask-contents)))

(defun eask--cask-package-version ()
  "Return package version from Cask-file."
  (nth 1 (alist-get 'package eask--cask-contents)))

(defun eask--cask-package-description ()
  "Return package description from Cask-file."
  (nth 2 (alist-get 'package eask--cask-contents)))

(defun eask--cask-package-file ()
  "Return package file from Cask-file."
  (car (alist-get 'package-file eask--cask-contents)))

(defun eask--cask-files ()
  "Return files from Cask-file."
  (alist-get 'files eask--cask-contents))

(defun eask--cask-package-descriptor ()
  "Return package descriptor from Cask-file."
  (car (alist-get 'package-descriptor eask--cask-contents)))

(defun eask--cask-sources ()
  "Return sources from Cask-file."
  (eask--cask-filter-contents 'source))

(defun eask--cask-reqs ()
  "Return dependencies from Cask-file."
  (eask--cask-filter-contents 'depends-on))

(defun eask--cask-reqs-dev ()
  "Return development dependencies file from Cask-file."
  (let ((dev-scopes (eask--cask-filter-contents 'development))
        (deps))
    (dolist (dev-scope dev-scopes)
      (setq deps (append deps (eask--cask-filter-contents 'depends-on (cdr dev-scope)))))
    deps))

(defun eask--cask-remove-emacs-dep (list)
  "Remove dependency Emacs from dependencies LIST."
  (cl-remove-if (lambda (req)
                  (when (string= (cadr req) "emacs")
                    (caddr req)))
                list))

(defun eask--cask-reqs-no-emacs ()
  "Return dependencies from Cask-file but exclude Emacs."
  (eask--cask-remove-emacs-dep (eask--cask-reqs)))

(defun eask--cask-reqs-dev-no-emacs ()
  "Return development dependencies from Cask-file but exclude Emacs."
  (eask--cask-remove-emacs-dep (eask--cask-reqs-dev)))

(defun eask--cask-emacs-version ()
  "Return Emacs version from Cask-file."
  (let ((reqs (eask--cask-reqs)))
    (cl-some (lambda (req)
               (when (string= (cadr req) "emacs")
                 (caddr req)))
             reqs)))

(defun eask--convert-cask (filename)
  "Convert Cask FILENAME to Eask."
  (let* ((filename (expand-file-name filename))
         (file (file-name-nondirectory (eask-root-del filename)))
         (new-file (eask-s-replace "Cask" "Eask" file))
         (new-filename (expand-file-name new-file))
         (eask--cask-contents (ignore-errors (cask--read filename)))  ; Read it!
         (converted))
    (eask-with-progress
      (format "Converting file `%s` to `%s`... " file new-file)
      (eask-with-verbosity 'debug
        (cond ((not (string-prefix-p "Cask" file))
               (eask-debug "✗ Invalid Cask filename, the file should start with `Cask`"))
              (t
               (with-current-buffer (find-file new-filename)
                 (erase-buffer)
                 (goto-char (point-min))

                 ;; XXX: Newline to look nicer!
                 (eask--unsilent (eask-msg "\n"))

                 (let* ((project-name
                         (or (eask--cask-package-name)
                             (file-name-nondirectory (directory-file-name default-directory))))
                        (package-name
                         (or (eask--cask-package-name)
                             (eask-read-string (format "package name: (%s) " project-name) nil nil project-name)))
                        (version (or (eask--cask-package-version)
                                     (eask-read-string "version: (1.0.0) " nil nil "1.0.0")))
                        (description (or (eask--cask-package-description)
                                         (eask-read-string "description: ")))
                        (guess-entry-point (eask-guess-entry-point project-name))
                        (entry-point
                         (or (eask--cask-package-file)
                             (eask-read-string (format "entry point: (%s) " guess-entry-point)
                                               nil nil guess-entry-point)))
                        (emacs-version
                         (or (eask--cask-emacs-version)
                             (eask-read-string "emacs version: (26.1) " nil nil "26.1")))
                        (website (eask-read-string "website: "))
                        (keywords (eask-read-string "keywords: "))
                        (keywords (split-string keywords "[, ]"))
                        (keywords (string-join keywords "\" \""))
                        (content (format
                                  ";; -*- mode: eask; lexical-binding: t -*-

(package \"%s\"
         \"%s\"
         \"%s\")

(website-url \"%s\")
(keywords \"%s\")

(package-file \"%s\")
"
                                  package-name version description website keywords
                                  entry-point)))
                   (insert content)

                   (when-let* ((files (eask--cask-files))
                               (files (if (stringp files) files (-flatten files))))
                     (insert "(files")
                     (dolist (file files)
                       (if (stringp file)
                           (insert "\n \"" file "\"")
                         (insert "\n " (eask-2str file))))
                     (insert ")\n"))

                   (when-let* ((pkg-desc (eask--cask-package-descriptor)))
                     (insert "\n")
                     (insert "(package-descriptor \"" (eask-2str pkg-desc) "\")\n"))

                   (insert "\n")
                   (insert "(script \"test\" \"echo \\\"Error: no test specified\\\" && exit 1\")\n")

                   (when-let* ((sources (eask--cask-sources)))
                     (insert "\n")
                     (dolist (source sources)
                       (insert "(source '" (eask-2str (cadr source)) ")\n")))

                   (insert "\n")
                   (insert "(depends-on \"emacs\" \"" emacs-version "\")")
                   (unless (eask--cask-reqs-no-emacs)
                     (insert "\n"))  ; Make sure end line exists!

                   (when-let* ((pkgs (eask--cask-reqs-no-emacs)))
                     (insert "\n")
                     (dolist (pkg pkgs)
                       (let ((val (mapconcat #'eask-2str (cdr pkg) "\" \"")))
                         (insert "(depends-on \"" val "\")\n"))))

                   (when-let* ((pkgs (eask--cask-reqs-dev-no-emacs)))
                     (insert "\n")
                     (insert "(development\n")
                     (dolist (pkg pkgs)
                       (let ((val (mapconcat #'eask-2str (cdr pkg) "\" \"")))
                         (insert " (depends-on \"" val "\")\n")))
                     (insert " )\n")))

                 (save-buffer))
               (setq converted t))))
      (if converted "done ✓" "skipped ✗"))
    converted))

;; ~/lisp/init/eldev.el

(defun eask-init-eldev--map-elpa (name)
  "Convert Eldev mapping to Eask mapping."
  (pcase name
    ("melpa-unstable" 'melpa)
    (_ name)))

(defun eask-init-eldev--convert (filename)
  "Convert Eldev FILENAME to Eask."
  (let* ((filename (expand-file-name filename))
         (file (file-name-nondirectory (eask-root-del filename)))
         (new-file (eask-s-replace "Eldev" "Eask" file))
         (new-filename (expand-file-name new-file))
         (converted))
    (eask-with-progress
      (format "Converting file `%s` to `%s`... " file new-file)
      (eask-with-verbosity 'debug
        (cond ((not (string-prefix-p "Eldev" file))
               (eask-debug "✗ Invalid Eldev filename, the file should start with `Eldev`"))
              (t
               (with-current-buffer (find-file new-filename)
                 (erase-buffer)
                 (goto-char (point-min))

                 (let* ((project-name (file-name-nondirectory (directory-file-name default-directory)))
                        (package-name (eask-read-string (format "\npackage name: (%s) " project-name) nil nil project-name))
                        (version (eask-read-string "version: (1.0.0) " nil nil "1.0.0"))
                        (description (eask-read-string "description: "))
                        (guess-entry-point (eask-guess-entry-point project-name))
                        (entry-point (eask-read-string (format "entry point: (%s) " guess-entry-point)
                                                       nil nil guess-entry-point))
                        (emacs-version (eask-read-string "emacs version: (26.1) " nil nil "26.1"))
                        (website (eask-read-string "website: "))
                        (keywords (eask-read-string "keywords: "))
                        (keywords (split-string keywords "[, ]"))
                        (keywords (string-join keywords "\" \""))
                        (content (format
                                  ";; -*- mode: eask; lexical-binding: t -*-

(package \"%s\"
         \"%s\"
         \"%s\")

(website-url \"%s\")
(keywords \"%s\")

(package-file \"%s\")

(script \"test\" \"echo \\\"Error: no test specified\\\" && exit 1\")
"
                                  package-name version description website keywords
                                  entry-point)))
                   (insert content)

                   (when-let* ((names (mapcar #'car package-archives))
                               (sources (mapcar #'eask-init-eldev--map-elpa names)))
                     (insert "\n")
                     (dolist (source sources)
                       (insert "(source '" (eask-2str source) ")\n"))))
                 (save-buffer))
               (setq converted t))))
      (if converted "done ✓" "skipped ✗"))
    converted))

;; ~/lisp/init/keg.el

(defun eask-init-keg--file-read (path)
  "Return sexp from Keg file (PATH) search from `deafult-directory'.
If no found the Keg file, returns nil."
  (let (sources devs packages lint-disables scripts)
    (when path
      (dolist (elm (read (with-temp-buffer
                           (insert-file-contents path)
                           (format "(%s)" (buffer-string)))))
        (let ((op (car elm))
              (args (cdr elm)))
          (cond
           ((eq 'source op)
            (dolist (elm args) (push elm sources)))
           ((eq 'dev-dependency op)
            (dolist (elm args) (push elm devs)))
           ((eq 'package op)
            (dolist (elm args) (push elm packages)))
           ((eq 'disable-lint op)
            (dolist (elm args) (push elm lint-disables)))
           ((eq 'script op)
            (dolist (elm args) (push elm scripts))))))
      `((sources . ,(nreverse (delete-dups sources)))
        (devs . ,(nreverse (delete-dups devs)))
        (packages . ,(nreverse (delete-dups packages)))
        (disables . ,(nreverse (delete-dups lint-disables)))
        (scripts . ,(nreverse (delete-dups scripts)))))))

(defun eask-init-key--convert (filename)
  "Convert Keg FILENAME to Eask."
  (let* ((filename (expand-file-name filename))
         (file (file-name-nondirectory (eask-root-del filename)))
         (new-file (eask-s-replace "Keg" "Eask" file))
         (new-filename (expand-file-name new-file))
         (contents (ignore-errors (eask-init-keg--file-read filename)))  ; Read it!
         (converted))
    (eask-with-progress
      (format "Converting file `%s` to `%s`... " file new-file)
      (eask-with-verbosity 'debug
        (cond ((not (string-prefix-p "Keg" file))
               (eask-debug "✗ Invalid Keg filename, the file should start with `Keg`"))
              (t
               (with-current-buffer (find-file new-filename)
                 (erase-buffer)
                 (goto-char (point-min))

                 (let* ((project-name (file-name-nondirectory (directory-file-name default-directory)))
                        (package-name (eask-read-string (format "\npackage name: (%s) " project-name) nil nil project-name))
                        (version (eask-read-string "version: (1.0.0) " nil nil "1.0.0"))
                        (description (eask-read-string "description: "))
                        (guess-entry-point (eask-guess-entry-point project-name))
                        (entry-point (eask-read-string (format "entry point: (%s) " guess-entry-point)
                                                       nil nil guess-entry-point))
                        (emacs-version (eask-read-string "emacs version: (26.1) " nil nil "26.1"))
                        (website (eask-read-string "website: "))
                        (keywords (eask-read-string "keywords: "))
                        (keywords (split-string keywords "[, ]"))
                        (keywords (string-join keywords "\" \""))
                        (content (format
                                  ";; -*- mode: eask; lexical-binding: t -*-

(package \"%s\"
         \"%s\"
         \"%s\")

(website-url \"%s\")
(keywords \"%s\")

(package-file \"%s\")

(script \"test\" \"echo \\\"Error: no test specified\\\" && exit 1\")
"
                                  package-name version description website keywords
                                  entry-point)))
                   (insert content)

                   (when-let* ((scripts (alist-get 'scripts contents)))
                     (dolist (script scripts)
                       (let* ((cmds (cadr script))
                              (_ (pop cmds))
                              (cmds (mapconcat #'identity cmds " ")))
                         (insert "(script \"" (eask-2str (car script))
                                 "\" " (prin1-to-string cmds) ")\n"))))

                   (when-let* ((sources (alist-get 'sources contents)))
                     (insert "\n")
                     (dolist (source sources)
                       (insert "(source '" (eask-2str source) ")\n")))

                   (insert "\n")
                   (insert "(depends-on \"emacs\" \"" emacs-version "\")"))
                 (unless (alist-get 'packages contents)
                   (insert "\n"))  ; Make sure end line exists!

                 (when-let* ((pkgs (alist-get 'packages contents)))
                   (insert "\n")
                   (dolist (pkg pkgs)
                     (insert "(depends-on \"" (eask-2str (car pkg)) "\")\n")))

                 (when-let* ((devs (alist-get 'devs contents)))
                   (insert "\n")
                   (insert "(development\n")
                   (dolist (dev devs)
                     (insert " (depends-on \"" (eask-2str dev) "\")\n"))
                   (insert " )\n"))

                 (save-buffer))
               (setq converted t))))
      (if converted "done ✓" "skipped ✗"))
    converted))

;; ~/lisp/init/source.el

(defun eask-init-source--convert (filename)
  "Convert elisp source FILENAME to Eask."
  (let* ((filename (expand-file-name filename))
         (file (file-name-nondirectory (eask-root-del filename)))
         (new-file (concat "Eask." (file-name-sans-extension file)))
         (new-filename (expand-file-name new-file))
         (pkg-desc (with-temp-buffer
                     (insert-file-contents filename)
                     (eask-ignore-errors-silent (package-buffer-info))))  ; Read it!
         (converted))
    (eask-with-progress
      (format "Converting file `%s` to `%s`... " file new-file)
      (eask-with-verbosity 'debug
        (cond ((not (string-suffix-p ".el" file))
               (eask-debug "✗ Invalid elisp filename, the file should end with `.el`"))
              (t
               (when pkg-desc
                 (with-current-buffer (find-file new-filename)
                   (erase-buffer)
                   (goto-char (point-min))

                   (let* ((eask-package-desc pkg-desc)
                          (package-name (package-desc-name pkg-desc))
                          (version (package-desc-version pkg-desc))
                          (version (package-version-join version))
                          (description (package-desc-summary pkg-desc))
                          (description (prin1-to-string description))
                          (website (or (eask-package-desc-url) ""))
                          (keywords (eask-package-desc-keywords))
                          (keywords (string-join keywords "\" \""))
                          (reqs (package-desc-reqs pkg-desc))
                          (content (format
                                    ";; -*- mode: eask; lexical-binding: t -*-

(package \"%s\"
         \"%s\"
         %s)

(website-url \"%s\")
(keywords \"%s\")

(package-file \"%s\")

(script \"test\" \"echo \\\"Error: no test specified\\\" && exit 1\")

(source \"gnu\")

"
                                    package-name version description website keywords
                                    file)))
                     (insert content)

                     (dolist (req reqs)
                       (let* ((req-name (car req))
                              (req-version (cdr req))
                              (req-version (package-version-join (car req-version))))
                         (insert (format "(depends-on \"%s\" \"%s\")\n" req-name req-version)))))

                   (save-buffer))
                 (setq converted t)))))
      (if converted "done ✓" "skipped ✗"))
    (when converted new-filename)))

;; ~/lisp/link/add.el
(defvar eask-link-add--package-name    nil "Used to form package name.")
(defvar eask-link-add--package-version nil "Used to form package name.")

(defun eask-link-add--package-desc-reqs (desc)
  "Return a list of requirements from package DESC."
  (cl-remove-if (lambda (name) (string= name "emacs"))
                (mapcar #'car (package-desc-reqs desc))))

(defun eask-link-add--create (source)
  "Add link with SOURCE path."
  (let* ((dir-name (format "%s-%s" eask-link-add--package-name eask-link-add--package-version))
         (link-path (expand-file-name dir-name package-user-dir)))
    (when (file-exists-p link-path)
      (eask-msg "")
      (eask-with-progress
        (ansi-yellow "!! The link is already present; overriding the existing link... ")
        (eask-link-delete-symlink link-path)
        (ansi-yellow "done !!")))
    (make-symbolic-link source link-path)
    (eask-msg "")
    (eask-info "(Created link from `%s` to `%s`)" source (eask-f-filename link-path))))

;; ~/lisp/link/delete.el

(defun eask-link-delete-symlink (path)
  "Delete symlink PATH."
  (ignore-errors (delete-file path))
  (ignore-errors (delete-directory path t)))

(defun eask-link-delete--link (name)
  "Delete a link by its' NAME."
  (let* ((links (eask-link-list))
         (source (assoc name links))
         (link (expand-file-name name package-user-dir)))
    (if (and source (file-symlink-p link))
        (progn
          (eask-link-delete-symlink link)
          (eask-info "✓ Package `%s` unlinked" name)
          t)
      (eask-info "✗ No linked package name `%s`" name)
      nil)))

;; ~/lisp/link/list.el

(defun eask-link-list ()
  "Return a list of all links."
  (mapcar
   (lambda (file)
     (cons (eask-f-filename file) (file-truename file)))
   (cl-remove-if-not #'file-symlink-p (directory-files package-user-dir t))))

(defun eask--print-link (link offset)
  "Print information regarding the LINK.

The argument OFFSET is used to align the result."
  (eask-println (concat "  %-" (eask-2str offset) "s  %s") (car link) (cdr link)))

;; ~/lisp/lint/checkdoc.el
(defvar checkdoc-version)
(defvar checkdoc-create-error-function)
(declare-function checkdoc-buffer-label "ext:checkdoc.el")
(defvar eask-lint-checkdoc--errors nil "Error flag.")

(defun eask-lint-checkdoc--print-error (text start _end &optional _unfixable)
  "Print error for checkdoc.

Arguments TEXT, START, END and UNFIXABLE are required for this function to
be assigned to variable `checkdoc-create-error-function'."
  (setq eask-lint-checkdoc--errors t)
  (let ((msg (concat (checkdoc-buffer-label) ":"
                     (int-to-string (count-lines (point-min) (or start (point-min))))
                     ": " text)))
    (if (eask-strict-p) (error msg) (warn msg))
    ;; Return nil because we *are* generating a buffered list of errors.
    nil))

(defun eask-lint-checkdoc--file (filename)
  "Run checkdoc on FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         (eask-lint-checkdoc--errors))
    (eask-lint-first-newline)
    (eask-msg "`%s` with checkdoc (%s)" (ansi-green file) checkdoc-version)
    (checkdoc-file filename)
    (unless eask-lint-checkdoc--errors (eask-msg "No issues found"))))

;; ~/lisp/lint/declare.el
(defvar check-declare-warning-buffer)

(defun eask-lint-declare--file (filename)
  "Run check-declare on FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         (errors))
    (eask-lint-first-newline)
    (eask-msg "`%s` with check-declare" (ansi-green file))
    (setq errors (eask--silent (check-declare-file filename)))
    (if errors
        (with-current-buffer check-declare-warning-buffer
          (eask-report (string-remove-prefix "\n" (buffer-string))))
      (eask-msg "No issues found"))))

;; ~/lisp/lint/elint.el
(declare-function elint-get-log-buffer "ext:elsa.el")

(defun eask-lint-elint--file (filename)
  "Run elint on FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         (noninteractive))
    (eask-lint-first-newline)
    (eask-msg "`%s` with elint" (ansi-green file))
    (eask-with-verbosity 'debug (elint-file filename))
    (let ((log-buffer (elint-get-log-buffer)))
      (eask-print-log-buffer log-buffer)
      (kill-buffer log-buffer))))

;; ~/lisp/lint/elisp-lint.el
(declare-function elisp-lint-file "ext:elsa.el")

(defconst eask-lint-elisp-lint--version nil
  "`elisp-lint' version.")

(defun eask-lint-elisp-lint--process-file (filename)
  "Process FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         success)
    (eask-msg "")
    (eask-msg "`%s` with elisp-lint (%s)" (ansi-green file) eask-lint-elisp-lint--version)
    (eask-with-verbosity 'debug
      (setq success (elisp-lint-file filename)))
    ;; Report result!
    (cond (success
           (eask-msg "No issues found"))
          ((eask-strict-p)
           (eask-error "Linting failed")))))

;; ~/lisp/lint/elsa.el
(defvar elsa-global-state)
(declare-function elsa-message-format "ext:elsa.el")
(declare-function elsa-analyse-file "ext:elsa.el")
(declare-function --each "ext:dash.el")
(require 'dash nil t)

(defconst eask-lint-elsa--version nil
  "Elsa version.")

(defun eask-lint-elsa--analyse-file (filename)
  "Process FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         errors)
    (eask-msg "")
    (eask-msg "`%s` with elsa (%s)" (ansi-green file) eask-lint-elsa--version)
    (eask-with-verbosity 'debug
      (setq errors (oref (elsa-analyse-file filename elsa-global-state) errors)))
    (if errors
        (--each (reverse errors)
          (let ((line (string-trim (concat file ":" (elsa-message-format it)))))
            (cond ((string-match-p "[: ][Ee]rror:" line)
                   (eask-error "%s" line))
                  ((string-match-p "[: ][Ww]arning:" line)
                   (eask-warn "%s" line))
                  (t (eask-log "%s" line)))))
      (eask-msg "No issues found"))))

;; ~/lisp/lint/indent.el

(defun eask-lint-indent--undo-pos (entry)
  "Return the undo pos from ENTRY."
  (cl-typecase (car entry)
    (number (car entry))
    (string (abs (cdr entry)))))

(defun eask-lint-indent--undo-infos (undo-list)
  "Return list of infos in UNDO-LIST."
  (let ((infos))
    (dolist (elm undo-list)
      (when-let* ((pos (eask-lint-indent--undo-pos elm))
                  (line (line-number-at-pos pos))
                  (expected (progn (eask--goto-line line)
                                   (current-indentation))))
        (push (list line expected) infos)))
    infos))

(defun eask-lint-indent--file (file)
  "Lint indent for FILE."
  (eask-msg "")
  (eask-msg "`%s` with indent-lint" (ansi-green (eask-root-del file)))
  (find-file file)
  (let ((tick (buffer-modified-tick))
        (bs (buffer-string)))
    (eask-with-temp-buffer (insert bs))
    (eask--silent (indent-region (point-min) (point-max)))
    (if-let* (((/= tick (buffer-modified-tick)))
              (infos (eask-lint-indent--undo-infos buffer-undo-list)))
        ;; Indentation changed: warn for each line.
        (dolist (info infos)
          (let* ((line    (nth 0 info))
                 (column  (nth 1 info))
                 (current (eask-with-buffer
                            (eask--goto-line line) (current-indentation))))
            (eask-report "%s:%s: Expected indentation is %s but found %s"
                         (buffer-name) line column current)))
      (eask-log "No mismatch indentation found"))))

;; ~/lisp/lint/keywords.el
(require 'finder nil t)

(defun eask-lint-keywords--defined (keywords)
  "Return t if KEYWORDS are defined correctly."
  (let ((available-keywords (mapcar #'car finder-known-keywords))
        (result))
    (dolist (keyword keywords)
      (when (memq (intern keyword) available-keywords)
        (setq result t)))
    result))

;; ~/lisp/lint/license.el
(require 'whitespace nil t)

(defun eask-lint-license--s-match-all (contents)
  "Return t when every CONTENTS match the `buffer-string'."
  (cl-every (lambda (content)
              (string-match-p (eask-s-replace "\n" " " content) (buffer-string)))
            contents))

(defun eask-lint-license--scan (file)
  "Scan the license FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((whitespace-style '(trailing)))
      (whitespace-cleanup))
    (let ((content (eask-s-replace "\n" " " (buffer-string))))
      (erase-buffer)
      (insert content))
    ;; See https://api.github.com/licenses
    (cond
     ((eask-lint-license--s-match-all
       '("Licensed under the Academic Free License version 3.0"))
      '("afl-3.0" "Academic Free License v3.0" "AFL-3.0"))
     ((eask-lint-license--s-match-all
       '("You should have received a copy of the GNU Affero General Public License"))
      '("agpl-3.0" "GNU Affero General Public License v3.0" "AGPL-3.0"))
     ((eask-lint-license--s-match-all
       '("Everyone is permitted to copy and distribute verbatim copies"
         "but changing it is not allowed."
         "artistic"))
      '("artistic-2.0" "Artistic License 2.0" "Artistic-2.0"))
     ((eask-lint-license--s-match-all
       '("Licensed under the Apache License, Version 2.0"))
      '("apache-2.0" "Apache License 2.0" "Apache-2.0"))
     ((eask-lint-license--s-match-all
       '("Redistribution and use in source and binary forms"
         "Redistributions in binary form must reproduce"))
      '("bsd-2-clause" "BSD 2-Clause \"Simplified\" License" "BSD-2-Clause"))
     ((eask-lint-license--s-match-all
       '("Redistribution and use in source and binary forms"
         "Neither the name of the copyright holder nor"))
      '("bsd-3-clause" "BSD 3-Clause \"New\" or \"Revised\" License" "BSD-3-Clause"))
     ((eask-lint-license--s-match-all
       '("Permission is hereby granted, free of charge, to any person or organization"))
      '("bsl-1.0" "Boost Software License 1.0" "BSL-1.0"))
     ((eask-lint-license--s-match-all
       '("The laws of most jurisdictions throughout the world automatically confer exclusive Copyright"))
      '("cc0-1.0" "Creative Commons Zero v1.0 Universal" "CC0-1.0"))
     ((eask-lint-license--s-match-all
       '("Eclipse Public License - v 2.0"
         "Eclipse Foundation"))
      '("epl-2.0" "Eclipse Public License 2.0" "EPL-2.0"))
     ((eask-lint-license--s-match-all
       '("Copying and distribution of this file, with or without"))
      '("fsfap" "FSF All Permissive License" "FSFAP"))
     ((eask-lint-license--s-match-all
       '("is free software." "you can redistribute it"
         "version 2 of the"))
      '("gpl-2.0" "GNU General Public License v2.0" "GPL-2.0"))
     ((eask-lint-license--s-match-all
       '("is free software." "you can redistribute it"
         "version 3 of the"))
      '("gpl-3.0" "GNU General Public License v3.0" "GPL-3.0"))
     ((eask-lint-license--s-match-all
       '("Permission to use, copy, modify, and/or distribute this"))
      '("isc" " Internet Systems Consortium" "ISC"))
     ((eask-lint-license--s-match-all
       '("Lesser"
         "GNU"
         "version 2"))
      '("lgpl-2.1" "GNU Lesser General Public License v2.1" "LGPL-2.1"))
     ((eask-lint-license--s-match-all
       '("This license governs use of the accompanying software."
         "If you use the software, you accept this license."
         "If you do not accept the license, do not use the software."))
      '("ms-pl" "Microsoft Public License" "MS-PL"))
     ((eask-lint-license--s-match-all
       '("Permission is hereby granted, free of charge, to any person"))
      '("mit" "MIT License" "MIT"))
     ((eask-lint-license--s-match-all
       '("http://mozilla.org/MPL/2.0/"))
      '("mpl-2.0" "Mozilla Public License 2.0" "MPL-2.0"))
     ((eask-lint-license--s-match-all
       '("Licensed under the Open Software License version 3.0"))
      '("osl-3.0" "Open Software License 3.0" "OSL-3.0"))
     ((eask-lint-license--s-match-all
       '("This is free and unencumbered software released into"))
      '("unlicense" "The Unlicense" "Unlicense"))
     ((eask-lint-license--s-match-all
       '("Everyone is permitted to copy and distribute verbatim copies"))
      '("wtfpl-1" "Do What the Fuck You Want To Public License (Version 1)" "WTFPL-1"))
     ((eask-lint-license--s-match-all
       '("Everyone is permitted to copy and distribute verbatim or modified"))
      '("wtfpl-2" "Do What the Fuck You Want To Public License (Version 2)" "WTFPL-2"))
     ((eask-lint-license--s-match-all
       '("Permission is granted to anyone to use this software for any purpose,"))
      '("zlib" "zlib License" "Zlib"))
     (t
      '("unknown" "Unknown license" "unknown")))))

(defun eask-lint-license--print-scanned (scanned)
  "Print all SCANNED license."
  (eask-msg "available via `eask lint license`")
  (eask-msg "")
  (let* ((names (mapcar #'car scanned))
         (offset (eask-seq-str-max names))
         (fmt (concat "  %-" (eask-2str offset) "s  %s")))
    (dolist (data scanned)
      (eask-msg fmt (nth 0 data) (nth 3 data)))
    (eask-msg "")
    (eask-info "(Total of %s scanned license%s)" (length names)
               (eask--sinr names "" "s"))))

;; ~/lisp/lint/org.el

(defun eask-lint-org--print-error (file result)
  "Print the error RESULT from FILE."
  (let* ((data (cl-second result))
         (filename (file-name-nondirectory file))
         (line (elt data 0))
         (text (elt data 2))
         (msg (concat filename ":" line ": " text)))
    (if (eask-strict-p) (error msg) (warn msg))))

(defun eask-lint-org--file (file)
  "Run `org-lint' on FILE."
  (eask-msg "`%s` with org-lint" (ansi-green file))
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (if-let* ((results (org-lint)))
        (mapc (lambda (result)
                (eask-lint-org--print-error file result))
              results)
      (eask-msg "No issues found"))))

;; ~/lisp/lint/package.el
(declare-function package-lint-current-buffer "ext:package-lint.el")

(defconst eask-lint-package--version nil
  "`package-lint' version.")

(defun eask-lint-package--file (filename)
  "Package lint FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename)))
    (eask-msg "")
    (eask-msg "`%s` with package-lint (%s)" (ansi-green file) eask-lint-package--version)
    (with-current-buffer (find-file filename)
      (package-lint-current-buffer)
      (kill-current-buffer)))
  (eask-print-log-buffer "*Package-Lint*"))

;; ~/lisp/lint/regexps.el
(declare-function relint-buffer "ext:package-lint.el")

(defconst eask-lint-regexps--relint-version nil
  "`relint' version.")

(defun eask-lint-regexps--relint-file (filename)
  "Package lint FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         (errors))
    (eask-msg "")
    (eask-msg "`%s` with relint (%s)" (ansi-green file) eask-lint-regexps--relint-version)
    (with-current-buffer (find-file filename)
      (setq errors (relint-buffer (current-buffer)))
      (dolist (err errors)
        (let* ((msg       (seq-elt err 0))
               (error-pos (seq-elt err 2))
               (severity  (seq-elt err 7))
               (report-func (pcase severity
                              (`error   #'eask-error)
                              (`warning #'eask-warn)
                              (_        #'eask-info))))
          (funcall report-func "%s:%s %s: %s"
                   file (line-number-at-pos error-pos)
                   (capitalize (eask-2str severity)) msg)))
      (unless errors
        (eask-msg "No issues found"))
      (kill-current-buffer))))

;; ~/lisp/run/command.el

(defun eask-run-command--desc (name)
  "Return command's description by its command's NAME."
  (car (split-string (or (documentation name) "") "\n")))

(defun eask-run-command--print-commands ()
  "Print all available commands."
  (eask-msg "available via `eask run command`")
  (eask-msg "")
  (let* ((keys (reverse eask-commands))
         (offset (eask-seq-str-max keys))
         (fmt (concat "  %-" (eask-2str offset) "s  %s")))
    (dolist (key keys)
      (eask-msg fmt key (eask-run-command--desc key)))
    (eask-msg "")
    (eask-info "(Total of %s available script%s)" (length keys)
               (eask--sinr keys "" "s"))))

(defun eask-run-command--execute (name)
  "Execute the command by NAME."
  (eask-info "[RUN]: %s" name)
  (funcall (eask-intern name)))

(defun eask-run-command--unmatched-commands (commands)
  "Return a list of COMMANDS that cannot be found in `eask-commands'."
  (let (unmatched)
    (dolist (command commands)
      (unless (memq (eask-intern command) eask-commands)
        (push command unmatched)))
    unmatched))

;; ~/lisp/run/script.el

(defconst eask-run-script--file (expand-file-name "run" eask-homedir)
  "Target file to export the `run' scripts.")

(defun eask-run-script--print-scripts ()
  "Print all available scripts."
  (eask-msg "available via `eask run script`")
  (eask-msg "")
  (let* ((keys (mapcar #'car (reverse eask-scripts)))
         (offset (eask-seq-str-max keys))
         (fmt (concat "  %-" (eask-2str offset) "s  %s")))
    (dolist (key keys)
      (eask-msg fmt key (cdr (assoc key eask-scripts))))
    (eask-msg "")
    (eask-info "(Total of %s available script%s)" (length keys)
               (eask--sinr keys "" "s"))))

(defun eask-run-command--export (command)
  "Export COMMAND instruction."
  (ignore-errors (make-directory eask-homedir t))  ; generate dir `~/.eask/'
  (when eask-is-pkg
    ;; XXX: Due to `MODULE_NOT_FOUND` not found error from vcpkg,
    ;; see https://github.com/vercel/pkg/issues/1356.
    ;;
    ;; We must split up all commands!
    (setq command (eask-s-replace " && " "\n" command)))
  (setq command (concat command " " (eask-rest)))
  (write-region (concat command "\n") nil eask-run-script--file t))

(defun eask-run-script--unmatched-scripts (scripts)
  "Return a list of SCRIPTS that cannot be found in `eask-scripts'."
  (let (unmatched)
    (dolist (script scripts)
      (unless (assoc script eask-scripts)
        (push script unmatched)))
    unmatched))

;; ~/lisp/source/add.el

(defun eask-source-add--from-mapping (name url)
  "Return t if NAME and URL matched our database."
  (string= (eask-source-url name) url))

(defun eask-source-add (name url exists)
  "Add an archive source by NAME.

If argument URL is nil; ignore the insertion.

Arguments EXISTS is used to print the information."
  (let* ((style-sym (string-match "([ \t\n\r]*source[ \t\n\r]*[']+" (buffer-string)))
         (name-str (if style-sym (concat "'" name)
                     (concat "\"" name "\"")))
         (built-in (eask-source-add--from-mapping name url)))
    (if (and url (not built-in))
        (insert "(source " name-str " \"" url "\")\n")
      (insert "(source " name-str ")\n"))
    (eask-info "(New source `%s' added and points to `%s')" name (or url (cdr exists)))))

(defun eask-source-add--write (name url exists)
  "Write source construct by NAME and URL.

The argument URL can be nil.

The argument EXISTS is use to search for correct position to insert new source."
  (with-current-buffer (find-file eask-file)
    (goto-char (point-min))
    (cond
     (exists
      (if (re-search-forward (concat "([ \t\n\r]*source[ \t\n\r]*['\"]+" name)  nil t)
          (let ((start (point))
                (built-in (eask-source-add--from-mapping name url)))
            (when (string= "\"" (string (char-after)))
              (cl-incf start))
            (re-search-forward "[ \t\r\n\")]*" nil t)  ; Forward to non-space characters!
            (forward-char -1)
            (when (string= "\n" (string (char-after)))
              (forward-char -1))
            (pcase (string (char-after))
              (")"
               (unless built-in
                 (insert " \"" url "\"")))
              ("\""
               (if built-in
                   (delete-region start (save-window-excursion
                                          (search-forward ")" nil t)
                                          (1- (point))))
                 (let ((old (thing-at-point 'string))
                       (new (concat "\"" url "\"")))
                   (delete-region (point) (+ (point) (length old)))
                   (insert new)))))
            (eask-info "(Changed archive `%s''s location from `%s' to `%s')"
                       name (cdr exists) url))
        (goto-char (point-max))
        (eask-source-add name url exists)))
     (t
      (if (re-search-forward "([ \t\n\r]*source[ \t\n\r]*['\"]+"  nil t)
          (forward-paragraph)
        (goto-char (point-max)))
      (eask-source-add name url exists)))
    (save-buffer)))

(defun eask-source-add--ask-if-overwrite (name url)
  "Ask source overwrite if needed.

Arguments NAME and URL are main arguments for this command."
  (if-let* ((exists (assoc name package-archives))
            (old-url (cdr exists)))
      (cond ((string= old-url url)
             (eask-info "(Nothing has changed due to the URLs are the same)"))
            ((yes-or-no-p
              (format
               (concat
                "The archive `%s' is already exists and currently points  to `%s'.

Do you want to overwrite it? ")
               name old-url))
             (eask-source-add--write name url exists))
            (t
             (eask-info "(Nothing has changed, aborted)")))
    (eask-source-add--write name url nil)))

;; ~/lisp/source/delete.el

(defun eask-source-delete (name)
  "Delete an archive source by NAME."
  (with-current-buffer (find-file eask-file)
    (goto-char (point-min))
    (when (re-search-forward (concat "([ \t\n\r]*source[ \t\n\r]*['\"]+" name) nil t)
      (delete-region (line-beginning-position) (1+ (line-end-position)))
      (eask-info "(Delete source `%s')" name))
    (save-buffer)))

;; ~/lisp/source/list.el

;; ~/lisp/test/activate.el

;; ~/lisp/test/buttercup.el

;; ~/lisp/test/ecukes.el

(defun eask-test-ecukes--run (files)
  "Run ecukes on FILES.

Modified from function `ecukes-cli/run'."
  (ecukes-load)
  (ecukes-reporter-use ecukes-cli-reporter)
  (ecukes-run files))

;; ~/lisp/test/ert-runner.el

(defun eask-test-ert-runner--run (fnc &rest args)
  "Run around function `ert-runner/run'.

Arguments FNC and ARGS are used for advice `:around'.

Handle the argument ARGS when command arguments are specified."
  (let* ((patterns (eask-args))
         (files (eask-expand-file-specs patterns)))
    (setq args files))
  (apply fnc args))

;; ~/lisp/test/ert.el

(defvar eask-test-ert--message-loop nil
  "Prevent inifinite recursive message function.")

(require 'ert nil t)

(defun eask-test-ert--message (fnc &rest args)
  "Colorized ert messages.

Arguments FNC and ARGS are used for advice `:around'."
  (if eask-test-ert--message-loop (apply fnc args)
    (let ((eask-test-ert--message-loop t)
          (text (ignore-errors (apply #'format args))))
      (cond
       ;; (message nil) is used to clear the minibuffer
       ;; However, format requires the first argument to be a format string
       ((null (car args))
        (apply fnc args))
       ((string-match-p "^[ ]+FAILED " text)
        (eask-msg (ansi-red text)))
       ((string-match-p "^[ ]+SKIPPED " text)
        (eask-msg text))
       ((string-match-p "^[ ]+passed " text)
        (eask-msg (ansi-green text)))
       (t (apply fnc args))))))

;; ~/lisp/test/melpazoid.el

(defcustom eask-test-melpazoid-el-url
  "https://raw.githubusercontent.com/riscy/melpazoid/master/melpazoid/melpazoid.el"
  "Url path to melpazoid's elisp file."
  :type 'string
  :group 'eask)

(provide 'eask-core)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; eask-core.el ends here
