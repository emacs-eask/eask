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

;; ~/lisp/_prepare.el
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
  "Prevent `_prepare.el' loading twice."
  (unless (string= (nth 0 args) (eask-script "_prepare")) (apply fnc args)))
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
  "Source lisp directory; should always end with slash.")
(defun eask-command ()
  "What's the current command?

If the command is with subcommand, it will return command with concatenate with
slash separator. For example, the following:

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
(defun eask-special-p ()
  "Return t if the command that can be run without Eask-file existence."
  (member (eask-command) '("init/cask" "cat" "keywords"
                           "generate/ignore" "generate/license")))
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
    (if (<= len 1) form-1 form-2)))
(defun eask-current-time ()
  "Return current time."
  (let ((now (current-time))) (logior (lsh (car now) 16) (cadr now))))
(defun eask-seq-str-max (sequence)
  "Return max length in list of strings."
  (let ((result 0))
    (mapc (lambda (elm) (setq result (max result (length (eask-2str elm))))) sequence)
    result))
(defun eask-f-filename (path)
  "Return the name of PATH."
  (file-name-nondirectory (directory-file-name path)))
(defun eask-s-replace (old new s)
  "Replace OLD with NEW in S each time it occurs."
  (if (fboundp #'string-replace)
      (string-replace old new s)
    (replace-regexp-in-string (regexp-quote old) new s t t)))
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
(defun eask--update-exec-path ()
  "Add all bin directory to `exec-path'."
  (dolist (entry (directory-files package-user-dir t directory-files-no-dot-files-regexp))
    (when-let* ((bin (expand-file-name "bin" entry))
                ((file-directory-p bin)))
      (add-to-list 'exec-path bin t)))
  (delete-dups exec-path))
(defun eask--update-load-path ()
  "Add all load-path for all .el files."
  (dolist (filename (eask-package-el-files))
    (add-to-list 'load-path (file-name-directory filename) t))
  (delete-dups load-path))
(defun eask-dependencies ()
  "Return list of dependencies."
  (append eask-depends-on (and (eask-dev-p) eask-depends-on-dev)))
(defun eask--package-mapc (func deps)
  "Like function `mapc' but for process package transaction specifically.

For arguments FUNC and DEPS, see function `mapc' for more information."
  (let* ((eask--package-prefix)  ; remain untouch
         (len (length deps))
         (len-str (eask-2str len))
         (fmt (concat "[%" (eask-2str (length len-str)) "d/" len-str "] "))
         (count 0))
    (dolist (pkg deps)
      (cl-incf count)
      (setq eask--package-prefix (format fmt count))
      (funcall func pkg))))
(defun eask--install-deps (dependencies msg)
  "Install DEPENDENCIES."
  (let* ((names (mapcar #'car dependencies))
         (names (mapcar #'eask-intern names))
         (len (length dependencies))
         (ies (eask--sinr len "y" "ies"))
         (pkg-installed (cl-remove-if #'package-installed-p names))
         (installed (length pkg-installed)) (skipped (- len installed)))
    (eask-log "Installing %s %s dependenc%s..." len msg ies)
    (eask-msg "")
    (eask--package-mapc #'eask-package-install names)
    (eask-msg "")
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
    (eask-msg "")
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
            (and (eask-config-p) (not eask--package-initialized)))
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
         (pkg-string (ansi-green (eask-2str pkg)))
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
           (eask-f-source archive)
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
(defvar eask--package-prefix ""
  "The prefix to display before each package action.")
(defun eask-package-install (pkg)
  "Install the package."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask--pkg-process pkg
    (cond
     ((package-installed-p pkg)
      (eask-msg "  - %sSkipping %s (%s)... already installed ✗"
                eask--package-prefix
                name version))
     ((progn
        (eask-pkg-init)
        (unless (eask-package-installable-p pkg)
          (eask-error "Package not installable `%s'; make sure package archive is included" pkg))))
     ((when-let* ((desc (eask-package-desc pkg))
                  (req-emacs (assoc 'emacs (package-desc-reqs desc)))
                  (req-emacs (package-version-join (nth 0 (cdr req-emacs))))
                  ((version< emacs-version req-emacs)))
        (if (eask-strict-p)
            (eask-error "  - %sSkipping %s (%s)... it requires Emacs %s and above ✗"
                        eask--package-prefix
                        pkg (eask-package--version-string pkg) emacs-version)
          (eask-msg "  - %sSkipping %s (%s)... it requires Emacs %s and above ✗"
                    eask--package-prefix
                    name version (ansi-yellow emacs-version)))))
     (t
      (eask--pkg-process pkg
        (eask-with-progress
          (format "  - %sInstalling %s (%s)... " eask--package-prefix name version)
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
      (eask-msg "  - %sSkipping %s (%s)... not installed ✗" eask--package-prefix name version))
     (t
      (eask--pkg-process pkg
        (eask-with-progress
          (format "  - %sUninstalling %s (%s)... " eask--package-prefix name version)
          (eask-with-verbosity 'debug
            (package-delete (eask-package-desc pkg t) (eask-force-p)))
          "done ✓"))))))
(defun eask-package-reinstall (pkg)
  "Reinstall the package."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask--pkg-process pkg
    (cond
     ((not (package-installed-p pkg))
      (eask-msg "  - %sSkipping %s (%s)... not installed ✗" eask--package-prefix name version))
     (t
      (eask-pkg-init)
      (eask--pkg-process pkg
        (eask-with-progress
          (format "  - %sReinstalling %s (%s)... " eask--package-prefix name version)
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
  (if-let ((version (or (eask-package--version pkg t)
                        (eask-package--version pkg nil))))
      (package-version-join version)
    ;; Just in case, but this should never happens!
    "0"))
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
(defconst eask-has-colors (getenv "EASK_HASCOLORS")
  "Return non-nil if terminal support colors.")
(defconst eask-homedir (getenv "EASK_HOMEDIR")
  "Eask temporary storage.")
(defconst eask-invocation (getenv "EASK_INVOCATION")
  "Eask invocation program.")
(defun eask--str2num (str) (ignore-errors (string-to-number str)))
(defun eask--flag (flag)
  "Return non-nil if FLAG exists.."
  (member (concat "--eask" flag) eask-argv))
(defun eask--flag-value (flag)
  "Return value for FLAG."
  (nth 1 (eask--flag flag)))
(defun eask-global-p ()       (eask--flag "-g"))               ; -g, --global
(defun eask-config-p ()       (eask--flag "-c"))               ; -c, --config
(defun eask-local-p ()        (and (not (eask-global-p))
                                   (not (eask-config-p))))     ; local project space
(defun eask-all-p ()          (eask--flag "-a"))               ; -a, --all
(defun eask-quick-p ()        (eask--flag "-q"))               ; -q, --quick
(defun eask-force-p ()        (eask--flag "-f"))               ; -f, --force
(defun eask-dev-p ()          (eask--flag "--dev"))            ; --dev, --development
(defun eask-debug-p ()        (eask--flag "--debug"))          ; --debug
(defun eask-strict-p ()       (eask--flag "--strict"))         ; --strict
(defun eask-timestamps-p ()   (eask--flag "--timestamps"))     ; --timestamps
(defun eask-log-level-p ()    (eask--flag "--log-level"))      ; --log-level
(defun eask-log-file-p ()     (eask--flag "--log-file"))       ; --log-file, --lf
(defun eask-elapsed-time-p () (eask--flag "--elapsed-time"))   ; --elapsed-time, --et
(defun eask-allow-error-p ()  (eask--flag "--allow-error"))    ; --allow-error
(defun eask-insecure-p ()     (eask--flag "--insecure"))       ; --insecure
(defun eask-no-color-p ()     (eask--flag "--no-color"))       ; --no-color
(defun eask-clean-p ()        (eask--flag "--clean"))          ; -c, --clean
(defun eask-json-p ()         (eask--flag "--json"))           ; --json
(defun eask-number-p ()       (eask--flag "--number"))         ; -n, --number
(defun eask-output ()      (eask--flag-value "--output"))       ; --o, --output
(defun eask-proxy ()       (eask--flag-value "--proxy"))        ; --proxy
(defun eask-http-proxy ()  (eask--flag-value "--http-proxy"))   ; --http-proxy
(defun eask-https-proxy () (eask--flag-value "--https-proxy"))  ; --https-proxy
(defun eask-no-proxy ()    (eask--flag-value "--no-proxy"))     ; --no-proxy
(defun eask-destination () (eask--flag-value "--dest"))         ; --dest, --destination
(defun eask-from ()        (eask--flag-value "--from"))         ; --from
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
(defun eask--add-proxy (protocal host)
  "Add proxy."
  (when host (push (cons protocal (eask-proxy)) url-proxy-services)))
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
     "--number"))
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
           (default-directory (cond ((eask-global-p) eask-homedir)
                                    ((eask-config-p) user-emacs-directory)
                                    (t default-directory)))
           (alist))
       (dolist (cmd eask--command-list)
         (push (cons cmd '(lambda (&rest _))) alist))
       (setq command-switch-alist (append command-switch-alist alist))
       ,@body)))
(defconst eask-file-keywords
  '("package" "website-url" "keywords"
    "author" "license"
    "package-file" "package-descriptor" "files"
    "script"
    "source" "source-priority"
    "depends-on" "development"
    "exec-paths" "load-paths")
  "List of Eask file keywords.")
(defun eask--loop-file-keywords (func)
  "Loop through Eask file keywords for environment replacement.  Internal used
for function `eask--alias-env'."
  (dolist (keyword eask-file-keywords)
    (let ((keyword-sym (intern keyword))
          (api (intern (concat "eask-f-" keyword)))    ; existing function
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
  (when (stringp filename)
    (eask-s-replace (or eask-file-root default-directory) "" filename)))
(defun eask-file-load (location &optional noerror)
  "Load Eask file in the LOCATION."
  (when-let* ((target-eask-file (expand-file-name location user-emacs-directory))
              (result (eask--alias-env (load target-eask-file 'noerror t t))))
    (setq eask-file target-eask-file  ; assign eask file only if success
          eask-file-root (file-name-directory target-eask-file))
    (run-hooks 'eask-file-loaded-hook)
    result))
(defun eask--match-file (name)
  "Check to see if NAME is our target Eask-file, then return it."
  (let (;; Ensure path to filename
        (name             (file-name-nondirectory (directory-file-name name)))
        ;; `p-' stards for pattern
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
    (eask-file-load file)))
(defmacro eask--with-hooks (&rest body)
  "Execute BODY with before/after hooks."
  (declare (indent 0) (debug t))
  `(let* ((command (eask-command))
          (before  (concat "eask-before-" command "-hook"))
          (after   (concat "eask-after-" command "-hook")))
     (run-hooks 'eask-before-command-hook)
     (run-hooks (intern before))
     ,@body
     (run-hooks (intern after))
     (run-hooks 'eask-after-command-hook)))
(defmacro eask--setup-home (dir &rest body)
  "Set up config directory in DIR, then execute BODY."
  (declare (indent 1) (debug t))
  `(let* ((user-emacs-directory (expand-file-name (concat ".eask/" emacs-version "/") ,dir))
          (package-user-dir (expand-file-name "elpa" user-emacs-directory))
          (user-init-file (locate-user-emacs-file "init.el"))
          (custom-file (locate-user-emacs-file "custom.el")))
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
          ((or (eask-global-p) (eask-special-p))  ; Commands without Eask-file needed
           (eask--setup-home (concat eask-homedir "../")  ; `/home/user/', escape `.eask'
             (let ((eask--first-init-p (not (file-directory-p user-emacs-directory))))
               ;; We accept Eask-file in `global' scope, but it shouldn't be used
               ;; for the sandbox.
               (eask-with-verbosity 'debug
                 (if (eask-file-try-load "./")
                     (eask-msg "✓ Loading global Eask file in %s... done!" eask-file)
                   (eask-msg "✗ Loading global Eask file... missing!"))
                 (message ""))
               (package-activate-all)
               (ignore-errors (make-directory package-user-dir t))
               (eask--with-hooks ,@body))))
          ((eask-config-p)
           (let ((inhibit-config (eask-quick-p)))
             ;; We accept Eask-file in `config' scope, but it shouldn't be used
             ;; for the sandbox.
             (eask-with-verbosity 'debug
               (if (eask-file-try-load "./")
                   (eask-msg "✓ Loading config Eask file in %s... done!" eask-file)
                 (eask-msg "✗ Loading config Eask file... missing!"))
               (message ""))
             (package-activate-all)
             (eask-with-progress
               (ansi-green "Loading your configuration... ")
               (eask-with-verbosity 'all
                 (unless inhibit-config
                   (load (locate-user-emacs-file "early-init.el") t)
                   (load (locate-user-emacs-file "../.emacs") t)
                   (load (locate-user-emacs-file "init.el") t)))
               (ansi-green (if inhibit-config "skipped ✗" "done ✓")))
             (eask--with-hooks ,@body)))
          (t
           (eask--setup-home nil  ; `nil' is the `default-directory'
             (let ((eask--first-init-p (not (file-directory-p user-emacs-directory))))
               (eask-with-verbosity 'debug
                 (if (eask-file-try-load "./")
                     (eask-msg "✓ Loading Eask file in %s... done!" eask-file)
                   (eask-msg "✗ Loading Eask file... missing!"))
                 (message ""))
               (if (not eask-file)
                   (eask-help "core/init")
                 (package-activate-all)
                 (ignore-errors (make-directory package-user-dir t))
                 (eask--silent (eask-setup-paths))
                 (eask--with-hooks ,@body))))))))))
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
(defvar eask-package            nil)
(defvar eask-package-desc       nil)  ; package descriptor
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
  "Load an Eask FILE and execute BODY with"
  (declare (indent 2) (debug t))
  `(eask--save-eask-file-state
     (eask--setup-env
       (eask--alias-env
         (if (let ((default-directory (file-name-directory ,file)))
               (ignore-errors (eask-file-load ,file)))
             (progn ,success)
           ,@error)))))
(defun eask-package--get (key)
  "Return package info by KEY."
  (plist-get eask-package key))
(defun eask-package-name ()        (eask-package--get :name))
(defun eask-package-version ()     (eask-package--get :version))
(defun eask-package-description () (eask-package--get :description))
(defun eask-depends-emacs-version ()
  "Get Eask-file Emacs version string."
  (nth 0 (cdar eask-depends-on-emacs)))
(defun eask-f-package (name version description)
  "Set the package information."
  (if eask-package
      (eask-error "Multiple definition of `package'")
    (setq eask-package `(:name ,name :version ,version :description ,description))
    (progn  ; Run checker
      (eask--checker-string "Name" name)
      (version= version "0.1.0")
      (eask--checker-string "Description" description))))
(defun eask-f-website-url (url)
  "Set website URL."
  (if eask-website-url
      (eask-error "Multiple definition of `website-url'")
    (setq eask-website-url url)))
(defun eask-f-keywords (&rest keywords)
  "Set package KEYWORDS."
  (if eask-keywords
      (eask-error "Multiple definition of `keywords'")
    (setq eask-keywords keywords)))
(defun eask-f-author (name &optional email)
  "Set package author's NAME and EMAIL."
  (if (member name (mapcar #'car eask-authors))
      (eask-warn "Warning regarding duplicate author name, %s" name)
    (when (and email
               (not (string-match-p "@" email)))
      (eask-warn "Email seems to be invalid, %s" email))
    (push (cons name email) eask-authors)))
(defun eask-f-license (name)
  "Set package license NAME."
  (if (member name eask-licenses)
      (eask-warn "Warning regarding duplicate license name, %s" name)
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
    (eask-msg (concat
               (if eask-package-desc "✓ " "✗ ")
               "Try constructing the package-descriptor (%s)... "
               (cond (eask-package-desc "succeeded!")
                     (skipped "skipped!")
                     (t "failed!")))
              (file-name-nondirectory file))))
(defun eask-f-package-file (file)
  "Set package FILE."
  (if eask-package-file
      (eask-error "Multiple definition of `package-file'")
    (setq eask-package-file (expand-file-name file))
    (if (file-exists-p eask-package-file)
        (eask--try-construct-package-desc eask-package-file)
      (eask-warn "Package-file seems to be missing `%s'" file))
    (when-let
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
    (eask-error "Multiple definition of `package-descriptor'"))
   ((and eask-package-desc                ; check if construct successfully
         (equal (eask-pkg-el) pkg-file))  ; check filename the same
    )                                     ; ignore
   (t
    (setq eask-package-descriptor (expand-file-name pkg-file))
    (cond ((not (string-suffix-p "-pkg.el" eask-package-descriptor))
           (eask-error "Pkg-file must end with `-pkg.el'"))
          ((not (file-exists-p eask-package-descriptor))
           (eask-warn "Pkg-file seems to be missing `%s'" pkg-file))
          (t
           (eask--try-construct-package-desc eask-package-descriptor))))))
(defun eask-f-files (&rest patterns)
  "Set files PATTERNS."
  (setq eask-files (append eask-files patterns)))
(defun eask-f-script (name command &rest args)
  "Add scripts' command."
  (when (symbolp name) (setq name (eask-2str name)))  ; ensure to string, accept symbol
  (when (assoc name eask-scripts)
    (eask-error "Run-script with the same key name is not allowed: `%s`" name))
  (push (cons name
              (mapconcat #'identity (append (list command) args) " "))
        eask-scripts))
(defun eask-f-source (name &optional location)
  "Add archive NAME with LOCATION."
  (when (symbolp name) (setq name (eask-2str name)))  ; ensure to string, accept symbol
  (when (assoc name package-archives)
    (eask-error "Multiple definition of source `%s'" name))
  (setq location (or location (cdr (assq (intern name) eask-source-mapping))))
  (unless location (eask-error "Unknown package archive `%s'" name))
  (when (and location
             (gnutls-available-p)
             (not (eask-network-insecure-p)))
    (setq location (eask-s-replace "https://" "http://" location)))
  (add-to-list 'package-archives (cons name location) t))
(defun eask-f-source-priority (archive-id &optional priority)
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
(defun eask-f-depends-on (pkg &rest args)
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
    (let* ((minimum-version (or (car args) "0"))
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
(defun eask-f-development (&rest dep)
  "Development scope with list of DEP."
  (setq dep (cl-remove-if #'null dep))  ; make sure no `nil', see #143
  (dolist (pkg dep)
    (push pkg eask-depends-on-dev)
    (delete-dups eask-depends-on-dev)
    (setq eask-depends-on (remove pkg eask-depends-on))))
(defun eask-f-exec-paths (&rest dirs)
  "Add all DIRS to exec-path."
  (dolist (dir dirs) (add-to-list 'exec-path (expand-file-name dir) t)))
(defun eask-f-load-paths (&rest dirs)
  "Add all DIRS to load-path."
  (dolist (dir dirs) (add-to-list 'load-path (expand-file-name dir) t)))
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
(defun eask--warn (fnc &rest args)
  "On warn."
  (let ((msg (eask--ansi 'warn (apply #'format-message args))))
    (eask--unsilent (eask-msg "%s" msg))
    (run-hook-with-args 'eask-on-warning-hook 'warn msg))
  (eask--silent (apply fnc args)))
(defcustom eask-verbosity 3
  "Log level for all messages; 4 means trace most anything, 0 means nothing.

Standard is, 0 (error), 1 (warning), 2 (info), 3 (log), 4 (debug), 5 (all)."
  :type 'integer)
(defcustom eask-timestamps nil
  "Log messages with timestamps."
  :type 'boolean)
(defcustom eask-log-level nil
  "Log messages with level."
  :type 'boolean)
(defcustom eask-level-color
  '((all   . ansi-magenta)
    (debug . ansi-blue)
    (log   . ansi-white)
    (info  . ansi-cyan)
    (warn  . ansi-yellow)
    (error . ansi-red))
  "Alist of each log level's color, in (SYMBOL . ANSI-FUNCTION)."
  :type 'alist)
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
  (let* ((string (eask-s-replace "✓" (ansi-green "✓") string))
         (string (eask-s-replace "✗" (ansi-red "✗") string))
         (string (eask-s-replace "💡" (ansi-yellow "💡") string)))
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
  (when-let ((elcs (mapcar (lambda (elm) (concat elm "c")) (eask-package-el-files))))
    (setq elcs (cl-remove-if-not (lambda (elm) (file-exists-p elm)) elcs))
    elcs))
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
(defun eask--help-display ()
  "Display help instruction."
  (goto-char (point-min))
  (let ((max-column 0))
    (while (not (eobp))
      (forward-line 1)
      (goto-char (line-beginning-position))
      (insert "  ")
      (goto-char (line-end-position))
      (setq max-column (max (current-column) max-column)))
    (eask-msg (concat "''" (spaces-string max-column) "''"))
    (eask-msg (ansi-white (buffer-string)))
    (eask-msg (concat "''" (spaces-string max-column) "'" "'"))))
(defun eask-help (command)
  "Show COMMAND's help instruction."
  (let* ((command (eask-2str command))  ; convert to string
         (help-file (concat eask-lisp-root "help/" command)))
    (if (file-exists-p help-file)
        (with-temp-buffer
          (insert-file-contents help-file)
          (unless (string= (buffer-string) "")
            (eask--help-display)))
      (eask-error "Help manual missig %s" help-file))))
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
       (format "Unmatched website URL '%s'; add %s to %s" eask-website-url
               (if (string-prefix-p "-pkg.el" def-point)
                   (format ":url \"%s\"" eask-website-url)
                 (format ";; URL: %s" eask-website-url))
               def-point)
       (format "Unmatched website URL '%s'; add (website-url \"%s\") to Eask-file" url url)
       (format "URL header is optional, but it's often recommended")))
    (let ((keywords (eask-package-desc-keywords)))
      (cond
       ((or keywords eask-keywords)
        (dolist (keyword keywords)
          (unless (member keyword eask-keywords)
            (eask-warn "Unmatched keyword '%s'; add (keywords \"%s\") to Eask-file or consider removing it" keyword keyword)))
        (dolist (keyword eask-keywords)
          (unless (member keyword keywords)
            (eask-warn "Unmatched keyword '%s'; add %s to %s or consider removing it"
                       keyword
                       (if (string-prefix-p "-pkg.el" def-point)
                           (format ":keywords '(\"%s\")" keyword)
                         (format ";; Keywords: %s" keyword))
                       def-point))))
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
(defun eask--checker-string (name var)
  "Run checker for VAR."
  (unless (stringp var)
    (eask-error "%s must be a string" name))
  (when (string-empty-p var)
    (eask-warn "%s cannot be an empty string" name)))
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
(defvar eask-lint-first-file-p nil
  "Set the flag to `t' after the first file is linted.")
(defun eask-lint-first-newline ()
  "Built-in linters will create extra newline, prevent that!"
  (when eask-lint-first-file-p
    (eask-msg "")
    (setq eask-lint-first-file-p t)))

;; ~/lisp/checker/check-eask.el
(defvar eask--checker-log nil)
(defvar eask--checker-warnings nil)
(defvar eask--checker-errors nil)
(defun eask--pretty-json (json)
  "Return pretty JSON."
  (with-temp-buffer (insert json) (json-pretty-print-buffer) (buffer-string)))
(defun eask--column-at-point (point)
  "Get column at POINT."
  (save-excursion (goto-char point) (current-column)))
(defun eask--load-buffer ()
  "Return the current file loading session."
  (car (cl-remove-if-not
        (lambda (elm) (string-prefix-p " *load*-" (buffer-name elm))) (buffer-list))))
(defun eask--write-json-format (level msg)
  "Prepare log for JSON format."
  (let* ((thing (thing-at-point 'sexp))
         (bounds (bounds-of-thing-at-point 'sexp))
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
            (message . ,msg))
          (cl-case level
            (`error eask--checker-errors)
            (`warn  eask--checker-warnings)))))
(defun eask--write-plain-text (level msg)
  "Prepare log for plain text format."
  (let* ((level-string (cl-case level
                         (`error "Error")
                         (`warn  "Warning")))
         (log (format "%s:%s:%s %s: %s"
                      (or load-file-name eask-file)
                      (if load-file-name (line-number-at-pos) 0)
                      (if load-file-name (current-column) 0)
                      level-string
                      msg)))
    (push (ansi-color-filter-apply log) eask--checker-log)))
(defun eask--write-log (level msg)
  "Write the log."
  (unless (string= " *temp*" (buffer-name))  ; avoid error from `package-file' directive
    (with-current-buffer (or (eask--load-buffer) (buffer-name))
      (funcall
       (cond ((eask-json-p) #'eask--write-json-format)
             (t             #'eask--write-plain-text))
       level msg))))
(defun eask--check-file (files)
  "Lint list of Eask FILES."
  (let (checked-files content)
    ;; Linting
    (dolist (file files)
      (eask--save-load-eask-file file
          (push file checked-files)))

    ;; Print result
    (eask-msg "")
    (cond ((and (eask-json-p)  ; JSON format
                (or eask--checker-warnings eask--checker-errors))
           (setq content
                 (eask--pretty-json (json-encode
                                     `((warnings . ,eask--checker-warnings)
                                       (errors   . ,eask--checker-errors)))))
           (eask-msg content))
          (eask--checker-log  ; Plain text
           (setq content
                 (with-temp-buffer
                   (dolist (msg (reverse eask--checker-log))
                     (insert msg "\n"))
                   (buffer-string)))
           (mapc #'eask-msg (reverse eask--checker-log)))
          (t
           (eask-info "(Checked %s file%s)"
                      (length checked-files)
                      (eask--sinr checked-files "" "s"))))

    ;; Output file
    (when (and content (eask-output))
      (write-region content nil (eask-output)))))

;; ~/lisp/clean/all.el
(defvar eask-no-cleaning-operation-p nil
  "Set to non-nil if there is no cleaning operation done.")
(defvar eask--clean-tasks-count 0
  "Count cleaning task.")
(defvar eask--clean-tasks-cleaned 0
  "Total cleaned tasks")
(defmacro eask--clean-section (title &rest body)
  "Print clean up TITLE and execute BODY."
  (declare (indent 1))
  `(let (eask-no-cleaning-operation-p)
     (cl-incf eask--clean-tasks-count)
     (eask-with-progress
       (concat "  - [" (eask-2str eask--clean-tasks-count) "/6] "
               (format "%s... " ,title))
       (eask-with-verbosity 'debug ,@body)
       (if eask-no-cleaning-operation-p
           "skipped ✗"
         (cl-incf eask--clean-tasks-cleaned)
         "done ✓"))))

;; ~/lisp/clean/autoloads.el

;; ~/lisp/clean/dist.el
(defun eask--clean-dist (path)
  "Clean up dist PATH."
  (let* ((name (eask-guess-package-name))
         (version (eask-package-version))
         (readme (expand-file-name (format "%s-readme.txt" name) path))
         (entry (expand-file-name (format "%s-%s.entry" name version) path))
         (packaged (eask-packaged-file))
         (deleted 0)
         (delete-dir))
    (when (eask-delete-file readme)   (cl-incf deleted))
    (when (eask-delete-file entry)    (cl-incf deleted))
    (when (eask-delete-file packaged) (cl-incf deleted))
    (when (and (not (zerop deleted)) (directory-empty-p path))
      (eask-with-progress
        (format "The dist folder %s seems to be empty, delete it as well... " path)
        (ignore-errors (delete-directory path))
        "done ✓")
      (setq delete-dir t))
    (eask-msg "")
    (eask-info "(Total of %s file%s, and %s directory deleted)" deleted
               (eask--sinr deleted "" "s")
               (if delete-dir "1" "0"))))

;; ~/lisp/clean/elc.el

;; ~/lisp/clean/log-file.el
(defun eask--clean-log (path)
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
    (when (and (not (zerop deleted)) (directory-empty-p path))
      (eask-with-progress
        (format "The dist folder %s seems to be empty, delete it as well... " path)
        (ignore-errors (delete-directory path))
        "done ✓")
      (setq delete-dir t))
    (eask-msg "")
    (eask-info "(Total of %s log file%s deleted, %s skipped)" deleted
               (eask--sinr deleted "" "s")
               (- (length log-files) deleted))))

;; ~/lisp/clean/pkg-file.el

;; ~/lisp/clean/workspace.el

;; ~/lisp/core/archives.el
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
(defun eask--print-archive-alist (alist)
  "Print the archvie ALIST."
  (let* ((names (mapcar #'car alist))
         (eask--length-name (eask-2str (eask-seq-str-max names)))
         (urls (mapcar #'cdr alist))
         (eask--length-url (eask-2str (eask-seq-str-max urls)))
         (priorities (mapcar #'cdr package-archive-priorities))
         (eask--length-priority (eask-2str (eask-seq-str-max priorities))))
    (mapc #'eask--print-archive alist)))

;; ~/lisp/core/cat.el

;; ~/lisp/core/compile.el
(defconst eask-compile-log-buffer-name "*Compile-Log*"
  "Byte-compile log buffer name.")
(defun eask--print-compile-log ()
  "Print `*Compile-Log*' buffer."
  (when (get-buffer eask-compile-log-buffer-name)
    (with-current-buffer eask-compile-log-buffer-name
      (if (and (eask-clean-p) (eask-strict-p))
          (eask-error (buffer-string))  ; Exit with error code!
        (eask-print-log-buffer))
      (eask-msg ""))))
(defun eask--byte-compile-file-external-contetnt (filename cmd)
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
(defun eask--byte-compile-file-external (filename)
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
         (content (eask--byte-compile-file-external-contetnt filename cmd)))
    (if (string-empty-p content)
        t  ; no error, good!
      (with-current-buffer (get-buffer-create eask-compile-log-buffer-name)
        (insert content)))))
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
        (setq result (if (eask-clean-p)
                         (eask--byte-compile-file-external filename)
                       (byte-compile-file filename))
              result (eq result t)))
      (if result "done ✓" "skipped ✗"))
    (eask--print-compile-log)
    result))
(defun eask--compile-files (files)
  "Compile sequence of FILES."
  (let* ((compiled (cl-remove-if-not #'eask--byte-compile-file files))
         (compiled (length compiled))
         (skipped (- (length files) compiled)))
    ;; XXX: Avoid last newline from the log buffer!
    (unless (get-buffer eask-compile-log-buffer-name)
      (eask-msg ""))
    (eask-info "(Total of %s file%s compiled, %s skipped)" compiled
               (eask--sinr compiled "" "s")
               skipped)))

;; ~/lisp/core/concat.el

;; ~/lisp/core/emacs.el

;; ~/lisp/core/eval.el

;; ~/lisp/core/exec-path.el
(defun eask--print-exec-path (path)
  "Print out the PATH."
  (message "%s" path))

;; ~/lisp/core/exec.el
(defconst eask--exec-path-file (expand-file-name "exec-path" eask-homedir)
  "Target file to export the `exec-path' variable.")
(defconst eask--load-path-file (expand-file-name "load-path" eask-homedir)
  "Target file to export the `load-path' variable.")
(defun eask--export-env ()
  "Export environments."
  (ignore-errors (delete-file eask--exec-path-file))
  (ignore-errors (delete-file eask--load-path-file))
  (ignore-errors (make-directory eask-homedir t))  ; generate dir `~/.eask/'
  (write-region (getenv "PATH") nil eask--exec-path-file)
  (write-region (getenv "EMACSLOADPATH") nil eask--load-path-file))

;; ~/lisp/core/files.el
(defun eask--print-filename (filename)
  "Print out the FILENAME."
  (message "%s" filename))

;; ~/lisp/core/info.el
(defvar eask--max-offset 0)
(defun eask--print-deps (title dependencies)
  "Print dependencies."
  (when dependencies
    (eask-msg "")
    (eask-msg title)
    (let* ((names (mapcar #'car dependencies))
           (offset (eask-seq-str-max names)))
      (setq eask--max-offset (max offset eask--max-offset)
            offset (eask-2str eask--max-offset))
      (dolist (dep dependencies)
        (let* ((target-version (cdr dep))
               (target-version (if (= (length target-version) 1)
                                   (nth 0 target-version)
                                 "specified")))
          (eask-msg (concat "  %-" offset "s (%s)") (car dep) target-version)
          (eask-debug "    Recipe: %s" (car dep)))))))

;; ~/lisp/core/install-deps.el

;; ~/lisp/core/install.el
(defun eask--install-packages (names)
  "Install packages."
  (let* ((names (mapcar #'eask-intern names))
         (len (length names)) (s (eask--sinr len "" "s"))
         (pkg-not-installed (cl-remove-if #'package-installed-p names))
         (installed (length pkg-not-installed)) (skipped (- len installed)))
    (eask-log "Installing %s specified package%s..." len s)
    (eask-msg "")
    (eask--package-mapc #'eask-package-install names)
    (eask-msg "")
    (eask-info "(Total of %s package%s installed, %s skipped)"
               installed s skipped)))
(defun eask--package-install-file (file)
  ;; Workaround: `package-install-file' fails when FILE is .el and contains CRLF EOLs:
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=48137
  (if (not (string-match "\\.el\\'" file))
      (package-install-file file)

    ;; load package file and check if it contains CRLFs
    (with-temp-buffer
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (if (not (search-forward "\r\n" nil t))
          (package-install-file file) ;; no cllf

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

;; ~/lisp/core/list.el
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

;; ~/lisp/core/load-path.el
(defun eask--print-load-path (path)
  "Print out the PATH."
  (message "%s" path))
(defun eask--filter-path (path)
  "Filter the PATH out by search regex."
  (cl-some (lambda (regex)
             (string-match-p regex path))
           (eask-args)))

;; ~/lisp/core/load.el

;; ~/lisp/core/outdated.el

;; ~/lisp/core/package-directory.el

;; ~/lisp/core/package.el
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
  "Form a directory recipe."
  (eask-load "extern/package-recipe")
  (let* ((name (eask-guess-package-name))
         (patterns (eask-package-dir--patterns))
         (path default-directory)
         (rcp (package-directory-recipe name :name name :files patterns :dir path)))
    (setf (slot-value rcp 'version) version)
    (setf (slot-value rcp 'time) (eask-current-time))
    rcp))
(defun eask-packaged-name ()
  "Find a possible packaged name."
  (let ((name (eask-guess-package-name))
        (version (eask-package-version)))
    (concat name "-" version)))
(defun eask--packaged-file (ext)
  "Find a possible packaged file."
  (expand-file-name (concat (eask-packaged-name) "." ext) eask-dist-path))
(defun eask-packaged-file ()
  "Return generated package artifact; it could be a tar or el."
  (if (eask-package-multi-p) (eask--packaged-file "tar")
    (eask--packaged-file "el")))

;; ~/lisp/core/recipe.el

;; ~/lisp/core/refresh.el

;; ~/lisp/core/reinstall.el
(defun eask--reinstall-packages (names)
  "Install packages."
  (let* ((names (mapcar #'eask-intern names))
         (len (length names)) (s (eask--sinr len "" "s"))
         (pkg-not-installed (cl-remove-if #'package-installed-p names))
         (installed (length pkg-not-installed)) (skipped (- len installed)))
    (eask-log "Reinstalling %s specified package%s..." len s)
    (eask-msg "")
    (eask--package-mapc #'eask-package-reinstall names)
    (eask-msg "")
    (eask-info "(Total of %s package%s reinstalled, %s skipped)"
               installed s skipped)))

;; ~/lisp/core/run.el
(defconst eask--run-file (expand-file-name "run" eask-homedir)
  "Target file to export the `run' scripts.")
(defun eask--print-scripts ()
  "Print all available scripts."
  (eask-msg "available via `eask run-script`")
  (eask-msg "")
  (let* ((keys (mapcar #'car (reverse eask-scripts)))
         (offset (eask-seq-str-max keys))
         (fmt (concat "  %-" (eask-2str offset) "s  %s")))
    (dolist (key keys)
      (eask-msg fmt key (cdr (assoc key eask-scripts))))
    (eask-msg "")
    (eask-info "(Total of %s available script%s)" (length keys)
               (eask--sinr keys "" "s"))))
(defun eask--export-command (command)
  "Export COMMAND instruction."
  (ignore-errors (make-directory eask-homedir t))  ; generate dir `~/.eask/'
  (write-region (concat command "\n") nil eask--run-file t))
(defun eask--unmatched-scripts (scripts)
  "Return a list of scripts that cannot be found in `eask-scripts'."
  (let (unmatched)
    (dolist (script scripts)
      (unless (assoc script eask-scripts)
        (push script unmatched)))
    unmatched))

;; ~/lisp/core/search.el
(defun eask--search-packages (query)
  "Filter available packages with QUERY."
  (let ((result))
    (dolist (package (mapcar #'car package-archive-contents))
      (when (string-match-p query (eask-2str package))
        (push package result)))
    result))

;; ~/lisp/core/status.el

;; ~/lisp/core/uninstall.el
(defun eask--uninstall-packages(names)
  "Uninstall packages."
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
(defun eask--package-version-string (pkg-desc)
  "Get package version string with color."
  (let ((version (package-desc-version pkg-desc)))
    (ansi-yellow (package-version-join version))))
(defun eask-package-upgrade (pkg-desc)
  "Upgrade package using PKG-DESC."
  (let* ((name (package-desc-name pkg-desc))
         (pkg-string (ansi-green (eask-2str name)))
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
        (eask-msg "")
        (eask-info "(Done upgrading all packages)"))
    (eask-msg "")
    (eask-info "(All packages are up to date)")))

;; ~/lisp/create/elpa.el
(defconst eask--template-elpa-name "template-elpa"
  "Holds template project name.")

;; ~/lisp/create/package.el
(defconst eask--template-project-name "template-elisp"
  "Holds template project name.")
(defun eask--replace-string-in-buffer (old new)
  "Replace OLD to NEW in buffer."
  (let ((str (buffer-string)))
    (setq str (eask-s-replace old new str))
    (delete-region (point-min) (point-max))
    (insert str)))
(defun eask--get-user ()
  "Return user name."
  (string-trim (shell-command-to-string "git config user.name")))
(defun eask--get-mail ()
  "Return user email."
  (string-trim (shell-command-to-string "git config user.email")))

;; ~/lisp/generate/workflow/circle-ci.el
(defun eask--circle-ci-insert-jobs (version)
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
(defun eask--gitlab-insert-jobs (version)
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
(defun eask--print-ignore-menu ()
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
(defun eask--print-license-menu ()
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
                                 "0")))
                       (append eask-depends-on-emacs eask-depends-on))))
    (setq eask--pkg-filename pkg-file)
    (write-region
     (pp-to-string `(define-package ,name ,version ,description ',reqs))
     nil pkg-file)))

;; ~/lisp/init/cask.el
(defun eask--convert-cask (filename)
  "Convert Cask FILENAME to Eask."
  (let* ((filename (expand-file-name filename))
         (file (file-name-nondirectory (eask-root-del filename)))
         (new-file (eask-s-replace "Cask" "Eask" file))
         (new-filename (expand-file-name new-file))
         (converted))
    (eask-with-progress
      (format "Converting file `%s` to `%s`... " file new-file)
      (eask-with-verbosity 'debug
        (cond ((not (string-prefix-p "Cask" file))
               (eask-debug "✗ Invalid Cask filename, the file should start with `Cask`"))
              ((file-exists-p new-filename)
               (eask-debug "✗ The file `%s` already presented" new-file))
              (t
               (with-current-buffer (find-file new-filename)
                 (insert-file-contents file)
                 (goto-char (point-min))
                 (while (re-search-forward "(source " nil t)
                   (forward-word 1)
                   (forward-word -1)  ; make sure infront of the word
                   (insert "'"))      ; make it symbol
                 (save-buffer))
               (setq converted t))))
      (if converted "done ✓" "skipped ✗"))
    converted))

;; ~/lisp/link/add.el
(defun eask--package-desc-reqs (desc)
  "Return a list of requirements from package DESC."
  (cl-remove-if (lambda (name) (string= name "emacs"))
                (mapcar #'car (package-desc-reqs desc))))
(defvar eask--link-package-name    nil "Used to form package name.")
(defvar eask--link-package-version nil "Used to form package name.")
(defun eask--create-link (name source)
  "Add link with NAME to PATH."
  (let* ((dir-name (format "%s-%s" eask--link-package-name eask--link-package-version))
         (link-path (expand-file-name dir-name package-user-dir)))
    (when (file-exists-p link-path)
      (eask-msg "")
      (eask-with-progress
        (ansi-yellow "!! The link is already presented; override the existing link... ")
        (eask--delete-symlink link-path)
        (ansi-yellow "done !!")))
    (make-symbolic-link source link-path)
    (eask-msg "")
    (eask-info "(Created link from `%s` to `%s`)" source (eask-f-filename link-path))))

;; ~/lisp/link/delete.el
(defun eask--delete-symlink (path)
  "Delete symlink PATH."
  (ignore-errors (delete-file path))
  (ignore-errors (delete-directory path t)))
(defun eask--delete-link (name)
  "Delete a link by its' NAME."
  (let* ((links (eask--links))
         (source (assoc name links))
         (link (expand-file-name name package-user-dir)))
    (if (and source (file-symlink-p link))
        (progn
          (eask--delete-symlink link)
          (eask-info "✓ Package `%s` unlinked" name)
          t)
      (eask-info "✗ No linked package name `%s`" name)
      nil)))

;; ~/lisp/link/list.el
(defun eask--links ()
  "Return a list of all links."
  (mapcar
   (lambda (file)
     (cons (eask-f-filename file) (file-truename file)))
   (cl-remove-if-not #'file-symlink-p (directory-files package-user-dir t))))
(defun eask--print-link (link offset)
  "Print information regarding the LINK.

The argument OFFSET is used to align the result."
  (message (concat "  %-" (eask-2str offset) "s  %s") (car link) (cdr link)))

;; ~/lisp/lint/checkdoc.el
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
(defun eask--checkdoc-file (filename)
  "Run checkdoc on FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         (eask--checkdoc-errors))
    (eask-lint-first-newline)
    (eask-msg "`%s` with checkdoc (%s)" (ansi-green file) checkdoc-version)
    (checkdoc-file filename)
    (unless eask--checkdoc-errors (eask-msg "No issues found"))))

;; ~/lisp/lint/declare.el
(defun eask--check-declare-file (filename)
  "Run check-declare on FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         (errors))
    (eask-lint-first-newline)
    (eask-msg "`%s` with check-declare" (ansi-green file))
    (setq errors (check-declare-file filename))
    (if errors
        (with-current-buffer check-declare-warning-buffer
          (eask-msg (buffer-string)))
      (eask-msg "No issues found"))))

;; ~/lisp/lint/elint.el
(defun eask--elint-file (filename)
  "Run elint on FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         (noninteractive))
    (eask-lint-first-newline)
    (eask-msg "`%s` with elint" (ansi-green file))
    (eask-with-verbosity 'debug (elint-file filename))
    (eask-print-log-buffer (elint-get-log-buffer))
    (kill-buffer (elint-get-log-buffer))))

;; ~/lisp/lint/elisp-lint.el
(defconst eask--elisp-lint-version nil
  "`elisp-lint' version.")
(defun eask--elisp-lint-process-file (filename)
  "Process FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         success)
    (eask-msg "")
    (eask-msg "`%s` with elisp-lint (%s)" (ansi-green file) eask--elisp-lint-version)
    (eask-with-verbosity 'debug
      (setq success (elisp-lint-file filename)))
    ;; Report result!
    (cond (success
           (eask-msg "No issues found"))
          ((eask-strict-p)
           (eask-error "Linting failed")))))

;; ~/lisp/lint/elsa.el
(defconst eask--elsa-version nil
  "Elsa version.")
(defun eask--elsa-analyse-file (filename)
  "Process FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         errors)
    (eask-msg "")
    (eask-msg "`%s` with elsa (%s)" (ansi-green file) eask--elsa-version)
    (eask-with-verbosity 'debug
      (setq errors (oref (elsa-analyse-file filename elsa-global-state) errors)))
    (if errors
        (--each (reverse errors)
          (let ((line (string-trim (concat file ":" (elsa-message-format it)))))
            (cond ((string-match-p "[: ][Ee]rror:" line) (eask-error line))
                  ((string-match-p "[: ][Ww]arning:" line) (eask-warn line))
                  (t (eask-log line)))))
      (eask-msg "No issues found"))))

;; ~/lisp/lint/indent.el
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

;; ~/lisp/lint/keywords.el
(defun eask--defined-keywords (keywords)
  "Return t if KEYWORDS are defined correctly."
  (let ((available-keywords (mapcar #'car finder-known-keywords))
        (result))
    (dolist (keyword keywords)
      (when (memq (intern keyword) available-keywords)
        (setq result t)))
    result))

;; ~/lisp/lint/license.el
(defun eask--string-match-all (regexps)
  "Return t when every REGEXPS match the `buffer-string'."
  (cl-every (lambda (regexp)
              (string-match-p regexp (buffer-string)))
            regexps))
(defun eask--scan-license (file)
  "Scan the license FILE."
  (with-current-buffer (find-file file)
    ;; See https://api.github.com/licenses
    (cond
     ((eask--string-match-all '("GNU AFFERO GENERAL PUBLIC LICENSE"
                                "Version 3"
                                "You should have received a copy of the GNU Affero General Public License"))
      '("agpl-3.0" "GNU Affero General Public License v3.0" "AGPL-3.0"))
     ((eask--string-match-all '("Apache"
                                "http://www.apache.org/licenses/"))
      '("apache-2.0" "Apache License 2.0" "Apache-2.0"))
     ((eask--string-match-all '("BSD 2-Clause"))
      '("bsd-2-clause" "BSD 2-Clause \"Simplified\" License" "BSD-2-Clause"))
     ((eask--string-match-all '("BSD 3-Clause"))
      '("bsd-3-clause" "BSD 3-Clause \"New\" or \"Revised\" License" "BSD-3-Clause"))
     ((eask--string-match-all '("Boost Software License - Version 1.0"))
      '("bsl-1.0" "Boost Software License 1.0" "BSL-1.0"))
     ((eask--string-match-all '("CC0 1.0"))
      '("cc0-1.0" "Creative Commons Zero v1.0 Universal" "CC0-1.0"))
     ((eask--string-match-all '("Eclipse Public License - v 2.0"
                                "Eclipse Foundation"))
      '("epl-2.0" "Eclipse Public License 2.0" "EPL-2.0"))
     ((eask--string-match-all '("GNU General Public License"
                                "Version 2"))
      '("gpl-2.0" "GNU General Public License v2.0" "GPL-2.0"))
     ((eask--string-match-all '("GNU General Public License"
                                "version 3"
                                "You should have received a copy of the GNU General Public License"))
      '("gpl-3.0" "GNU General Public License v3.0" "GPL-3.0"))
     ((eask--string-match-all '("Lesser GPL"
                                "Version 2.1"))
      '("lgpl-2.1" "GNU Lesser General Public License v2.1" "LGPL-2.1"))
     ((eask--string-match-all '("Permission is hereby granted, free of charge, to any person obtaining a copy"))
      '("mit" "MIT License" "MIT"))
     ((eask--string-match-all '("Mozilla Public License Version 2.0"
                                "http://mozilla.org/MPL/2.0/"))
      '("mpl-2.0" "Mozilla Public License 2.0" "MPL-2.0"))
     ((eask--string-match-all '("This is free and unencumbered software released into the public domain"
                                "https://unlicense.org"))
      '("unlicense" "The Unlicense" "Unlicense"))
     (t
      '("unknown" "Unknown license" "unknown")))))
(defun eask--print-scanned-license (scanned)
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

;; ~/lisp/lint/package.el
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

;; ~/lisp/lint/regexps.el
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
      (unless errors
        (eask-msg "No issues found"))
      (kill-this-buffer))))

;; ~/lisp/test/activate.el

;; ~/lisp/test/buttercup.el

;; ~/lisp/test/ert-runner.el

;; ~/lisp/test/ert.el
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

(provide 'eask-core)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; eask-core.el ends here
