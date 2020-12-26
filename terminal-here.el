;;; terminal-here.el --- Run an external terminal in current directory -*- lexical-binding: t; -*-

;; Copyright © 2017 David Shepherd

;; Author: David Shepherd <davidshepherd7@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))
;; Keywords: tools, frames
;; URL: https://github.com/davidshepherd7/terminal-here


;;; Commentary:

;; Provides commands to help open external terminal emulators in the
;; directory of the current buffer.


;;; Code:

(require 'cl-lib)

;; TODO: it would be nice not to need to load all of tramp just for the file
;; name parsing. I'm not sure if that's possible though.
(require 'tramp)



(defgroup terminal-here nil
  "Open external terminal emulators in the current buffer's directory."
  :group 'external
  :prefix "terminal-here-")

(defcustom terminal-here-terminal-command
  (list
   (cons 'gnu/linux  'x-terminal-emulator)
   (cons 'darwin     'terminal-app)
   (cons 'windows-nt 'cmd)
   (cons 'cygwin     'cmd)
   (cons 'ms-dos     'cmd)
   )
  "Specification of the command to use to start a terminal.

RECOMMENDED:

Alist of terminals to use for each OS.

The terminal should usually be a key from `terminal-here-terminal-command-table', the OS symbol should
be a possible value of `system-type'.

To use a terminal which is not in `terminal-here-terminal-command-table' you can also set the terminal
part to a list of strings representing the command line to run, which will be passed to `start-process'.

For advanced use cases the terminal can be set to a function which accepts a directory to launch
in and returns a list of strings to pass to `start-process'.



LEGACY:

Previously `terminal-here-terminal-command` contained either a list of strings representing the command
line to run or a function returning such a list. This is still supported but probably not
useful anymore except for backwards compatibility."
  :group 'terminal-here
  :type '(choice
          (repeat (cons (choice (symbol)
                                (repeat string)
                                (function))))
          (repeat string)
          (function)))

(defcustom terminal-here-command-flag
  nil
  "The flag to tell your terminal to treat the rest of the line as a command to run
Typically this is -e, gnome-terminal uses -x.

NOTE: If `terminal-here-terminal-command' is a symbol then this variable is ignored and the flag is looked up in
`terminal-here-command-flag-table' instead."
  :group 'terminal-here
  :type 'string)

(defcustom terminal-here-project-root-function
  nil
  "Function called to find the current project root directory.

If nil falls back to `projectile-project-root', (which requires
you install the `projectile' package), or `vc-root-dir' which is
available in Emacs >= 25.1.

The function should return nil or signal an error if the current
buffer is not in a project."
  :group 'terminal-here
  :type '(choice (const nil) function))


(defcustom terminal-here-terminal-command-table
  (list
   ;; Linux
   (cons 'urxvt               (list "urxvt"))
   (cons 'gnome-terminal      (list "gnome-terminal"))
   (cons 'alacritty           (list "alacritty"))
   (cons 'st                  #'terminal-here--find-and-run-st)
   (cons 'konsole             (list "konsole"))
   ;; A default which points to whichever terminal the user configures using
   ;; debconf (or more likely: as part of apt install).
   (cons 'x-terminal-emulator (list "x-terminal-emulator"))

   ;; Mac OS
   (cons 'terminal-app        (list "open" "-a" "Terminal.app" "."))
   (cons 'iterm-app           (list "open" "-a" "iTerm.app" "."))

   ;; Windows
   ;; From http://stackoverflow.com/a/13509208/874671
   (cons 'cmd                 (list "cmd.exe" "/C" "start" "cmd.exe")))
  "A table of terminal commands.

The keys should be symbols, the values should be either a list of
strings: (terminal-binary arg1 arg2 ...); or a function taking a
directory and returning such a list.

When you add a new entry here you should also add an entry to `terminal-here-command-flag-table'
if you want to use terminal-here with tramp files to create ssh connections.
"
  :group 'terminal-here
  :type '(repeat (cons symbol
                       (choice (repeat string)
                               (function)))))

(defcustom terminal-here-command-flag-table
  (list
   (cons 'urxvt          "-e")
   (cons 'gnome-terminal "-x")
   (cons 'alacritty      "-e")
   (cons 'st             "-e")
   (cons 'konsole        "-e") ;; ssh seems to immediately exit with konsole

   ;; I don't know how to do this on any Mac or Windows terminals! PRs please!
   )
  "A table of flags to tell terminals to use the rest of the line as a command to run."
  :group 'terminal-here
  :type '(repeat (cons symbol
                       (choice (repeat string)
                               (function)))))



;;; Terminal command configuration

(defun terminal-here--non-function-symbol-p (x)
  (and (symbolp x) (not (functionp x))))

(defun terminal-here--per-os-command-table-p (x)
  (and (listp x)
       (consp (car x))
       (terminal-here--non-function-symbol-p (caar x))))

(defun terminal-here--maybe-lookup-os-terminal-command (table-or-term-spec)
  (if (not (terminal-here--per-os-command-table-p table-or-term-spec))
      table-or-term-spec
    (let ((term-spec (alist-get system-type table-or-term-spec)))
      (unless term-spec
        (user-error "`terminal-here-terminal-command' looks like a table of terminals for each OS, but no settings were found for the current `system-type': %s" system-type))
      term-spec)))

(defun terminal-here--maybe-lookup-in-command-table (term-spec)
  (if (not (terminal-here--non-function-symbol-p term-spec))
      term-spec
    (let ((terminal-command (alist-get term-spec terminal-here-terminal-command-table)))
      (unless terminal-command
        (user-error "No settings found for terminal %s in `terminal-here-terminal-command-table'" term-spec))
      terminal-command)))

(defun terminal-here--maybe-funcall (dir x)
  (if (not (functionp x))
      x
    (funcall x dir)))

(defun terminal-here--get-terminal-command (dir)
  (thread-last terminal-here-terminal-command
    (terminal-here--maybe-lookup-os-terminal-command)
    (terminal-here--maybe-lookup-in-command-table)
    (terminal-here--maybe-funcall dir)))

(defun terminal-here--get-command-flag ()
  (or
   terminal-here-command-flag
   (let ((term-spec (terminal-here--maybe-lookup-os-terminal-command terminal-here-terminal-command)))
     (when (terminal-here--non-function-symbol-p term-spec)
       (let ((flag (alist-get term-spec terminal-here-command-flag-table)))
         (unless flag
           (user-error "No flag settings found for terminal %s in `terminal-here-command-flag-table'" term-spec))
         flag)))
   (user-error "Couldn't work out how to run an ssh command in your terminal, customize `terminal-here-command-flag' or set `terminal-here-terminal-command' to specify your terminal by symbol")))

(defun terminal-here--term-command (dir)
  (let ((ssh-data (terminal-here--parse-ssh-dir dir)))
    (if ssh-data
        (terminal-here--ssh-command (car ssh-data) (cadr ssh-data))
      (terminal-here--get-terminal-command dir))))



;;; Individual terminals

(defun terminal-here--find-and-run-st (_)
  "Suckless term can either be called st or stterm depending on how it was installed."
  (if (executable-find "stterm") '("stterm") '("st")))



;;; Tramp/ssh support

(defun terminal-here--parse-ssh-dir (dir)
  (when (string-prefix-p "/ssh:" dir)
    (with-parsed-tramp-file-name dir nil
      (list (if user (concat user "@" host) host) localname))))

(defun terminal-here--ssh-command (remote dir)
  (append (terminal-here--term-command "") (list (terminal-here--get-command-flag) "ssh" "-t" remote
                                    "cd" (shell-quote-argument dir) "&&" "exec" "$SHELL" "-")))

(defun terminal-here-maybe-tramp-path-to-directory (dir)
  "Extract the local part of a local tramp path.

Given a tramp path returns the local part, otherwise returns nil."
  (when (tramp-tramp-file-p dir)
    (let ((file-name-struct (tramp-dissect-file-name dir)))
      (cond
       ;; sudo: just strip the extra tramp stuff
       ((equal (tramp-file-name-method file-name-struct) "sudo")
        (tramp-file-name-localname file-name-struct))
       ;; ssh: run with a custom command handled later
       ((equal (tramp-file-name-method file-name-struct) "ssh") dir)
       (t (user-error "Terminal here cannot currently handle tramp files other than sudo and ssh"))))))




;;; Launching

(defun terminal-here-launch-in-directory (dir)
  "Launch a terminal in directory DIR.

Handles tramp paths sensibly."
  (terminal-here--run-command (terminal-here--term-command dir)
                 (or (terminal-here-maybe-tramp-path-to-directory dir) dir)))

(defun terminal-here--run-command (command dir)
  (let* ((default-directory dir)
         (process-name (car command))
         (proc (apply #'start-process process-name nil command)))
    (set-process-sentinel
     proc
     (lambda (proc _)
       (when (and (eq (process-status proc) 'exit) (/= (process-exit-status proc) 0))
         (message "Error: in terminal here, command `%s` exited with error code %d"
                  (mapconcat #'identity command " ")
                  (process-exit-status proc)))))
    ;; Don't close when emacs closes, seems to only be necessary on Windows.
    (set-process-query-on-exit-flag proc nil)))

;;;###autoload
(defun terminal-here-launch ()
  "Launch a terminal in the current working directory.

This is the directory of the current buffer unless you have
changed it by running `cd'."
  (interactive)
  (terminal-here-launch-in-directory default-directory))

;;;###autoload
(defalias 'terminal-here 'terminal-here-launch)

;;;###autoload
(defun terminal-here-project-launch ()
  "Launch a terminal in the current project root.

Uses `terminal-here-project-root-function' to determine the project root."
  (interactive)
  (let* ((real-project-root-function
          (or terminal-here-project-root-function
              (cl-find-if #'fboundp (list 'projectile-project-root 'vc-root-dir))
              (user-error "No `terminal-here-project-root-function' is set and no default could be picked.")))
         (root (funcall real-project-root-function)))
    (when (not root)
      (user-error "Not in any project according to `terminal-here-project-root-function'"))
    (terminal-here-launch-in-directory root)))



(provide 'terminal-here)

;;; terminal-here.el ends here
