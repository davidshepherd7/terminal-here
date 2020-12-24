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
  #'terminal-here-default-terminal-command
  "Specification of the command used to start a terminal.

Either:

* A symbol from `terminal-here-terminal-command-table' indicating the name of the terminal to use
* A list of strings like '(terminal-binary arg1 arg2 ...)
* A function taking a directory and returning such a list."
  :group 'terminal-here
  :type '(choice (symbol)
                 (repeat string)
                 (function)))

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

(defcustom terminal-here-command-flag
  "-e"
  "The flag to tell your terminal to treat the rest of the line as a command to run
Typically this is -e, gnome-terminal uses -x."
  :group 'terminal-here
  :type 'string)

(defcustom terminal-here-terminal-command-table
  (list
   (cons 'terminal-app        (list "open" "-a" "Terminal.app" "."))
   (cons 'iterm-app           (list "open" "-a" "iTerm.app" "."))
   (cons 'x-terminal-emulator (list "x-terminal-emulator"))
   ;; From http://stackoverflow.com/a/13509208/874671
   (cons 'cmd                 (list "cmd.exe" "/C" "start" "cmd.exe")))
  "A table of terminal commands.

The keys should be symbols, the values should be either a list of
strings: (terminal-binary arg1 arg2 ...); or a function taking a
directory and returning such a list."
  :group 'terminal-here
  :type '(repeat (cons symbol
                       (choice (repeat string)
                               (function))))
  )




(defun terminal-here--get-terminal-command ()
  (if (and (symbolp terminal-here-terminal-command)
           (not (functionp terminal-here-terminal-command)))
      (let ((terminal-command (alist-get terminal-here-terminal-command terminal-here-terminal-command-table)))
        (unless terminal-command
          (user-error "No settings found for terminal %s in `terminal-here-terminal-command-table'" terminal-here-terminal-command))
        terminal-command)
    terminal-here-terminal-command))

(defun terminal-here-default-terminal-command (_dir)
  "Pick a good default command to use for DIR."
  (cond
   ((eq system-type 'darwin)
    (alist-get 'terminal-app terminal-here-terminal-command-table))

   ((memq system-type '(windows-nt ms-dos cygwin))
    (alist-get 'cmd terminal-here-terminal-command-table))

   ;; Probably X11!
   ((executable-find "x-terminal-emulator") (alist-get 'x-terminal-emulator terminal-here-terminal-command-table))

   (t (user-error  "No default terminal detected, please set `terminal-here-terminal-command'"))))

(defun terminal-here--parse-ssh-dir (dir)
  (when (string-prefix-p "/ssh:" dir)
    (with-parsed-tramp-file-name dir nil
      (list (if user (concat user "@" host) host) localname))))

(defun terminal-here--ssh-command (remote dir)
  (append (terminal-here--term-command "") (list terminal-here-command-flag "ssh" "-t" remote "cd" (shell-quote-argument dir) "&&" "exec" "$SHELL" "-")))

(defun terminal-here--term-command (dir)
  (let ((ssh-data (terminal-here--parse-ssh-dir dir))
        (term-command (terminal-here--get-terminal-command)))
    (cond
     (ssh-data (terminal-here--ssh-command (car ssh-data) (cadr ssh-data)))
     ((functionp term-command) (funcall term-command dir))
     (t term-command))))

(defun terminal-here-launch-in-directory (dir)
  "Launch a terminal in directory DIR.

Handles tramp paths sensibly."
  (let ((term-command (terminal-here--term-command dir)))
    (terminal-here--run-command term-command
                   (or (terminal-here-maybe-tramp-path-to-directory dir) dir))))

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
