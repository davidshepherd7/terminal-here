;;; terminal-here.el --- Run an external terminal in current directory -*- lexical-binding: t; -*-

;; Copyright © 2017 David Shepherd

;; Author: David Shepherd <davidshepherd7@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
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

(defun terminal-here-find-executable (variants)
  (file-name-nondirectory
   (cl-some (lambda (executable)
              (executable-find executable))
            variants)))

(defun terminal-here-default-terminal-command (_dir)
  "Pick a good default command to use for DIR."
  (cond
   ((eq system-type 'darwin)
    (list "open" "-a" "Terminal.app" "."))

   ;; From http://stackoverflow.com/a/13509208/874671
   ((memq system-type '(windows-nt ms-dos cygwin))
    (list "cmd.exe" "/C" "start" "cmd.exe"))

   ;; Probably X11!
   (t '("x-terminal-emulator"))))


(defcustom terminal-here-terminal-command
  #'terminal-here-default-terminal-command
  "The command used to start a terminal.

Either a list of strings: (terminal-binary arg1 arg2 ...); or a
function taking a directory and returning such a list."
  :group 'terminal-here
  :type '(choice (repeat string)
                 (function)))

(defcustom terminal-here-project-root-function
  (cl-find-if 'fboundp '(projectile-project-root vc-root-dir))
  "Function called to find the current project root directory.

Good options include `projectile-project-root', which requires
you install the `projectile' package, or `vc-root-dir' which is
available in Emacs >= 25.1.

The function should return nil or signal an error if the current
buffer is not in a project."
  :group 'terminal-here
  :type 'function)

(defcustom terminal-here-command-flag
  "-e"
  "The flag to tell your terminal to treat the rest of the line as a command to run
Typically this is -e, gnome-terminal uses -x."
  :group 'terminal-here
  :type 'string)

(defcustom terminal-here-multiplexers
  '("screen" "tmux")
  "List of terminal emulators."
  :group 'terminal-here
  :type 'string)

(defcustom terminal-here-multiplexer
  (terminal-here-find-executable terminal-here-multiplexers)
  "The terminal multiplexer to run inside a terminal."
  :type 'string
  :options '("screen" "tmux")
  :group 'terminal-here)

(defcustom terminal-here-multiplexer-new-session t
  "If t launch a new `screen' session inside multiplexer.
`tmux' can only attach to existing session.

If f attach to a `screen' session inside multiplexer."
  :type 'boolean
  :group 'terminal-here)

(defcustom terminal-here-multiplexer-flags nil
  "Flags passed to multiplexer."
  :type 'list
  :group 'terminal-here)



(defun terminal-here-multiplexer-session (dir)
  "If new is t create a new multiplexer session called `dir'.

If new is f attach to a multiplexer session called `dir'."
  (let ((dir (file-name-nondirectory (directory-file-name dir))))
    (cond ((string-equal terminal-here-multiplexer "screen")
           (list (if terminal-here-multiplexer-new-session "-S" "-r") dir))
          ((string-equal terminal-here-multiplexer "tmux")
           (list "new-session" "-A" "-s" dir)))))

(defun terminal-here-multiplexer-command (dir)
  "Return a multiplexer command."
  (cons terminal-here-multiplexer (terminal-here-multiplexer-session dir)))

(defun terminal-here--parse-ssh-dir (dir)
  (when (string-prefix-p "/ssh:" dir)
    (cdr (split-string dir ":"))))

(defun terminal-here--ssh-command (remote dir &optional multiplexer)
  `(,@(terminal-here--term-command "")
    ,terminal-here-command-flag "ssh" "-t" ,remote "cd" ,dir "&&" "exec"
    ,@(if multiplexer (terminal-here-multiplexer-command dir) '("$SHELL" "-"))))

(defun terminal-here--term-command (dir &optional multiplexer)
  (let ((ssh-data (terminal-here--parse-ssh-dir dir)))
    (cond
     (ssh-data (terminal-here--ssh-command (car ssh-data) (cadr ssh-data)
                                           multiplexer))
     (t (let ((command (if (functionp terminal-here-terminal-command)
                           (funcall terminal-here-terminal-command dir)
                         terminal-here-terminal-command)))
          (if multiplexer
              `(,@command ,terminal-here-command-flag
                          ,@(terminal-here-multiplexer-command dir))
            command))))))

(defun terminal-here-launch-in-directory (dir &optional multiplexer)
  "Launch a terminal in directory DIR.

Handles tramp paths sensibly."
  (let ((term-command (terminal-here--term-command dir multiplexer)))
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

(defmacro terminal-here-not-null-symbol (symbol)
  `(when (null ,symbol)
     (user-error (format "No `%S' is set." ',symbol))))

;;;###autoload
(defun terminal-here-launch (&optional multiplexer)
  "Launch a terminal in the current working directory.

This is the directory of the current buffer unless you have
changed it by running `cd'."
  (interactive)
  (when multiplexer (terminal-here-not-null-symbol terminal-here-multiplexer))
  (terminal-here-launch-in-directory default-directory multiplexer))

;;;###autoload
(defalias 'terminal-here 'terminal-here-launch)

;;;###autoload
(defun terminal-here-project-launch (&optional multiplexer)
  "Launch a terminal in the current project root.

If projectile is installed the projectile root will be used,
  Otherwise `vc-root-dir' will be used."
  (interactive)
  (when (terminal-here-not-null-symbol terminal-here-project-root-function))
  (let ((root (funcall terminal-here-project-root-function)))
    (when (not root)
      (user-error "Not in any project according to `terminal-here-project-root-function'"))
    (terminal-here-launch-in-directory root multiplexer)))

;;;###autoload
(defun terminal-here-launch-multiplexer ()
  "Launch a terminal with `terminal-here-multiplexer' in the current
working directory.

This is the directory of the current buffer unless you have
changed it by running `cd'."
  (interactive)
  (terminal-here-launch t))

;;;###autoload
(defun terminal-here-project-launch-multiplexer ()
  "Launch a terminal with `terminal-here-multiplexer' in the current
project root.

If projectile is installed the projectile root will be used,
  Otherwise `vc-root-dir' will be used."
  (interactive)
  (terminal-here-project-launch t))



(provide 'terminal-here)

;;; terminal-here.el ends here
