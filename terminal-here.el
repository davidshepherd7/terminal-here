;;; terminal-here.el --- Run an external terminal in current directory -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: David Shepherd <davidshepherd7@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;; Keywords: tools, frames
;; URL: https://github.com/davidshepherd7/terminal-here


;;; Commentary:

;; Provides commands to help open external terminal emulators in the
;; directory of the current buffer.


;;; Code:

(require 'cl-lib)


(defgroup terminal-here nil
  "Open external terminal emulators in the current buffer's directory."
  :group 'external
  :prefix "terminal-here-")

(defun terminal-here-default-terminal-command (dir)
  "Pick a good default command to use for DIR."
  (cond
   ((eq system-type 'darwin)
    (list "open" "-a" "Terminal.app" dir))

   ((memq system-type '(windows-nt ms-dos cygwin))
    (list "start" "/D" dir "cmd"))

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
you install the `projectile' package, or `vc-root-dir', which is
available in Emacs >= 25.1."
  :group 'terminal-here
  :type 'function)



(defun terminal-here-launch-in-directory (dir)
  "Launch a terminal in directory DIR."
  (let* ((term-command (if (functionp terminal-here-terminal-command)
                           (funcall terminal-here-terminal-command dir)
                         terminal-here-terminal-command))
         (process-name (car term-command))
         (default-directory dir))
    (apply #'start-process process-name nil term-command)))

;;;###autoload
(defun terminal-here-launch ()
  "Launch a terminal in the current working directory.

This is the directory of the current buffer unless you have
changed it by running `cd'."
  (interactive)
  (terminal-here-launch-in-directory default-directory))

;;;###autoload
(defun terminal-here-project-launch ()
  "Launch a terminal in the current project root.

If projectile is installed the projectile root will be used,
  Otherwise `vc-root-dir' will be used."
  (interactive)
  (terminal-here-launch-in-directory
   (if terminal-here-project-root-function
       (funcall terminal-here-project-root-function)
     (signal 'user-error "No `terminal-here-project-root-function' is set."))))



(provide 'terminal-here)

;;; terminal-here.el ends here
