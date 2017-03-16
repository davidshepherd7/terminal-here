;;; terminal-here.el --- Run an external terminal in current directory -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: David Shepherd <davidshepherd7@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24"))
;; Keywords: tools, frames
;; URL: https://github.com/davidshepherd7/terminal-here


;;; Commentary:

;; An Emacs package to help open external terminal emulators in the directory of
;; the current buffer. See the readme.md file for more details.


;;; Code:



(defun terminal-here--default-terminal-command ()
  "Pick a good default command to use."
  (cond
   ((eq system-type 'darwin)
    (lambda (dir) (list "open" "-a" "Terminal.app" dir)))

   ((or (eq system-type 'windows-nt) (eq system-type 'ms-dos) (eq system-type 'cygwin))
    (lambda (dir) (list "start" "/D" dir "cmd")))

   ;; Probably X11!
   (t '("x-terminal-emulator"))))


(defcustom terminal-here-terminal-command
  (terminal-here--default-terminal-command)
  "The command used to start a terminal.

Either a list of strings: (terminal-binary arg1 arg2 ...); or a
function taking a directory and returning such a list."
  :group 'terminal-here
  :type '(choice (repeat string)
                 (function)))




(defun terminal-here-launch-in-directory (dir)
  "Launch a terminal in directory DIR."
  (let* ((term-command (if (functionp terminal-here-terminal-command)
                           (funcall terminal-here-terminal-command dir)
                         terminal-here-terminal-command))
         (process-name (car term-command))
         (default-directory dir))
    (apply #'start-process process-name nil term-command)))

(defun terminal-here-launch ()
  "Launch a terminal in the current working directory.

This is the directory of the current buffer unless you have
changed it by running `cd'."
  (interactive)
  (terminal-here-launch-in-directory default-directory))


(defun terminal-here-project-launch ()
  (interactive)
  "Launch a terminal in the current project root.

If projectile is installed the projectile root will be used,
  Otherwise `vc-root-dir' will be used."
  (terminal-here-launch-in-directory (if (functionp 'projectile-project-root)
                            (projectile-project-root)
                          (vc-root-dir))))



(provide 'terminal-here)

;;; terminal-here.el ends here
