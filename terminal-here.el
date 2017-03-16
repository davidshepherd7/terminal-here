;;; terminal-here.el --- Run an external terminal in current directory -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: David Shepherd <davidshepherd7@gmail.com>
;; Version: 0.1
;; Package-Requires: (TODO)
;; Keywords: TODO
;; URL: https://github.com/davidshepherd7/terminal-here


;;; Commentary:

;; TODO


;;; Code:


(defcustom terminal-here-terminal-command
  "x-terminal-emulator"
  "Terminal binary, or list of (binary arg1 arg2 ...)"
  :group 'terminal-here
  :type '(list string))


(defun terminal-here-cwd ()
  "Launch a terminal in the working directory of the current buffer"
  (interactive)
  (let ((terminal-command (if (listp terminal-here-terminal-command) (car terminal-here-terminal-command) terminal-here-terminal-command))
        (terminal-arguments (if (listp terminal-here-terminal-command) (cdr terminal-here-terminal-command) '())))
    (apply #'start-process
           terminal-command nil terminal-command
           terminal-arguments)))


(provide 'terminal-here)


;;; terminal-here.el ends here
