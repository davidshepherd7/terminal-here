;; Don't load old byte-compiled versions!
(setq load-prefer-newer t)

;; Load terminal-here
(require 'f)
(add-to-list 'load-path (f-parent (f-dirname load-file-name)))
(require 'terminal-here)

;; Load test helpers
(require 'el-mock)
(eval-when-compile
  (require 'cl)) ;; for el-mock
(require 'validate)
