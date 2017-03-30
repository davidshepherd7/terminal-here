;;; -*- lexical-binding: t; -*-

(defmacro with-terminal-here-mocks (&rest body)
  "Stub out some process interaction functions"
  `(with-mock
     (stub set-process-sentinel)
     (stub set-process-query-on-exit-flag)
     ,@body))



(ert-deftest linux-default-command ()
  (with-terminal-here-mocks
   (mock (start-process "x-terminal-emulator" * "x-terminal-emulator"))
   (let ((system-type 'gnu/linux))
     (custom-reevaluate-setting 'terminal-here-terminal-command)
     (terminal-here-launch-in-directory "adir"))))

(ert-deftest osx-default-command ()
  (with-terminal-here-mocks
   (mock (start-process "open" * "open" "-a" "Terminal.app" "adir"))
   (let ((system-type 'darwin))
     (custom-reevaluate-setting 'terminal-here-terminal-command)
     (terminal-here-launch-in-directory "adir"))))

(ert-deftest windows-default-command ()
  (with-terminal-here-mocks
   (mock (start-process "cmd.exe" *  "cmd.exe" "/C" "start" "cmd.exe"))
   (let ((system-type 'windows-nt))
     (custom-reevaluate-setting 'terminal-here-terminal-command)
     (terminal-here-launch-in-directory "adir"))))



(ert-deftest custom-terminal-command-as-list ()
  (with-terminal-here-mocks
   (mock (start-process "1" * "1" "2" "3"))
   (validate-setq terminal-here-terminal-command '("1" "2" "3"))
   (terminal-here-launch-in-directory "adir")))

(ert-deftest custom-terminal-command-as-function ()
  (with-terminal-here-mocks
   (mock (start-process "1" * "1" "2" "3" "adir"))
   (validate-setq terminal-here-terminal-command (lambda (dir) (list "1" "2" "3" dir)))
   (terminal-here-launch-in-directory "adir")))

(ert-deftest custom-terminal-command-as-junk-rejected ()
  (with-terminal-here-mocks
   (should-error
    (validate-setq terminal-here-terminal-command "astring")
    :type 'user-error)))



(ert-deftest no-project-root-function ()
  ;; Can't use validate setq here because you aren't allowed to set this to nil
  ;; by hand.
  (setq terminal-here-project-root-function nil)
  (should-error (terminal-here-project-launch) :type 'user-error))

(ert-deftest with-project-root-function ()
  (let ((project-root-finder (lambda () "" "vc-root")))
    (validate-setq terminal-here-project-root-function project-root-finder)
    (with-terminal-here-mocks
     (mock (terminal-here-launch-in-directory "vc-root"))
     (terminal-here-project-launch))))

(ert-deftest project-root-finds-nothing ()
  (validate-setq terminal-here-project-root-function (lambda () nil))
  (should-error (terminal-here-project-launch :type 'user-error)))
