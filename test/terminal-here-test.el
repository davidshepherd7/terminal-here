;;; -*- lexical-binding: t; -*-

(ert-deftest linux-default-command ()
  (with-mock
    (mock (start-process "x-terminal-emulator" * "x-terminal-emulator"))
    (let ((system-type 'gnu/linux))
      (custom-reevaluate-setting 'terminal-here-terminal-command)
      (terminal-here-launch-in-directory "adir"))))

(ert-deftest osx-default-command ()
  (with-mock
    (mock (start-process "open" * "open" "-a" "Terminal.app" "adir"))
    (let ((system-type 'darwin))
      (custom-reevaluate-setting 'terminal-here-terminal-command)
      (terminal-here-launch-in-directory "adir"))))

(ert-deftest windows-default-command ()
  (with-mock
    (mock (start-process "start" * "start" "/D" "adir" "cmd"))
    (let ((system-type 'windows-nt))
      (custom-reevaluate-setting 'terminal-here-terminal-command)
      (terminal-here-launch-in-directory "adir"))))



(ert-deftest custom-terminal-command-as-list ()
  (with-mock
    (mock (start-process "1" * "1" "2" "3"))
    (validate-setq terminal-here-terminal-command '("1" "2" "3"))
    (terminal-here-launch-in-directory "adir")))

(ert-deftest custom-terminal-command-as-function ()
  (with-mock
    (mock (start-process "1" * "1" "2" "3" "adir"))
    (validate-setq terminal-here-terminal-command (lambda (dir) (list "1" "2" "3" dir)))
    (terminal-here-launch-in-directory "adir")))

(ert-deftest custom-terminal-command-as-junk-rejected ()
  (with-mock
    (should-error
     (validate-setq terminal-here-terminal-command "astring")
     :type 'user-error)))



(ert-deftest projectile-root ()
  (with-mock
    (mock (projectile-project-root) => "project-root")
    (mock (terminal-here-launch-in-directory "project-root"))
    (terminal-here-project-launch)))

(ert-deftest non-projectile-project-root ()
  (with-mock
    (should (not (boundp 'projectile-project-root)))
    (mock (vc-root-dir) => "vc-root")
    (mock (terminal-here-launch-in-directory "vc-root"))
    (terminal-here-project-launch)))
