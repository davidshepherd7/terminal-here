;;; -*- lexical-binding: t; -*-

(defmacro with-terminal-here-mocks (&rest body)
  "Stub out some process interaction functions"
  `(with-mock
     (stub set-process-sentinel)
     (stub set-process-query-on-exit-flag)
     ,@body))



(ert-deftest terminal-here-launch-alias ()
  (with-terminal-here-mocks
   (mock (terminal-here-launch-in-directory *))
   (terminal-here)))

(ert-deftest linux-default-command ()
  (with-terminal-here-mocks
   (mock (start-process "x-terminal-emulator" * "x-terminal-emulator"))
   (let ((system-type 'gnu/linux))
     (custom-reevaluate-setting 'terminal-here-terminal-command)
     (terminal-here-launch-in-directory "adir"))))

(ert-deftest osx-default-command ()
  (with-terminal-here-mocks
   (mock (start-process "open" * "open" "-a" "Terminal.app" "."))
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
  (validate-setq terminal-here-project-root-function nil)
  (cl-letf (((symbol-function #'projectile-project-root) nil)
            ((symbol-function #'vc-root-dir) nil))
    (should-error (terminal-here-project-launch) :type 'user-error)))

(ert-deftest default-project-root-function ()
  (validate-setq terminal-here-project-root-function nil)
  (cl-letf (((symbol-function #'projectile-project-root) (lambda () "" "projectile-root"))
            ((symbol-function #'vc-root-dir) nil))
    (with-terminal-here-mocks
     (mock (terminal-here-launch-in-directory "projectile-root"))
     (terminal-here-project-launch))))

(ert-deftest with-project-root-function ()
  (let ((project-root-finder (lambda () "" "vc-root")))
    (validate-setq terminal-here-project-root-function project-root-finder)
    (with-terminal-here-mocks
     (mock (terminal-here--run-command * "vc-root"))
     (terminal-here-project-launch))))

(ert-deftest project-root-finds-nothing ()
  (validate-setq terminal-here-project-root-function (lambda () nil))
  (should-error (terminal-here-project-launch :type 'user-error)))



(ert-deftest sudo-tramp ()
  (with-terminal-here-mocks
   (mock (terminal-here--run-command * "/etc/emacs/"))
   (terminal-here-launch-in-directory "/sudo:root@localhost:/etc/emacs/"))

  (with-terminal-here-mocks
   (mock (terminal-here--run-command * "/etc/emacs/"))
   (terminal-here-launch-in-directory "/sudo:postgres@localhost:/etc/emacs/"))

  (with-terminal-here-mocks
   (mock (terminal-here--run-command *  "/etc/emacs/"))
   (terminal-here-launch-in-directory "/sudo:root@127.0.0.1:/etc/emacs/")))



(ert-deftest parse-ssh-dir ()
  (should (equal (terminal-here--parse-ssh-dir "/ssh:buildbot:/home/buildbot/") (list "buildbot" "/home/buildbot/")))
  (should (equal (terminal-here--parse-ssh-dir "/ssh:david@pi:/home/pi/") (list "david@pi" "/home/pi/")))
  (should (equal (terminal-here--parse-ssh-dir "/ssh:root@192.168.0.1:/etc/hosts") (list "root@192.168.0.1" "/etc/hosts")))
  (should (equal (terminal-here--parse-ssh-dir "/ssh:myhost:/home/me/colon:dir") (list "myhost" "/home/me/colon:dir")))

  (should-not (terminal-here--parse-ssh-dir "/home/buildbot/"))
  (should-not (terminal-here--parse-ssh-dir "/ssh/foo/bar")))

(ert-deftest ssh-tramp ()
  (cl-letf* ((launch-command nil)
             ((symbol-function 'terminal-here--run-command)
              (lambda (command _dir)
                (setq launch-command command))))
    (validate-setq terminal-here-command-flag "-k")
    (terminal-here-launch-in-directory "/ssh:david@pi:/home/pi/")
    (should (equal (car launch-command) "x-terminal-emulator"))
    (should (equal (cadr launch-command) "-k"))
    (should (equal (caddr launch-command) "ssh"))))
