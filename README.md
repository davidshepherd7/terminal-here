# Terminal Here

[![CI](https://travis-ci.org/davidshepherd7/terminal-here/workflows/CI/badge.svg)](https://github.com/davidshepherd7/terminal-here/actions)
[![melpa][melpa-badge]][melpa-link]
[![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

[melpa-link]: http://melpa.org/#/terminal-here
[melpa-badge]: http://melpa.org/packages/terminal-here-badge.svg
[melpa-stable-link]: https://stable.melpa.org/#/terminal-here
[melpa-stable-badge]: https://stable.melpa.org/packages/terminal-here-badge.svg


An Emacs package to open an *external* terminal emulator in directories associated with the current buffer.


## Usage

Run `terminal-here-launch` to start a terminal in the current directory.

If the default command doesn't launch your preferred terminal you can set 
`terminal-here-terminal-command` to either a list containing the command and
arguments (e.g. `(list "my-terminal" "--foo")`)
or a function which takes a directory and returns such a list.

Recommended keybindings:

```
(require 'terminal-here)
(global-set-key (kbd "C-<f5>") #'terminal-here-launch)
(global-set-key (kbd "C-<f6>") #'terminal-here-project-launch)
```


## Platforms

Terminal-here has out-of-the-box support for some platforms, but will work anywhere with some customisation.

Out-of-the-box support tested on:

* Ubuntu 16.04 (but should work identically on any Debian-based system)
* Windows 10
* OSX

Should be supported out-of-the-box, but currently untested:

* Older versions of Windows

Requires setting `terminal-here-terminal-command` before use:

* Non-Debian-based UNIXes, because I haven't seen a standard way to open the user's preferred terminal

If you have problems just set `terminal-here-terminal-command` as described above.


## Remote directories

`terminal-here` can run ssh to open terminals in remote directories for files
opened with [tramp](https://www.gnu.org/software/tramp/#Overview). This may
require additional setup because of inconsistencies between different terminals.

If your terminal has a flag to treat the rest of the command line as the command
to run inside the terminal, you just need to set `terminal-here-command-flag` to
this flag. If not it may be impossible to get ssh support. Some examples are:

* `xterm`, `urxvt`: `-e` (this is the default)
* `gnome-terminal`: `-x`


## Alternatives

There are lots of built in ways to run terminals *inside* emacs (`shell`,
`eshell`, `ansi-term`, ...) but these can have problems like slow output speed
or incompatibility with existing configs. I currently prefer to run external
terminal emulators, YMMV.

A couple of
[places on](http://emacs.stackexchange.com/questions/7650/how-to-open-a-external-terminal-from-emacs)
[the internet](http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html) have instructions for running specific terminals from Emacs, but they are not as portable as they could be.
