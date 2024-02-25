# Terminal Here

[![CI](https://github.com/davidshepherd7/terminal-here/workflows/CI/badge.svg)](https://github.com/davidshepherd7/terminal-here/actions)
[![melpa][melpa-badge]][melpa-link]
[![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

[melpa-link]: http://melpa.org/#/terminal-here
[melpa-badge]: http://melpa.org/packages/terminal-here-badge.svg
[melpa-stable-link]: https://stable.melpa.org/#/terminal-here
[melpa-stable-badge]: https://stable.melpa.org/packages/terminal-here-badge.svg


An Emacs package to open an *external* terminal emulator in directories associated with the current buffer.


## Usage

### Quickstart

Run `terminal-here-launch` to start a terminal in the current directory.

Recommended keybindings:

```
(require 'terminal-here)
(global-set-key (kbd "C-<f5>") #'terminal-here-launch)
(global-set-key (kbd "C-<f6>") #'terminal-here-project-launch)
```

### Configure the terminal

If the default command doesn't launch your preferred terminal you can configure
it using these three variables:

* `terminal-here-linux-terminal-command`
* `terminal-here-mac-terminal-command`
* `terminal-here-windows-terminal-command`

The documentation for these variables gives a list of terminals with builtin
support. If your terminal is in the list simply set the variable for your OS to
the terminal of your choice. e.g.

```
(setq terminal-here-linux-terminal-command 'urxvt)
(setq terminal-here-mac-terminal-command 'iterm2)
```

To use a terminal without builtin support, or to configure how your terminal is
run, you can also set the variable to a list of strings specifying the command
line which runs the terminal, e.g.

```
(setq terminal-here-linux-terminal-command '("urxvt" "-fade" "50"))
```

To support more even more complex terminals or selecting a terminal at runtime:
you can set any of the `*-terminal-command` variables to a function which
accepts a launch directory argument and returns a list of strings specifying the
command line which runs the terminal.

To support other operating systems the variable `terminal-here-terminal-command`
can be used instead of the OS-specific variables listed above. If set it will
override other settings.


### Remote directories

`terminal-here` can run ssh to open terminals in remote directories for files
opened with [tramp](https://www.gnu.org/software/tramp/#Overview). For any of
the predefined terminal configurations this should work without any configuration.

For other terminals you will need to set `terminal-here-command-flag` to the
command line flag which tells the terminal to treat the rest of the command line
as a separate command to run inside the terminal.


### Inner commands

You can optionally provide a command to run inside the launched terminal as the
argument to `terminal-here-launch` and `terminal-here-project-launch`.

Configuring these commands is pretty error prone and hard to debug, so below
are some working examples to get you started. You might also find it useful to
do `(setq terminal-here-verbose t)` to get some debugging information in the
messages buffer.


#### Launch an interactive command and quit the terminal when it's done

`(terminal-here-launch (list "htop"))`


#### Launch an interactive command that acts on the current file

`(terminal-here-launch (list "less" (buffer-file-name)))`


#### Launch a non-interactive command and keep the terminal open when it's done

This runs ls and then drops you into your normal shell:

`(terminal-here-launch (list (getenv "SHELL") "-c" "ls && exec $SHELL"))`

Explanation: we start a non-interactive shell to run the command that we specify
with `-c`. Inside this inner command we first run `ls`, then replace the
non-interactive shell with a new interactive one.

Why do we need the non-interactive shell? Because we need the `&&` syntax to run
one command followed by another command, and that syntax requires a shell.

Why can't we just launch an interactive shell that runs a command first? Because
unfortunately the common shells don't support that feature, I don't know why
since it would be pretty handy!


#### Launch a shell with a python venv (HACK)

`(terminal-here-launch (list (getenv "SHELL") "-c" "source .venv/bin/activate && exec $SHELL"))`

Explanation: this is basically the same as the above except that the first
command is sourcing a venv.

Strictly speaking this is not how venvs are intended to be used: really we
should source the venv from inside the interactive shell. But this mostly works
because the important part of a venv is setting some environment variables and
the environment variables are inherited by the interactive shell.

Not everything works though, in particular the prompt is not modified to include
the venv name. Other things may also break so if something else about your venv
seems weird then try sourcing it manually instead.


## Platforms

Terminal-here has out-of-the-box support for most platforms, but will work
anywhere with some customisation.

Out-of-the-box support tested on:

* Ubuntu (but should work identically on any Debian-based system)
* Windows 10
* OSX

Should be supported out-of-the-box, but currently untested:

* Older versions of Windows
* Non-Debian-based Linux systems with a desktop environment (e.g. GNOME, KDE)

Requires configuration before use:

* Non-Debian-based UNIXes without a desktop environment (because there is no way
  to pick a good default terminal to use).

If you have problems just configure your terminal manually as described above.

## Changelog

### Unstable

* Add support for running commands inside launched terminals.
* Add the custom variable `terminal-here-verbose` to provide more debugging information.
* Fix xfce terminal using the wrong name.
* Add support for some additional terminals on Linux.
* Add support for gnome-console (aka kgx)


### 2.0

* Added support for simpler customisation by setting the terminal to a symbol
  from a list of supported terminal symbols.
* Allow simple configuration of different terminals on different operating systems.
* Improved selection of a default terminal for non-Debian Linux OSes.
* Out-of-the-box support for SSH on many more terminals.


## Alternatives

There are lots of built in ways to run terminals *inside* emacs (`shell`,
`eshell`, `ansi-term`, ...) but these can have problems like slow output speed
or incompatibility with existing configs. I currently prefer to run external
terminal emulators, YMMV.

A couple of
[places on](http://emacs.stackexchange.com/questions/7650/how-to-open-a-external-terminal-from-emacs)
[the internet](http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html) have instructions for running specific terminals from Emacs, but they are not as portable as they could be.
