# Terminal Here

[![travis][travis-badge]][travis-link] <!-- [![melpa][melpa-badge]][melpa-link] --> <!-- [![melpa stable badge][melpa-stable-badge]][melpa-stable-link] -->

[travis-link]: https://travis-ci.org/davidshepherd7/terminal-here
[travis-badge]: https://travis-ci.org/davidshepherd7/terminal-here.svg?branch=master
[melpa-link]: http://melpa.org/#/terminal-here
[melpa-badge]: http://melpa.org/packages/terminal-here-badge.svg
[melpa-stable-link]: https://stable.melpa.org/#/terminal-here
[melpa-stable-badge]: https://stable.melpa.org/packages/terminal-here-badge.svg


An Emacs package to open an *external* terminal emulator in directories associated with the current buffer.

Currently OSX and Windows support is based on what [atom-open-terminal-here](https://github.com/blueimp/atom-open-terminal-here/blob/master/index.coffee#L46) does. I don't have access to emacs running on either operating system, so please let me know if it works or not.


## Usage

Currently not on Melpa, so grab the elisp file, put it in your path and require
it. Hopefully it will be soon.

<!-- `M-x package-install terminal-here` -->

Run `terminal-here-launch` to start a terminal in the current directory.

Recommended keybindings:

```
(require 'terminal-here)
(global-set-key (kbd "C-<f5>") #'terminal-here-launch)
(global-set-key (kbd "C-<f6>") #'terminal-here-project-launch)
```

but pick anything you like really.


## Alternatives

There are lots of built in ways to run terminals *inside* emacs (`shell`,
`eshell`, `ansi-term`, ...) but these can have problems like slow output speed
or incompatibility with existing configs. I currently prefer to run external
terminal emulators, YMMV.

A couple of
[places on](http://emacs.stackexchange.com/questions/7650/how-to-open-a-external-terminal-from-emacs)
[the internet](http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html) have instructions for running specific terminals from Emacs, but they are not as portable as they could be.
