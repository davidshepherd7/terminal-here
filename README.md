# Terminal Here

An Emacs package to help open external terminal emulators in the directory of the current buffer.

## Usage

Currently not on Melpa, so grab the elisp file, put it in your path and require
it. Hopefully it will be soon.

<!-- `M-x package-install terminal-here` -->

Run TODO to start a terminal in the current directory.

Recommended keybindings:

TODO


## Alternatives

There are lots of built in ways to run terminals *inside* emacs (`shell`,
`eshell`, `ansi-term`, ...) but these can have problems like slow output speed
or incompatibility with existing configs. I currently prefer to run external
terminal emulators, YMMV.

A couple of
[places on](http://emacs.stackexchange.com/questions/7650/how-to-open-a-external-terminal-from-emacs)
[the internet](http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html) have instructions for running specific terminals from Emacs, but they are not as portable as they could be.


## Ramblings

This package is actually pretty trivial, but I figure it's useful for people who
are (very) new to emacs to be able to do this by simply installing a package. I
found myself wanting to do something similar in atom but I didn't have time to
figure out how to write the (probably trivial) required coffeescript. So I was
very happy to find that someone had made a package for it.

Also integration with projectile or other project packages might be of some use.
