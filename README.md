This emacs configuration file is self-boostrapping.

Latest version always available at: https://github.com/aculich/.emacs.d

**NOTE:** Emacs 24 is _required_!!!

Background
----------
There are a lot of great features enabled by other people which this
config draws from.

The starter-kit package does a really good job getting things going
and could use a few tweaks.

The sublime modifications are useful because it shows sublime users
that they can get all the default features they've come to love in the
Sublime text editor, but they can still have all the power of Emacs,
too. Except... sublime makes some very, uh, maddening choices which
need to be fixed lest us old bearded Emacs dudes will go insane.

See this commit for more info:
https://github.com/aculich/sublime-sanity.el/commit/bdb1d4e67bbe5e38fbc3a0aeaf65c9c96be97735

Installation
------------
The recommended way to use it is to move your old ~/.emacs and
~/.emacs.d settings out of the way, clone the repo from github and
start up emacs. The first time you start up you'll have to wait a
minute for all the packages to download from elpa and marmalade, but
the next time you start emacs everything will be set up and the
startup should be speedy!

    cd ~
    mv ~/.emacs ~/.emacs.old
    mv ~/.emacs.d ~/.emacs.d.old
    git clone https://github.com/aculich/.emacs.d.git
    echo "...wait a minute for packages to download..." && emacs

Share & Enjoy!
