This emacs configuration file is self-boostrapping. The recommended
way to use it is to move your old ~/.emacs and ~/.emacs.d settings out
of the way, clone the repo from github and start up emacs. The first
time you start up you'll have to wait a minute for all the packages to
download from elpa and marmalade, but the next time you start emacs
everything will be set up and the startup should be speedy!

    cd ~
    mv ~/.emacs ~/.emacs.old
    mv ~/.emacs.d ~/.emacs.d.old
    git clone https://github.com/aculich/.emacs.d.git
    echo "...wait a minute for packages to download..." && emacs

**NOTE:** Emacs 24 is _required_!!!
