#! /bin/bash

# Setup of emacs
# James Brooks
# 8-May-2009

# emacs
sudo apt-get update
sudo apt-get upgrade
sudo apt-get install emacs-snapshot

# core emacs init files
sudo apt-get install git-core
git clone git://github.com/kerspoon/emacs-starter-kit.git ~/.emacs.d

# a nice font for emacs
sudo apt-get install ttf-inconsolata

# pymacs
sudo apt-get install python-setuptools
mkdir -p ~/.emacs.d/elpa-to-submit && cd ~/.emacs.d/elpa-to-submit
wget http://pymacs.progiciels-bpi.ca/archives/Pymacs-0.23.tar.gz
tar xfv Pymacs-0.23.tar.gz
cd Pymacs-0.23
make
sudo easy_install .

# rope, ropemacs
sudo apt-get install mercurial
mkdir /tmp/rope && cd /tmp/rope
hg clone http://bitbucket.org/agr/rope
hg clone http://bitbucket.org/agr/ropemacs
hg clone http://bitbucket.org/agr/ropemode
sudo easy_install rope
ln -s ../ropemode/ropemode ropemacs/
sudo easy_install ropemacs

# auto-complete
mkdir -p ~/.emacs.d/elpa-to-submit && cd ~/.emacs.d/elpa-to-submit
wget http://www.cx4a.org/pub/auto-complete.el
wget http://www.cx4a.org/pub/auto-complete-cpp.el
wget http://www.cx4a.org/pub/auto-complete-python.el
# wget http://www.cx4a.org/pub/auto-complete-yasnippet.el

