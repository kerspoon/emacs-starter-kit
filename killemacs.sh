#!/bin/sh

set -x

# http://emacs-fu.blogspot.com/2009/02/emacs-daemon.html 
pkill -TERM -u $USER emacs

echo "done"
exit 0

