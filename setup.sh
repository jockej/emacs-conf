#!/bin/sh

# check for realpath(1) which doesn't exist on OpenBSD.
# We need to use realpath(1) on FreeBSD since the home directory is actually
# /usr/home/$USERNAME, which seems to matter very much to stow(1)
which realpath > /dev/null

if [ $? -eq 0 ]; then
  echo "Found realpath(1)"
  HOMEDIR=`realpath $HOME`
else
  echo "No realpath(1), using $HOME directly"
  HOMEDIR=$HOME
fi

echo "HOME is $HOMEDIR"

if [ ! -d "$HOMEDIR/emacs-conf" ]; then
  echo "$HOMEDIR/emacs-conf doesn't exist"
  exit 1
fi

if [ -L $HOMEDIR/.emacsconf ]; then
  echo "~/.emacsconf already exists"
  exit 1
else
  echo "Creating ~/.emacsconf"
  ln -s "$HOMEDIR/emacs-conf" "$HOMEDIR/.emacsconf"
fi

if [ -d "$HOMEDIR/.emacs.d" ]; then
  echo "~/.emacs.d exists"
else
  echo "Creating ~/.emacs.d"
  mkdir "$HOMEDIR/.emacs.d"
fi

if [ -e "$HOMEDIR/.emacs.d/init.el" ]; then
  echo "$HOMEDIR/.emacs.d/init.el exists"
  exit 1
else
  echo "Creating symlink $HOMEDIR/.emacs.d/init.el"
  ln -s "$HOMEDIR/emacs-conf/loader.el" "$HOMEDIR/.emacs.d/init.el"
fi
