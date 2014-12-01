#!/bin/bash
#
set -ex
mkdir ~/emacs

# NCURSES
cd ~/emacs
wget http://ftp.gnu.org/gnu/ncurses/ncurses-5.9.tar.gz
tar xf ncurses-5.9.tar.gz

cd ~/emacs/ncurses*
./configure --prefix $HOME/.emacs.d/lroot --exec-prefix=$HOME/.emacs.d/lroot
make -j 4
make install

# EMACS
cd ~/emacs
wget http://mirror.anl.gov/pub/gnu/emacs/emacs-24.4.tar.gz
tar xf emacs-24.4.tar.gz

cd ~/emacs/emacs*
./configure --prefix=$HOME/.emacs.d/lroot --exec-prefix=$HOME/.emacs.d/lroot --x-includes=$HOME/.emacs.d/lroot/include \
 --x-libraries=$HOME/.emacs.d/lroot/lib --with-xpm=no --with-jpeg=no --with-gif=no --with-tiff=no --with-x-toolkit=no 
make -j 4
make install

# global-6.2.9

cd ~/emacs
wget http://ftp.gnu.org/gnu/global/global-6.2.12.tar.gz
tar xf global-6.2.12.tar.gz

cd ~/emacs/global*
./configure --prefix=$HOME/.emacs.d/lroot --exec-prefix=$HOME/.emacs.d/lroot --with-ncurses=$HOME/.emacs.d/lroot

ln -s ~/.emacs.d/lroot/include/ncurses/ncurses.h ~/.emacs.d/lroot/include/ncurses.h

make -j 4
make install

export PATH=$PATH:$HOME/.emacs.d/lroot/bin
emacs --version

exit
# In emacs execute M-x list-packages
# search for the following packages and put a letter "I" on each row.
# Then pres "x" for execute to install these packages:

  anaphora           20140728.... installed             anaphoric macros providing implicit temp variables
  async              20141001.151 installed             Asynchronous processing in Emacs
  auto-complete      20141111.... installed             Auto Completion for GNU Emacs
  color-theme        20080305.34  installed             install color themes
  dash               20141106.455 installed             A modern list library for Emacs
  ecb                20140215.114 installed             a code browser for Emacs
  f                  20140828.716 installed             Modern API for working with files and directories
  git                20140128.241 installed             An Elisp API for programmatically using Git
  helm               20141112.946 installed             Helm is an Emacs incremental and narrowing framework
  helm-gtags         20141110.... installed             GNU GLOBAL helm interface
  popup              20141002.320 installed             Visual Popup User Interface
  s                  20140910.334 installed             The long lost Emacs string manipulation library.
  workgroups2        20141102.... installed             New workspaces for Emacs



exit
### Loaded by Glenn
  ac-etags	    20131127.311 installed Etags/ctags completion source for auto-complete
  ac-helm	    20131224.647 installed Helm interface for auto-complete
  ascope	    20130824.1158 installed Another cscope interface for emacs
  auto-complete	    20130724.1750 installed Auto Completion for GNU Emacs [github]
  cl-lib	    0.3		installed  Properly prefixed CL functions and macros
  cmake-mode	    20110824	installed  Major-mode for editing CMake sources
  cmake-project	    0.7		installed  Integrates CMake build process with Emacs
  color-theme	    20080305.34	installed  Install color themes
  color-theme-active 0.0.1	installed  Active theme inspired by jEdit theme of the same name
  color-theme-actress 0.2.2	installed  A dark color theme for GNU Emacs.
  color-theme-blackboard 0.0.2	installed  Blackboard Colour Theme for Emacs.
  color-theme-eclipse 0.0.2	installed  Eclipse color theme for GNU Emacs.
  color-theme-github 0.0.3	installed  Github color theme for GNU Emacs.
  color-theme-gruber-darker 1	installed  Gruber Darker color theme for Emacs by Jason Blevins
  color-theme-molokai 0.1	installed  Molokai color theme by Lloyd
  color-theme-tango 0.0.2	installed  Tango palette color theme for GNU Emacs.
  color-theme-twilight 0.1	installed  Twilight Colour Theme for Emacs.
  color-theme-wombat+ 0.0.2	installed  Wombat with improvements and many more faces
  concurrent	    20130914.536 installed Concurrent utility functions for emacs lisp
  confluence	    20130814.535 installed Emacs mode for interacting with confluence wikis
  cpputils-cmake    0.4.0	installed  Easy real time C++ syntax check and intellisense if you use CMake.
  ctable	    20131202.2114 installed Table component for Emacs Lisp
  deferred	    20130930.607 installed Simple asynchronous functions for emacs lisp
  direx		    20130930.1224 installed Simple Directory Explorer
  elscreen	    20120413.807 installed Emacs window session manager
  emacs-eclim	    20140125.258 installed An interface to the Eclipse IDE.
  epc		    20130803.2228 installed A RPC stack for the Emacs Lisp
  findr		    20130127.2032 installed Breadth-first file-finding facility for (X)Emacs [wiki]
  fringe-helper	    20130519.741 installed Helper functions for fringe bitmaps
  git-commit-mode   20140125.1553 installed Major mode for editing git commit messages
  git-rebase-mode   20140125.1553 installed Major mode for editing git rebase files
  gtags		    3.3		installed  Gtags facility for Emacs
  helm		    20140125.1101 installed Helm is an Emacs incremental and narrowing framework
  helm-c-yasnippet  20140117.2255 installed Helm source for yasnippet.el
  helm-git	    20120630.1403 installed Helm extension for Git.
  helm-git-grep	    20140128.151 installed Helm for git grep, an incremental git-grep(1)
  helm-gtags	    20140128.238 installed GNU GLOBAL helm interface
  helm-ls-git	    20140104.657 installed List git files.
  helm-package	    20140108.2223 installed Listing ELPA packages with helm interface
  inflections	    20121016.957 installed Convert english words between singular and plural [github]
  jedi		    20130714.1228 installed Python auto-completion for Emacs
  jedi-direx	    20130525.1039 installed Tree style source code viewer for Python buffer
  jira		    20131210.1822 installed Connect to JIRA issue tracking software
  jump		    20130702.833 installed Build functions which contextually jump between files [github]
  magit		    20140129.1145 installed Control Git from Emacs
  malabar-mode	    20140203.1403 installed A better Java mode for Emacs
  noflet	    20130901.922 installed Locally override functions
  package+	    20131010.1412 installed Extensions for the package library.
  persp-mode	    20140126.702 installed "perspectives" + save/load + shared among frames - bugs.
  popup		    20130708.2245 installed Visual Popup User Interface [github]
  s		    20131223.944 installed The long lost Emacs string manipulation library.
  sr-speedbar	    20131207.2049 installed Same frame speedbar
  ssh		    20120904.1342 installed Support for remote logins using ssh. [github]
  ssh-config-mode   20120513.2233 installed Mode for fontification of ~/.ssh/config [github]
  sublimity	    20131111.2311 installed Smooth-scrolling and minimap, like sublime editor
  tabbar	    20140208.905 installed No description available.
  wget		    1.94	installed  Emacs-wget is an interface program of GNU wget on Emacs.
  workgroups	    20110726.941 installed Workgroups for windows (for Emacs)
  workgroups2	    20131224.657 installed New workspaces for Emacs
  xml-rpc	    20130423.1356 installed An elisp implementation of clientside XML-RPC
  yasnippet	    20131224.143 installed Yet another snippet extension for Emacs.
