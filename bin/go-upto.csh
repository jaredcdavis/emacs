#!/bin/sh

# NOTE: there is an alternative for sh/bash/zsh, see go-upto (with no .csh suffix)
#
# Usage:
#  Add the folloiwng to .cshrc:
#
#    alias upto="source /path/to/go-upto.csh"
#
#  Then you can do:
#
#    $ cd foo
#    $ cd bar/baz/a/b/c
#    $ upto baz            <-- go back up to foo/bar/baz
#    $ cd a/b/c
#    $ upto bar            <-- go back up to foo/bar
#
# And so forth.  The associated emacs stuff builds this into the Emacs
# directory-tracking mechanisms.

set GO_UPTO_GOAL=$1

set GO_UPTO_START=`pwd`

set GO_UPTO_HERE=`pwd`
while ( "`basename $GO_UPTO_HERE`" != "$GO_UPTO_GOAL"  &&  "$GO_UPTO_HERE" != "/" )
	cd ..
        set GO_UPTO_HERE=`pwd`
end

# Goofy trick to make "cd -" work.
cd $GO_UPTO_START
cd $GO_UPTO_HERE
