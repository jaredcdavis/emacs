#!/bin/sh

emacs -batch -f batch-byte-compile emacs-acl2.el
emacs -batch -f batch-byte-compile jared.el
emacs -batch -f batch-byte-compile top.el


