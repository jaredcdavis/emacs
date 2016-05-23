emacs
=====

This is just my Emacs configuration for easy sharing among multiple computers.

I don't recommend that you use it.

Some features may require Emacs 24.??

## Setup

I clone this repository into `~/emacs`.

My actual `.emacs` file sets up just a few things that vary from
computer to computer, e.g., paths for ACL2 doc stuff, fonts, etc.

An example:

```
(defvar *acl2-sources-dir* "~/acl2/")
(load "~/acl2/books/xdoc/xdoc.el")

(load "~/emacs/top.el")

(ignore-errors (visit-tags-table "~/acl2/TAGS"))
(ignore-errors (visit-tags-table "~/acl2/books/TAGS"))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit (default default) :stipple nil :background "#000059" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 117 :width normal :foundry "unknown" :family "Inconsolata")))))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js2-bounce-indent-p t)
 '(js2-mode-escape-quotes nil)
 '(safe-local-variable-values (quote ((syntax . COMMON-LISP) (Package . "CCL") (Package X86 :use CL) (Package X8664 :use CL) (Package X8632 :use CL) (Package CCL :use CL) (Package PPC :use CL) (Package ARM :use CL) (Package CHUD (:USE CL CCL)) (auto-fill . t) (Package . CCL)))))
```
