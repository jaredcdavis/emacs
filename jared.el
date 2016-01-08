;; jared.el
;; Jared's custom emacs code.

;(load "~sswords/el/auctex.el" nil t t)
;(load "~sswords/el/preview-latex.el" nil t t)

;; I don't like the toolbar or the menubar, so I turn them off here.  You
;; might want them enabled.

(if (equal (substring (emacs-version) 0 3) "GNU")
    (tool-bar-mode 0))

(if (equal (substring (emacs-version) 0 3) "GNU")
    (menu-bar-mode 0))

; This is no longer in Emacs 24, I guess.
;(if (equal (substring (emacs-version) 0 3) "GNU")
;    (pc-selection-mode))

(if (equal (substring (emacs-version) 0 3) "GNU")
    (set-scroll-bar-mode nil))



;; Allow me to use C-x C-h to mark the whole buffer.  Normally this is bound to
;; C-x h, but my fingers are lazy and I was constantly typing the wrong command
;; until I added this.  Now you can use either.

(defun wipe-buffer ()
  (interactive)
  (mark-whole-buffer)
  (delete-active-region))

(global-set-key [(control x) (control h)] 'mark-whole-buffer)
(global-set-key [(control a)] 'wipe-buffer)


;; I hate when I'm trying to hit C-x k, but hit C-x C-k instead.  So, I rebind
;; C-x C-k to let me be sloppier.

(defun kill-this-buffer ()
  (interactive)
  (kill-buffer nil))

(global-set-key [(control x) (control k)] 'kill-this-buffer)

(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

(defun up-one () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(global-set-key [(shift mouse-4)] 'down-one)
(global-set-key [(shift mouse-5)] 'up-one)

(defun up-a-lot () (interactive) (scroll-up))
(defun down-a-lot () (interactive) (scroll-down))
(global-set-key [(control mouse-4)] 'down-a-lot)
(global-set-key [(control mouse-5)] 'up-a-lot)



;; Allow C-x-l to downcase a region

(put 'downcase-region 'disabled nil)


;; Allow C-x-u to upcase a region

(put 'upcase-region 'disabled nil)


;; Allow me to use [insert] to toggle overwrite mode.

(put 'overwrite-mode 'disabled nil)


;; Jared's new Control-tab code.
;;
;; I hate how "C-x o" works with more than two windows open.  It cycles through them
;; one after another, but usually I have one active window and a shell I'm talking to,
;; and I just want to bounce between those.
;;
;; I bind control-tab to just switch between the next and previous buffer.  I also
;; change the emacs-acl2's enter-theorem-other-window function, so that C-t e will
;; work properly with control tab.
;;
;; This is kind of clunky because it switches directions on you.  So, you need to
;; order the windows correctly, and you may need to pay attention to the way the
;; cursor is switching to get the initial setup right.

(defvar my-control-tab-direction 1)

(defun my-control-tab-switch ()
  (interactive)
  (other-window my-control-tab-direction)
  (setq my-control-tab-direction (- my-control-tab-direction)))

(define-key global-map [(control tab)] 'my-control-tab-switch)
(define-key global-map [(control o)] 'my-control-tab-switch)

(defun maybe-get-package-string-from-prev-line ()
  (interactive)
  (let ((place (point))
	(ret   ""))
    (previous-line)
    (beginning-of-line)
    (let ((here (point)))
      (when (equal (buffer-substring here (+ 2 here)) "#!")
	(setq ret (buffer-substring here place))))
    (goto-char place)
    ret))

;; I think this is my old version from the #!pkg stuff that matt fixed up later
;; (defun enter-theorem-other-window ()
;;   (interactive)
;;   (push-mark) ; I think I sometimes like to go back to the form.
;;   (let (pkg-str defun-str)
;;     (end-of-line)
;;     (beginning-of-defun)
;;     (setq pkg-str (maybe-get-package-string-from-prev-line))
;;     (let ((beg (point)))
;;       (forward-sexp)
;;       (setq end (point))
;;       (setq defun-str (buffer-substring beg (point))))
;;     (other-window 1)
;;     (switch-to-buffer *acl2-shell*)
;;     (end-of-buffer)
;;     (insert pkg-str)
;;     (insert defun-str)
;;     (other-window 0)
;;     (end-of-buffer))
;;   (setq my-control-tab-direction -1))


(defun enter-theorem-other-window ()
  (interactive)
  (push-mark) ; I think I sometimes like to go back to the form.
  (let ((str (acl2-current-form-string))
	(shell *acl2-shell*))
    (other-window 1)
    (switch-to-buffer shell)
    (end-of-buffer)
    (insert str)
    (end-of-buffer))
  (setq my-control-tab-direction -1))


;; Highlight def* like you highlight defuns

(if (equal (substring (emacs-version) 0 3) "GNU")
    (font-lock-add-keywords 'lisp-mode
			    '(("(\\(def[^ ]*\\)[ ]+\\([^ ]+\\)"
			       (1 font-lock-keyword-face)
			       (2 font-lock-function-name-face))
			      ("(\\(in-theory \\)" (1 font-lock-builtin-face))
			      ("(\\(local \\)"     (1 font-lock-builtin-face))
			      ("(\\(enable \\)"    (1 font-lock-builtin-face))
			      ("(\\(disable \\)"   (1 font-lock-builtin-face))
			      ("(\\(e/d \\)"       (1 font-lock-builtin-face))

			      ("\\(in-theroy\\)"  (1 font-lock-warning-face))
			      ("\\(gaurd\\)"      (1 font-lock-warning-face))
			      ("(\\(fatal \\)"    (1 font-lock-warning-face))
			      ("(\\(raise \\)"    (1 font-lock-warning-face))
			      ("(\\(er \\)"       (1 font-lock-warning-face))
			      ("(\\(warn \\)"     (1 font-lock-warning-face))
			      
			      )))



;; This is a really wonderful command.  You can type C-; and it will automatically
;; comment out whatever defun or defthm or encapsulate you are sitting on.  (It
;; comments all the way back to whatever line has a in column zero.)

(defun comment-defun ()
  (interactive)
  (beginning-of-defun)
  (let ((lower (point)))
    (forward-sexp)
    (comment-region lower (point))))

(global-set-key (kbd "C-;") 'comment-defun)


;; You can type M-; to comment out a region after you highlight it.

(global-set-key (kbd "M-;") 'comment-region)


;; You can type C-M-; to uncommand a region after you have highlighted it.

(global-set-key (kbd "C-M-;") 'uncomment-region)




;; This is another wonderful command.  If you type C-, it will collapse
;; all the whitespace across newlines together and leave you with just
;; one space.  This is really useful after emacs has garbled your indenting.
;; Doug Harper at Rockwell Collins wrote this.

(defun just-one-space-across-newlines ()
  "Delete all spaces, tabs and newlines around point, leaving one space."
  (interactive)
  (let ((point (point)))

    ;; Replace a white character at point and any trailing whitespace
    ;; by a single space.

    (if (and (re-search-forward "[ \t\n]+" nil t)
             (equal point (match-beginning 0)))
        (replace-match " "))

    ;; Replace leading whitespace (if any) and a black character at
    ;; point by a single space followed by the black character.

    (goto-char point)

    (cond
     ;; First look for <black white*> just before, or including point;
     ;; if found, replace it by <black blank>.

     ((and (re-search-backward "\\([^ \t\n]\\)[ \t\n]*" nil t))
      (if (equal (match-end 0) point)
          (if (equal (buffer-substring point (1+ point)) " ")
              (replace-match "\\1")
            (replace-match "\\1 "))))

     ;; Failing that, look for <start-of-buffer white*> just before,
     ;; or including point; if found, replace it by <start-of-buffer
     ;; blank>.

     (t
      (goto-char (point-min))
      (if (and (re-search-forward "[ \t\n]*" nil t)
               (or (equal (match-end 0) point)
                   (equal (match-end 0) (1+ point))))
          (replace-match "\\1 "))))))

(global-set-key (kbd "C-,") 'just-one-space-across-newlines)



(defun advance-to-prev-whitespace ()
  (interactive)
  (let ((point (point)))
    (re-search-backward "[ \t\n]+" nil t)))

(global-set-key (kbd "C-3") 'advance-to-next-whitespace)


(defun flatten-whitespace-until (finished)
  (advance-to-prev-whitespace)
  (if (>= (point) finished)
      (progn
	(just-one-space-across-newlines)
	(flatten-whitespace-until finished))))

(defun flatten-sexpr ()
  (interactive)
  (let ((start-point (point)))
    (re-search-forward "\)" nil t)
    (flatten-whitespace-until start-point)))

(global-set-key (kbd "C-4") 'flatten-sexpr)






;; Abbreviations are great, and you can add your own abbreviations to the
;; global table.
;;
;; Basically just type what is on the left side, e.g., hie.  Then hit C-'
;; and it will be expanded into whats on the right hand side, e.g.,
;; :hints(("Goal" :in-theory (enable .  This can save a lot of typing.

(define-abbrev-table 'text-mode-abbrev-table '())
(define-abbrev-table 'lisp-mode-abbrev-table '())
(define-abbrev-table 'fundamental-mode-abbrev-table '())
(define-abbrev-table 'global-abbrev-table
  '(("ap" "(accumulated-persistence t)")
    ("sap" "(show-accumulated-persistence :frames-a)")
    ("hi" ":hints((\"Goal\" :in-theory (")
    ("hie" ":hints((\"Goal\" :in-theory (enable ")
    ("hid" ":hints((\"Goal\" :in-theory (disable ")
    ("hied" ":hints((\"Goal\" :in-theory (e/d (")
    ("hil" ":hints((\"Goal\" :induct (len")
    ("huil" ":hints((\"Goal\" :use ((:instance lemma")
    ("id" ":in-theory (disable ")
    ("ie" ":in-theory (enable ")
    ("ied" ":in-theory (e/d (")
    ("dn" ":do-not '(generalize fertilize)")
    ("bt" "(setq ccl::*backtrace-print-level* 3 ccl::*backtrace-print-length* 4)")
    ("lie" "(local (in-theory (enable ")
    ("lid" "(local (in-theory (disable ")
    ("lied" "(local (in-theory (e/d ")
    ("d" "(disassemble ")
    ("d$" "(disassemble$ ")

    ("xa"    "/share/apps/fv/jared/xa/cn")
    ("xae"   "/share/apps/fv/jared/xa/cn/e")
    ("xab"   "/share/apps/fv/jared/xa/cn/e/acl2/books")
    ("xabc"  "/share/apps/fv/jared/xa/cn/e/acl2/books/centaur")
    ("xac"   "/share/apps/fv/jared/xa/cn/e/cbooks")
    ("xap"   "/share/apps/fv/jared/xa/cn/proofs")

    ("xb"    "/share/apps/fv/jared/xb/cn")
    ("xbe"   "/share/apps/fv/jared/xb/cn/e")
    ("xbb"   "/share/apps/fv/jared/xb/cn/e/acl2/books")
    ("xbbc"  "/share/apps/fv/jared/xb/cn/e/acl2/books/centaur")
    ("xbc"   "/share/apps/fv/jared/xb/cn/e/cbooks")
    ("xbp"   "/share/apps/fv/jared/xb/cn/proofs")

    ("xc"    "/share/apps/fv/jared/xc/cn")
    ("xce"   "/share/apps/fv/jared/xc/cn/e")
    ("xcb"   "/share/apps/fv/jared/xc/cn/e/acl2/books")
    ("xcbc"  "/share/apps/fv/jared/xc/cn/e/acl2/books/centaur")
    ("xcc"   "/share/apps/fv/jared/xc/cn/e/cbooks")
    ("xcp"   "/share/apps/fv/jared/xc/cn/proofs")

    ("xd"    "/share/apps/fv/jared/xd/cn")
    ("xde"   "/share/apps/fv/jared/xd/cn/e")
    ("xdb"   "/share/apps/fv/jared/xd/cn/e/acl2/books")
    ("xdbc"  "/share/apps/fv/jared/xd/cn/e/acl2/books/centaur")
    ("xdc"   "/share/apps/fv/jared/xd/cn/e/cbooks")
    ("xdp"   "/share/apps/fv/jared/xd/cn/proofs")

    ("xg"    "/share/apps/fv/jared/xg/cn")
    ("xge"   "/share/apps/fv/jared/xg/cn/e")
    ("xgb"   "/share/apps/fv/jared/xg/cn/e/acl2/books")
    ("xgbc"  "/share/apps/fv/jared/xg/cn/e/acl2/books/centaur")
    ("xgc"   "/share/apps/fv/jared/xg/cn/e/cbooks")
    ("xgp"   "/share/apps/fv/jared/xg/cn/proofs")

    ("xp"    "/n/fv2/jared/xp/cn")
    ("xpe"   "/n/fv2/jared/xp/cn/e")
    ("xpab"  "/n/fv2/jared/xp/cn/e/acl2/books")
    ("xpabc" "/n/fv2/jared/xp/cn/e/acl2/books/cexbtaur")
    ("xpc"   "/n/fv2/jared/xp/cn/e/cbooks")
    ("xpp"   "/n/fv2/jared/xp/cn/proofs")

    ("xcm"    "/share/apps/fv/jared/xcm/cn")
    ("xcme"   "/share/apps/fv/jared/xcm/cn/e")
    ("xcmb"   "/share/apps/fv/jared/xcm/cn/e/acl2/books")
    ("xcmbc"  "/share/apps/fv/jared/xcm/cn/e/acl2/books/centaur")
    ("xcmc"   "/share/apps/fv/jared/xcm/cn/e/cbooks")
    ("xcmp"   "/share/apps/fv/jared/xcm/cn/proofs")

    ))

(global-set-key (kbd "C-'") 'expand-abbrev)



;; This wraps a (force ) around some s-expression and then puts you at the
;; start of the next s-expression.  I bind it to Ctrl+F.

(global-set-key
 (kbd "C-f")
 (fset 'force-hyp
       "(force \206)\206\202"))


;; Ctrl+enter will send a theorem to the other window.
;; So will Ctrl+t,Ctrl+e and Ctrl+q
(global-set-key [(control return)] 'enter-theorem-other-window)
(global-set-key (kbd "C-q") 'enter-theorem-other-window)

(defun instaswitch-to-shell ()
  (interactive)
  (switch-to-buffer "*shell*"))

;; Ctrl+] will instaswitch to the *shell* buffer.
(global-set-key (kbd "C-]") 'instaswitch-to-shell)

;; Ctrl+\ will also instaswitch to the *shell* buffer, because otherwise
;; I hit it by accident when I'm trying to hit C-], and it prompts me to
;; enter an "input method", which is really irritating.
(global-set-key (kbd "C-\\") 'instaswitch-to-shell)

(global-set-key (kbd "C-x C-]") 'switch-to-buffer)



(defun pbt-1 ()
  (interactive)
  (insert ":pbt 0")
  (comint-send-input))

;; Ctrl+P will show you the session history (:pbt 0)
(global-set-key (kbd "C-p") 'pbt-1)


(global-set-key (kbd "C-/") 'replace-string)
(global-set-key (kbd "C-M-/") 'replace-regexp)

;(load "~/.ido.el")
(ido-mode t)


(global-set-key (kbd "C-e") 'end-of-line)

(global-set-key (kbd "C-t k") 'join-line)

(show-paren-mode)  ;; turn paren mode on
(auto-fill-mode)   ;; turn auto fill mode off


(defun pretty ()
  (interactive)
  (set-background-color "#000049")
  (set-foreground-color "white")
  (set-cursor-color "yellow")
  (set-face-foreground 'font-lock-comment-face "LightSkyBlue2")
  (set-face-foreground 'font-lock-builtin-face "Yellow")
  (set-face-foreground 'font-lock-keyword-face "Aquamarine")
  (set-face-foreground 'font-lock-function-name-face "Violet")
  (set-face-foreground 'font-lock-string-face "LightPink")
  (set-face-foreground 'font-lock-type-face "#aaddff")
  (set-face-foreground 'font-lock-variable-name-face "#aaffaa")
  (setq ansi-color-names-vector
    ["black" "red" "#90ff90" "yellow" "#9090ff" "magenta" "cyan" "white"])
  (setq ansi-color-map (ansi-color-make-color-map)))

(defun dark ()
  (interactive)
  (set-background-color "#000000")
  (set-foreground-color "#ffffff")
  (set-cursor-color "#ffff00")
  (set-face-foreground 'font-lock-comment-face "#b0efff")
  (set-face-foreground 'font-lock-keyword-face "#90ffa0")
  (set-face-foreground 'font-lock-function-name-face "#ff90ff")
  (set-face-foreground 'font-lock-string-face "#ffc090")
  (set-face-foreground 'font-lock-builtin-face "#ffff00")
  (set-face-foreground 'font-lock-type-face "#aaccff")
  (set-face-foreground 'font-lock-variable-name-face "#ffff99")
  (setq ansi-color-names-vector
    ["black" "red" "#90ff90" "yellow" "#9090ff" "magenta" "cyan" "white"])
  (setq ansi-color-map (ansi-color-make-color-map)))

(defun light ()
  (interactive)
  (set-background-color "grey98")
  (set-foreground-color "black")
  (set-cursor-color "DarkRed")
  (set-face-foreground 'font-lock-comment-face "DarkBlue")
  (set-face-foreground 'font-lock-builtin-face "Yellow4")
  (set-face-foreground 'font-lock-keyword-face "DarkGreen")
  (set-face-foreground 'font-lock-function-name-face "DarkViolet")
  (set-face-foreground 'font-lock-string-face "DarkRed")
  (set-face-foreground 'font-lock-type-face "#003399")
  (set-face-foreground 'font-lock-variable-name-face "#990066")
  (setq ansi-color-names-vector
    ["black" "red" "#009000" "yellow" "#000090" "magenta" "cyan" "white"])
  (setq ansi-color-map (ansi-color-make-color-map)))


(defun pretty2 ()
  (interactive)
  (set-background-color "light yellow")
  (set-foreground-color "DarkGreen")
  (set-cursor-color "DarkRed")
  (set-face-foreground 'font-lock-comment-face "DarkBlue")
  (set-face-foreground 'font-lock-builtin-face "SaddleBrown")
  (set-face-foreground 'font-lock-keyword-face "Purple3")
  (set-face-foreground 'font-lock-function-name-face "DarkRed")
  (set-face-foreground 'font-lock-string-face "Red3")
  (set-face-foreground 'font-lock-type-face "#003399")
  (set-face-foreground 'font-lock-variable-name-face "#990066")
  (setq ansi-color-names-vector
    ["black" "red" "#009000" "#907000" "#000090" "magenta" "cyan" "white"])
  (setq ansi-color-map (ansi-color-make-color-map)))


(defun server ()
  (interactive)
  (set-background-color "#003000")
  (set-foreground-color "#ffffff")
  (set-cursor-color "#f0f0a0")
  (set-face-foreground 'font-lock-comment-face "#c0d0c0")
  (set-face-foreground 'font-lock-builtin-face "#90ffff")
  (set-face-foreground 'font-lock-keyword-face "#ffffa0")
  (set-face-foreground 'font-lock-function-name-face "#ffb090")
  (set-face-foreground 'font-lock-string-face "#ffc0c0")
  (setq ansi-color-names-vector
    ["black" "#ff9090" "#90f090" "yellow" "#9090f0" "magenta" "cyan" "white"])
  (setq ansi-color-map (ansi-color-make-color-map)))




;; hrmn, is there a way to get lisp highlighting in shell mode?
;(setq shell-font-lock-keywords lisp-font-lock-keywords)

;; hrmn, this odesn't seem to do it either

;; (setq font-lock-defaults
;;       '((lisp-font-lock-keywords
;; 	 lisp-font-lock-keywords-1 lisp-font-lock-keywords-2)
;; 	nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
;; 	(font-lock-mark-block-function . mark-defun)
;; 	(font-lock-syntactic-face-function
;; 	 . lisp-font-lock-syntactic-face-function)))

(setq inferior-lisp-program "bash")

(pretty)


(if (equal window-system 'x)
    (add-hook 'c-mode-hook '(lambda ()
			      (c-set-style "bsd")
			      (font-lock-mode 1)
			      (column-number-mode t)
			      (setq comment-start "// ")
			      (setq comment-end   "")
			      (setq indent-tabs-mode nil)
			      (c-set-offset 'comment-intro 0)
			      (setq c-basic-offset 4)
			      (setq tab-width 4)
			      (abbrev-mode 0)  ;; it does crazy things
			      )))

(if (equal window-system 'x)
    (add-hook 'c++-mode-hook '(lambda ()
			      (font-lock-mode 1)
			      (column-number-mode t)
			      (abbrev-mode 0)
			      (setq c-basic-offset 4)
			      (setq c-indent-level 4)
			      (c-set-style "stroustrup"))))


(defun indent-region-according-to-mode ()
  (interactive)
  (save-excursion
    (let* ((min (min (point) (mark)))
	   (max (max (point) (mark))))
      (goto-char min)
      (while (<= (point) max)
	(indent-according-to-mode)
	(next-line 1)
	(end-of-line)))))

(if (equal window-system 'x)
    (add-hook 'ruby-mode-hook '(lambda ()
				 (font-lock-mode t)
				 (column-number-mode t)
				 (define-key ruby-mode-map [(control meta q)] 'indent-region-according-to-mode)
				 )))


(defun docstring ()
  (interactive)
  (text-mode)
  (set-fill-column 70)
  (auto-fill-mode t))



(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

(put '%defun 'lisp-indent-function 'defun)
(put '%thm 'lisp-indent-function 'defun)

(put 'with-local-stobj      'lisp-indent-function 'defun)
(put 'with-fast-alist      'lisp-indent-function 'defun)
(put 'with-fast-alists      'lisp-indent-function 'defun)
(put 'pattern-match       'lisp-indent-function 'defun)
(put 'encapsulate       'lisp-indent-function 'defun)
(put 'PATTERN-MATCH       'lisp-indent-function 'defun)
(put 'pattern-match-list       'lisp-indent-function 'defun)
(put 'PATTERN-MATCH-LIST       'lisp-indent-function 'defun)
(put 'mv-let       'lisp-indent-function 'defun)
(put 'MV-LET       'lisp-indent-function 'defun)
(put 'mlet*       'lisp-indent-function 'defun)
(put 'MLET*       'lisp-indent-function 'defun)
(put 'B*       'lisp-indent-function 1)
(put 'b*       'lisp-indent-function 1)
(put 'dk       'lisp-indent-function 'defun)
(put 'verify-guards  'lisp-indent-function 'defun)
(put 'with-output 'lisp-indent-function 'defun)
(put 'cutil::defprojection 'lisp-indent-function 'defun)
(put 'cutil::deflist 'lisp-indent-function 'defun)
(put 'cutil::defalist 'lisp-indent-function 'defun)
(put 'cutil::defaggregate 'lisp-indent-function 'defun)
(put 'cutil::defenum 'lisp-indent-function 'defun)
(put 'with-open-file 'lisp-indent-function 'defun)
(put 'with-stdout 'lisp-indent-function 'defun)
(put 'with-output-to 'lisp-indent-function 'defun)
(put 'with-acl2-channels-bound 'lisp-indent-function 'defun)
(put 'when-have-feature 'lisp-indent-function 'defun)
(put 'aig-cases 'lisp-indent-function 'defun)
(put 'getopt::defoptions 'lisp-indent-function 'defun)
(put 'hunchentoot:define-easy-handler 'lisp-indent-function 'defun)




(require 'ansi-color)

(defun ansi-buffer ()
  (interactive)
  (let ((place (point))
        (start)
        (end))
    (beginning-of-buffer)
    (setq start (point))
    (end-of-buffer)
    (setq end (point))
    (ansi-color-apply-on-region start end)
    (goto-char place)))



