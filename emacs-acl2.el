; Attempt to set *acl2-sources-dir*.
(if (and (not (boundp '*acl2-sources-dir*))
	 (file-name-absolute-p load-file-name))
    (let ((pattern (if (string-match "[\\]" load-file-name)
		       "\[^\\]+\\*$"
		     "/[^/]+/*$"))
	  (dir (file-name-directory load-file-name)))
      (let ((posn (string-match pattern dir)))
	(if posn
	    (setq *acl2-sources-dir*
		  (substring dir 0 (1+ posn)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Control-t keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (not (boundp 'ctl-t-keymap))

; This trick probably came from Bob Boyer, to define a new keymap; so now
; control-t is the first character of a complex command.
  (defvar ctl-t-keymap)
  (setq ctl-t-keymap (make-sparse-keymap))
  (define-key (current-global-map) "\C-T" ctl-t-keymap)

; Control-t t now transposes characters, instead of the former control-t.
  (define-key ctl-t-keymap "\C-T" 'transpose-chars)
  (define-key ctl-t-keymap "\C-t" 'transpose-chars)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General shell stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Start up a shell.  This also loads in comint-mode, used below.

(shell)
(ansi-color-for-comint-mode-on)
(font-lock-mode 0)


; Do meta-x new-shell to start a new shell.
(defvar number-of-other-sshs 0)
(defvar ssh-target "fv-hpc")

(defun new-shell ()
  "Start up another shell."
  (interactive)
  (let ((bufname (concat "*shell-" 
			 (number-to-string
			  (setq number-of-other-sshs
				(+ 1 number-of-other-sshs)))
			 "*")))
    (switch-to-buffer
     (make-comint bufname (or (getenv "SHELL")
			      "bash")))
    (shell-mode)))

; Avoid killing shell buffers by accident:
(defun kill-buffer-without-process (name)
  "Kill a buffer unless there's a process associated with it."
  (interactive
   (let (val
         (default-name (buffer-name (current-buffer)))
         (table
          (mapcar (function (lambda (x) (cons (buffer-name x) x))) (buffer-list))))
     (setq val (completing-read (format "Kill buffer: (default: %s) "
                                        default-name)
                                table
                                nil
                                t))
     (list (if (equal val "")
               default-name val))))
  (if (get-buffer-process name)
      (error "Process is active in the indicated buffer.  Use meta-x kill-buffer instead.")
    (kill-buffer name)))

(define-key (current-global-map) "\C-Xk" 'kill-buffer-without-process)

; Variable *acl2-shell* is the name of the "ACL2 shell", the buffer to which
; forms are written by various commands defined in this file.  Control-t c
; (defined below) changes the ACL2 buffer.
(defvar *acl2-shell* "*shell*")

; Set the ACL2 shell to the current buffer.
(define-key ctl-t-keymap "c" 'set-shell-buffer)
(defun set-shell-buffer ()
  (interactive)
  (setq *acl2-shell* (buffer-name (current-buffer))))


(defun set-local-shell-buffer ()
  (interactive)
  (make-local-variable '*acl2-shell*)
  (setq *acl2-shell* (buffer-name (window-buffer (next-window)))))

(define-key ctl-t-keymap "\C-c" 'set-local-shell-buffer)

; Change to the ACL2 shell.
(define-key ctl-t-keymap "b" 'switch-to-shell)
(defun switch-to-shell ()
  (interactive)
  (switch-to-buffer *acl2-shell*))

; Send the current form to the ACL2 shell.  Here, the "current form" is the one
; starting with the immediately preceding left parenthesis in column 0.  (It is
; OK to stand on that parenthesis as well.)
;(define-key ctl-t-keymap "e" 'enter-theorem)

; Old version (before v2-8) hardwires in the use of *shell*.
;(defalias 'enter-theorem
;  (read-kbd-macro
;   "C-e C-M-a NUL C-M-f ESC w C-x b *shell* RET M-> C-y"))

(define-key ctl-t-keymap "\C-e" 'enter-theorem-other-window)

(defun acl2-scope-start-p ()
  (looking-at
   "(encapsulate[ \t]*\\(;;.*\\)?\n[ \t\n]*()[ \t]*;; start lemmas for"))

(defun acl2-beginning-of-def ()
; See the documentation for enter-theorem.  We return nil unless we go
; to a preceding package marker, #!pkg, in which case we return t.
  (let ((saved-point (point))
	(ans nil))
    (end-of-line)
    (beginning-of-defun)
    (let ((temp-point (point)))
      (cond ((not (equal temp-point (point-min)))
	     (forward-line -1)
	     (cond ((looking-at "#!")
		    (setq ans t))
		   (t (goto-char temp-point))))))
    (cond ((acl2-scope-start-p)
	   (goto-char saved-point)
	   (if (not (looking-at "("))
	       (backward-up-list))
	   (let ((scope-p (acl2-scope-start-p)))
	     (or scope-p
		 (progn (while (not scope-p)
			  (setq saved-point (point))
			  (backward-up-list)
			  (setq scope-p (acl2-scope-start-p)))
			(goto-char saved-point))))))
    ans))

(defun acl2-current-form-string (&optional ignore-pkg-marker)
  ;(save-excursion
    (end-of-line)
    (let ((temp (acl2-beginning-of-def)))
      (let ((beg (point)))
	(if (and temp (not ignore-pkg-marker))
	    (forward-line 1))
	(forward-sexp)
	(buffer-substring beg (point)))))

;; (defun better-beginning-of-defun ()
;;   ;; I think this was my old version that doesn't work at the start of the buffer.
;;   (interactive)
;;   (beginning-of-defun)
;;   (let ((start-def (point)))
;;     (previous-line)
;;     (beginning-of-line)
;;     (let ((here (point)))
;;       (unless (equal (buffer-substring here (+ 2 here)) "#!")
;; 	(goto-char start-def)))))

;; I do this in .jared.el now
;; (defun enter-theorem-other-window ()
;;   (interactive)
;;   (push-mark) ; I think I sometimes like to go back to the form.
;;   (let (str)
;;     (end-of-line)
;;     (better-beginning-of-defun)
;;     (let ((beg (point)))
;;       (unless (equal (buffer-substring (point) (+ (point) 1)) "(")
;; 	(next-line))
;;       (forward-sexp)
;;       (setq str (buffer-substring beg (point))))
;;     (other-window 1)
;;     (switch-to-buffer *acl2-shell*)
;;     (end-of-buffer)
;;     (insert str)
;;     (other-window 0)
;;     (end-of-buffer)))

;; (defun enter-theorem ()
;;   (interactive)
;;   (push-mark) ; I think I sometimes like to go back to the form.
;;   (let (str)
;;     (end-of-line)
;;     (beginning-of-defun)
;;     (let ((beg (point)))
;;       (forward-sexp)
;;       (setq str (buffer-substring beg (point))))
;;     (set-buffer *acl2-shell*)
;;     (end-of-buffer)
;;     (insert str))
;;   (switch-to-buffer *acl2-shell*)
;;   ;; The rest seemed necessary in some older emacs version, when set-buffer
;;   ;; above was instead switch-to-buffer and the above switch-to-buffer was
;;   ;; omitted, but this no longer seems necessary.
;;   ;;(end-of-buffer)
;;   )

; Avoid killing process with control-d in shell buffer:
(define-key comint-mode-map "\C-d" 'delete-char)

; The following only seems necessary in gnu.
(define-key comint-mode-map "Œ" 'c-m-l)

; Allow use of meta-p and meta-n for command completion.  Multiple
; meta-p/meta-n commands cycle backward/forward through previous matching
; commands.
; See also emacs lisp source file lisp/comint.el.
(define-key comint-mode-map "\ep" 'comint-previous-matching-input-from-input)
(define-key comint-mode-map "\en" 'comint-next-matching-input-from-input)

; Bind control-<RETURN> to the command that brings the current buffer's
; directory back to what it is supposed to be.
(define-key global-map "\C-\M-M" 'shell-resync-dirs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Write region to shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The next forms support control-t l (ell), which writes the current region to
; file "./temp-emacs-file.out" and puts an appropriate LD command in the shell
; buffer.

(defvar *shell-temp-file-name* "temp-emacs-file.out")

(defvar *shell-temp-file-directory*)

(defun set-shell-temp-file-directory ()
  (setq *shell-temp-file-directory*
	"./"))

(defun shell-temp-file-name ()
  (expand-file-name *shell-temp-file-name* (set-shell-temp-file-directory)))

(defun write-region-for-shell (beg end)
  "Writes the current region to the shell temp file, with the header
   string at the top and the footer string at the bottom and <return> separating each.
   Assumes beg < end."
  (let ((flg (buffer-modified-p)))
    (save-excursion
      (goto-char beg)
      (write-region beg end (shell-temp-file-name)))
    (set-buffer-modified-p flg)))

(defun send-region-to-shell (message)
  "Writes the current region to the shell temp file and then puts one at the
   end of the ACL2 shell buffer, ready to submit that file."
  (let ((beg (min (point) (mark)))
	(end (max (point) (mark)))
	(shell *acl2-shell*))
    (write-region-for-shell beg end)
    (other-window 1)
    (switch-to-buffer shell)
    (end-of-buffer)
    (insert message)))

(defun acl2-load ()
  "Writes the current region to the shell temp file and then puts the cursor
   at the end of the ACL2 shell buffer, ready to execute an ld."
  (interactive)
  (send-region-to-shell
   (concat (format
	    ";; Ready to execute ACL2-LOAD -- hit <RETURN> when ready\n")
	   (format "(acl2::time$ (acl2::ld \"%s\" :LD-PRE-EVAL-PRINT acl2::t :ld-error-action :return))"
		   (shell-temp-file-name)))))

(defun acl2-load-inhibited ()
  "Writes the current region to the shell temp file and then puts the cursor
   at the end of the ACL2 shell buffer, ready to execute an ld with output
   inhibited and proofs skipped."
  (interactive)
  (send-region-to-shell
   (concat (format
	    ";; Ready to execute ACL2-LOAD -- hit <RETURN> when ready\n")
	   (format "(acl2::time$ (acl2::with-output :off :all (acl2::ld \"%s\" :ld-error-action :return :ld-skip-proofsp t)))"
		   (shell-temp-file-name)))))

(define-key ctl-t-keymap "l" 'acl2-load)
(define-key ctl-t-keymap "\C-l" 'acl2-load-inhibited)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some editing commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Contributed by Bill Bevier:
(defun find-unbalanced-parentheses ()
  "Finds parenthesis mismatch error in buffer. Reads through all of the
current buffer and tries to find places in which the parentheses do not
balance. Positions point to possible trouble-spots, printing out a message
that says what the trouble appears to be.  This command only finds
one such error; if you suspect more errors, run it again."
  (interactive)
  (let ((saved-point (point)))
    (goto-char (point-min));; Go to start of buffer.
    (let (old-point)
      (setq old-point (point))
      (forward-sexp)
      (while (not (equal (point) old-point))
	(setq old-point (point))
	(forward-sexp)))
    (goto-char saved-point)
    (message "All parentheses appear balanced.")))

(defun cursor-at-end-and-bottom ()
  "Put cursor at the end of the buffer on the bottom line"
  (interactive)
  (recenter -1))

; Control-t Control-a puts current line (line with cursor) at bottom of window:
(define-key ctl-t-keymap "\C-a" 'cursor-at-end-and-bottom)

; Control-t <TAB> completes filename in any buffer:
(define-key ctl-t-keymap "\t" 'comint-dynamic-complete-filename)

(defun scroll-up-half ()
  (interactive)
  (scroll-up (/ (window-height) 2)))

(defun scroll-down-half ()
  (interactive)
  (scroll-down (/ (window-height) 2)))

; Like control-v, but only half a screen:
(define-key ctl-t-keymap "\C-V" 'scroll-up-half)

; Like meta-v, but only half a screen:
(define-key ctl-t-keymap "v" 'scroll-down-half)

(defun search-forward-with-case (string)
  (interactive "sSearch: ")
  (let ((case-fold-search nil))
    (search-forward string)))

; Case-sensitive forward search (i.e., searches forward non-interactively, with
; string supplied in minibuffer).
(define-key ctl-t-keymap "s" 'search-forward-with-case)

; Forward search (case-insensitive by default):
(define-key ctl-t-keymap "\C-s" 'search-forward)

(define-key (current-global-map) "\C-\M-q" 'indent-sexp)

(define-key ctl-t-keymap "" 'up-list)

; For the following, set compare-windows-whitespace to something other than "[
; \t\n]+"
; if desired.
(defun approx-compare-windows (&optional ignore-case)
  "Compare windows, ignoring whitespace.  If optional argument is supplied,
then also ignore case if that argument is positive, else do not ignore case."
  (interactive "P")
  (if ignore-case
      (let ((compare-ignore-case (> ignore-case 0)))
	(compare-windows "0"))
    (compare-windows "0")))

; Set compare-windows-whitespace to something other than "[ \t\n]+"
; if desired.  Also consider compare-ignore-case.
(define-key ctl-t-keymap "w" 'compare-windows)
(define-key ctl-t-keymap "q" 'approx-compare-windows)


(defun my-lisp-mode-hook ()
  (setq indent-tabs-mode nil)   
  (column-number-mode t)
  (setq comment-column 0)
  (hs-minor-mode)
  (setq comment-indent-function 'comment-indent-default)
  (flyspell-prog-mode))

(if (not (boundp 'lisp-mode-hook)) (setq lisp-mode-hook nil))
(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)

; Other modes can be put below as well (asm, c++, c, perl, emacs-lisp).
(if (equal window-system 'x)
    (add-hook 'lisp-mode-hook '(lambda () (font-lock-mode 1))))

(defun acl2-sources-dir ()
  (let ((dir
	 (if (boundp '*acl2-sources-dir*)
	     *acl2-sources-dir*
	   (setq *acl2-sources-dir*
		 (expand-file-name
		  (read-file-name
		  "*acl2-sources-dir* (e.g. /u/acl2/v2-9/acl2-sources/): "
		  nil nil t))))))
    (if (or (equal dir "")
	    (let ((lastch (aref "abc/" (1- (length "abc/")))))
	      (and (not (equal lastch ?/))
		   (not (equal lastch ?\\)))))
	(concat dir
		(if (and (string-match "[\\]" dir)
			 (not (string-match "/" dir)))
		    "\\"
		  "/"))
      dir)))

(defun visit-acl2-tags-table ()
  "Visit the tags table for ACL2."
  (interactive)
  (visit-tags-table (concat (acl2-sources-dir) "TAGS")))

; Set the right margin (used when auto-fill-mode is on).
(set-default 'fill-column 79)

; From Boyer?  See documentation for fill-format-string.  This is useful both
; for format and for ACL2's printing functions fmt and fms.
(define-key ctl-t-keymap "f" 'fill-format-string)

(defun jared-at-likely-string-start-p (pos)
  ;; Is the character at pos a quote-character that isn't preceeded
  ;; by a backslash?
  (and (equal (char-after pos) 34)
       (or (equal pos 0)
	   (not (equal (char-after (- pos 1)) 92)))))

(defun jared-find-string-start ()
  (let ((pos (point)))
    (while (and (> pos 0)
		(not (jared-at-likely-string-start-p pos)))
      (setq pos (- pos 1)))
    (when (jared-at-likely-string-start-p pos)
      (goto-char pos))))

(defun fill-format-string ()

  "Remove the ~<newline>'s from a Lisp format statement, and
put in new ones, after any space, in such a way that
the next space does not pass fill-column."

  (interactive "")

  (jared-find-string-start)
;  (or (equal (char-after (point)) 34)
;      (error "Call fill-format-string immediately in front of a string."))
  (let ((start-point (point))
	(fill (make-string (+ 1 (current-column)) 32)))
    (forward-sexp 1)
    (let ((end-point (point))
	  (new-end nil))
      (save-restriction
	(narrow-to-region (+ 1 start-point)
			  (- end-point 1))
	(goto-char (point-min))
	(while (re-search-forward "~\n" nil t)
	  (delete-char -2)
	  (while (or (looking-at " ")
		     (looking-at "\t")
		     (looking-at "\n"))
	    (delete-char 1)))
	(goto-char (point-max))
	(setq new-end (point)))
      (save-restriction
	(beginning-of-line)
	(narrow-to-region (point)
			  new-end)
	(goto-char (+ 1 start-point))
	(while (re-search-forward "[ \t]" nil t)
	  (cond ((next-break-too-far)
		 (insert "~\n")
		 (insert fill))))))))

(defun next-break-too-far ()
  (let ((p (point)))
    (cond ((equal (point) (point-max))
	   nil)
	  (t (cond ((re-search-forward "[ \t\n]" nil t)
		    (prog1
			(>= (current-column) fill-column)
		      (goto-char p)))
		   (t (goto-char (point-max))
		      (prog1
			  (>= (current-column) fill-column)
			(goto-char p))))))))

;; Make case and case-match indent like defun.
(put 'case 'lisp-indent-function 'defun)
(put 'case! 'lisp-indent-function 'defun)
(put 'case-match 'lisp-indent-function 'defun)
(put 'dolist     'lisp-indent-function 'defun)


(defun fill-xdoc-string ()
  (interactive "")
  (jared-find-string-start)
  (let ((start-point (point))
	(fill (make-string (+ 1 (current-column)) 32)))
    (forward-sexp 1)
    (let ((end-point (point))
	  (new-end nil))
      ;; Walk through the string and collapse any whitespace across lines
      ;; into a single space.
      (save-restriction
	(narrow-to-region (+ 1 start-point)
			  (- end-point 1))
	(goto-char (point-min))
	(while (re-search-forward "[ \t]*\n[ \t\n]*" nil t)
	  (replace-match " "))
	(goto-char (point-max))
	(setq new-end (point)))
      ;; Now do a basic word-wrapping pass, inserting the fill string after
      ;; each line.
      (save-restriction
	(beginning-of-line)
	(narrow-to-region (point)
			  new-end)
	(goto-char (+ 1 start-point))
	(while (re-search-forward "[ \t]" nil t)
	  (cond ((next-break-too-far)
		 (replace-match "")
		 (insert "\n")
		 (insert fill))))))))

(define-key ctl-t-keymap "x" 'fill-xdoc-string)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACL2 proof-tree support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *acl2-interface-dir*)

(defun acl2-interface-dir ()
  (if (boundp '*acl2-interface-dir*)
      *acl2-interface-dir*
    (setq *acl2-interface-dir*
	  (concat (acl2-sources-dir) "interface/emacs/"))))

(autoload 'start-proof-tree
  (concat (acl2-interface-dir) "top-start-shell-acl2")
  "Enable proof tree logging in a prooftree buffer."
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run ACL2 as inferior process in emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You may have better luck simply issuing your ACL2 command in an ordinary
; (emacs) shell.  But in case anyone wants to try this:

(autoload 'run-acl2
  (concat *acl2-interface-dir* "top-start-inferior-acl2")
  "Open communication between acl2 running in shell and prooftree."
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACL2 proof-checker support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Insert  DV  command that gets to subexpression at the cursor.
; This is for use with the P and TH commands.
(define-key ctl-t-keymap "d" 'dv-manual)

; Insert DIVE command that gets to subexpression at the cursor.
; This is for use with the PP command.
(define-key ctl-t-keymap "\C-d" 'dive-manual)

; The rest of the functions in this section support \C-t d and \C-t \C-d. 

(defvar *acl2-pc-dive-syntax-table* nil)

(defun maybe-set-acl2-pc-dive-syntax-table ()
  (cond ((null *acl2-pc-dive-syntax-table*)
	 (setq *acl2-pc-dive-syntax-table*
	       (copy-syntax-table (syntax-table)))
	 (modify-syntax-entry ?- "w" *acl2-pc-dive-syntax-table*)
	 (modify-syntax-entry ?: "w" *acl2-pc-dive-syntax-table*)
	 (modify-syntax-entry ?_ "w" *acl2-pc-dive-syntax-table*)
	 (modify-syntax-entry ?+ "w" *acl2-pc-dive-syntax-table*)
	 (modify-syntax-entry ?* "w" *acl2-pc-dive-syntax-table*)
	 (modify-syntax-entry ?. "w" *acl2-pc-dive-syntax-table*)
	 *acl2-pc-dive-syntax-table*)))

(defun dive-manual ()
  "Returns the 0-based address of the current s-expression inside
the expression beginning at the margin, assuming that the point
is properly inside the margin (otherwise causes an error), then
moves to the end of the buffer and plops down the appropriate DIVE
command for the proof-checker.  Causes an error if one is already
at the top."
  (interactive)
  (let ((addr (find-address)))
    (end-of-buffer)
    (if (null addr)
	(error "Null address.")
      (insert (prin1-to-string (cons 'dive addr))))))

(defun dv-manual ()
  "Returns the 0-based address of the current s-expression inside
the expression beginning at the margin, assuming that the point
is properly inside the margin (otherwise causes an error), then
moves to the end of the buffer and plops down the appropriate DV
command for the proof-checker. Causes an error if one is already at the top."
  (interactive)
  (let ((addr (find-address)))
    (end-of-buffer)
    (if (null addr)
	(error "Null address.")
      (insert (prin1-to-string (cons 'dv addr))))))

(defun beginning-of-current-defun ()
  "Causes an error if one is already at the beginning of defun, in
the sense of c-m-a"
;  (interactive)
  (let ((old-point (point)))
    (end-of-defun)
    (beginning-of-defun)
    (or (not (equal (point) old-point))
	(error "Already at the beginning of the expression."))))

(defun find-address ()
  "Returns the 0-based address of the current s-expression inside
the expression beginning at the margin.  Leaves one at the original point."
  (maybe-set-acl2-pc-dive-syntax-table)
  (with-syntax-table
      *acl2-pc-dive-syntax-table*
    (let (quit-point old-point result)
      (setq old-point (point))
      (beginning-of-current-defun)
      (setq quit-point (point))
      (goto-char old-point)
      (while (not (equal (point) quit-point))
	(setq result (cons (move-up-one-level) result)))
      (goto-char old-point)
      result)))

(defun move-up-one-level ()
  "Like backward-up-list, except that it returns the position
of the current s-expression in the enclosing list"
;  (interactive)
  (let (saved-point final-point n)
    (forward-sexp) ; puts us just past the end of current sexp
    (setq saved-point (point))
    (backward-up-list 1)
    (setq final-point (point))
    (forward-char 1)
    (forward-sexp)
    (setq n 0)
    (while (not (equal (point) saved-point))
      (setq n (1+ n))
      (forward-sexp))
    (goto-char final-point)
    n))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun acl2-info ()
  "Starts up info pointing at top of acl2 documentation"
  (interactive)
  (info (concat (acl2-sources-dir) "doc/EMACS/acl2-doc-emacs.info")))

(setq display-time-interval 10)
(display-time) ; turn off with (display-time-mode)

; Disable commands that we do not want to execute by mistake:
(put 'shell-resync-dirs 'disabled t)
(put 'suspend-or-iconify-emacs 'disabled t)
(put 'suspend-emacs 'disabled t)
(put 'iconify-or-deiconify-frame 'disabled t)

(autoload 'tex-insert-quote "tex-mode" nil t)
(define-key global-map "\C-[\"" 'tex-insert-quote)







