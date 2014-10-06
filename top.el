; Jared's Emacs Configuration
; Top file.

(load "server")

(unless (server-running-p)
  (server-start))

;; TAGS configuration.
;(tags-reset-tags-tables)
(setq tags-revert-without-query nil)
(setq tags-add-tables nil)

(add-to-list 'load-path "emacs")

(when *at-centaur*
  (load "~/emacs/ssh.el"))

(load "~/emacs/haml-mode.el")
(load "~/emacs/emacs-acl2.el")
(load "~/emacs/jared.el")
(load "~/emacs/verilog-mode.el")
(load "~/emacs/nasm-mode.el")

(setq verilog-auto-endcomments    nil)
(setq verilog-indent-level        2)
(setq verilog-indent-level-module 2)
(setq verilog-indent-level-declaration 2)
(setq verilog-indent-level-behavioral  2)

(put 'narrow-to-region 'disabled nil)

(add-to-list 'completion-ignored-extensions ".out")
(add-to-list 'completion-ignored-extensions ".cert")
(add-to-list 'completion-ignored-extensions ".lx64fsl")
(add-to-list 'completion-ignored-extensions ".boot")
(add-to-list 'completion-ignored-extensions ".body")
(add-to-list 'completion-ignored-extensions ".mtime")
(add-to-list 'completion-ignored-extensions ".pcert")
(add-to-list 'completion-ignored-extensions ".time")
(add-to-list 'completion-ignored-extensions ".port")
(add-to-list 'completion-ignored-extensions ".acl2x")
(add-to-list 'completion-ignored-extensions ".mpcert")

(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))
;(setq load-path (append load-path '("/u/jared")))


(add-to-list 'auto-mode-alist '("\\.v\\'" . verilog-mode))
(add-to-list 'auto-mode-alist '("\\.iospec\\'" . verilog-mode))
(add-to-list 'auto-mode-alist '("\\.acl2\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.\\(asm\\|nasm\\|s\\)$" . nasm-mode))

(put 'shell-resync-dirs 'disabled nil)

(setq *printer-name* "hp5")
(setq ps-printer-name "hp5")

(column-number-mode t)


(setq-default show-trailing-whitespace t)
;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(safe-local-variable-values (quote ((auto-fill . t))))
;;  '(verilog-auto-endcomments nil)
;;  '(verilog-auto-indent-on-newline nil)
;;  '(verilog-auto-lineup (quote nil))
;;  '(verilog-auto-newline nil)
;;  '(verilog-auto-reset-widths nil)
;;  '(verilog-auto-star-expand nil)
;;  '(verilog-indent-begin-after-if t)
;;  '(verilog-indent-lists nil)
;;  '(verilog-minimum-comment-distance 0)
;;  '(verilog-tab-always-indent nil))



(defun mv-ify ()
  (interactive)
  (let* ((word   (thing-at-point 'word))
         (bounds (bounds-of-thing-at-point 'word))
         (repl   (cond
                  ((equal word "first") "0")
                  ((equal word "car") "0")
                  ((equal word "second") "1")
                  ((equal word "cadr") "1")
                  ((equal word "third") "2")
                  ((equal word "caddr") "2")
                  ((equal word "fourth") "3")
                  ((equal word "fifth") "4")
                  ((equal word "sixth") "5")
                  ((equal word "seventh") "6"))))
    (if repl
        (progn
          (delete-region (car bounds) (cdr bounds))
          (insert (concat "mv-nth " repl)))
      (message
       (concat "Unrecogized word: " word)))))

(global-set-key (kbd "C-z") 'mv-ify)


(defun cvt ()
  (interactive)
  (goto-char 0) (query-replace "<code>"   "@({")
  (goto-char 0) (query-replace "</code>"  "})")
  (goto-char 0) (query-replace "<tt>"     "@('")
  (goto-char 0) (query-replace "</tt>"    "')")
  (goto-char 0) (query-replace "&amp;"    "&")
  (goto-char 0) (query-replace "&lt;"     "<")
  (goto-char 0) (query-replace "&gt;"     ">")
  (goto-char 0) (query-replace "&quot;"   "\""))





; Rebind control-pagedown (next) and control-pageup (prior) to do what pagedown
; and pageup do, because I hit this by accident sometimes, and control-pagedown
; brings up an irritating window asking me if I want to enable some confusing
; scroll-left command. (I don't).

(global-set-key [(control next)] 'scroll-up-nomark)
(global-set-key [(control prior)] 'scroll-down-nomark)

; actually allow me to load a buffer into multiple frames!

(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)


;; Ido-mode redefines C-x C-f to be fancy, but it's slow on big directories.
;; So, I use C-x C-r on these directories instead.  Well, this used to work:
;;  (global-set-key [(control x) (control r)] 'find-file)
;;
;; But now apparently ido-mode destroys find-file and just fucks you over
;; totally.
;;
;; All I want to do is be able to open a goddamn absolute path to a file when I
;; copy and paste one.  There's no fucking way to do it with ido-mode because
;; when I copy and paste it into the C-x C-f prompt, ido starts trying fucks it
;; up and just always does something totally godawful.  As an experiment,
;;
;;    touch /home/users/jared/temp.file
;;    copy "/home/users/jared/temp.file" to the clipboard or whatever
;;    C-x C-f     (ido prompt: Find file: ~/)
;;    type C-y    (ido prompt: Find file: ~//home/users/jared/temp.file [no match])
;;
;; Okay, a different way:
;;
;;    C-x C-f     (ido prompt: Find file: ~/)
;;    type "//"   (ido prompt: Find file: /)
;;    type C-y    (ido prompt: Find file: //home/users/jared/temp.file [no match])
;;
;; Why no match?  Who fucking knows.  If I hit enter to confirm, it just takes me
;; back to opening a file in ~/.
;;
;; Well, is the problem this extra slash?
;;
;;    Copy "home/users/jared/temp.file" to the clipboard
;;    C-x C-f    (ido prompt: Find file: ~/)
;;    type "//"  (ido prompt: Find file: /)
;;    type C-y   (ido prompt: Find file: /home/users/jared/temp.file [no match])
;;
;; What the fuck?  Why is this no match?  If I say confirm, it takes me to opening
;; a file in /home/users/jared.
;;
;; So basically this is just totally fucked up and there is no way to just tell it
;; to open a file.
;;
;; Okay, you can fix this by just using C-f to go back to the old find-file mode
;; and then pasting it in.  So the right thing to do is:
;;
;;   C-x C-f (start ido mode)
;;   C-f     (go back to old find-file mode)
;;   C-y     (paste in the absolute path)





(defun shell-directory-tracker (str)
  "Tracks cd, pushd, popd, and upto commands issued to the shell.
This function is called on each input passed to the shell.
It watches for cd, pushd, popd, and upto commands and sets the buffer's
default directory to track these commands.

You may toggle this tracking on and off with \\[shell-dirtrack-mode].
If Emacs gets confused, you can resync with the shell with \\[dirs].
\(The `dirtrack' package provides an alternative implementation of this
feature - see the function `dirtrack-mode'.)

See variables `shell-cd-regexp', `shell-chdrive-regexp', `shell-pushd-regexp',
and  `shell-popd-regexp', while `shell-pushd-tohome', `shell-pushd-dextract',
and `shell-pushd-dunique' control the behavior of the relevant command.

Environment variables are expanded, see function `substitute-in-file-name'."
  (if shell-dirtrackp
      ;; We fail gracefully if we think the command will fail in the shell.
      (condition-case chdir-failure
      (let ((start (progn (string-match
                   (concat "^" shell-command-separator-regexp)
                   str) ; skip whitespace
                  (match-end 0)))
        end cmd arg1)
        (while (string-match shell-command-regexp str start)
          (setq end (match-end 0)
            cmd (comint-arguments (substring str start end) 0 0)
            arg1 (comint-arguments (substring str start end) 1 1))
          (if arg1
          (setq arg1 (shell-unquote-argument arg1)))
          (cond ((string-match (concat "\\`\\(" shell-popd-regexp
                       "\\)\\($\\|[ \t]\\)")
                   cmd)
             (shell-process-popd (comint-substitute-in-file-name arg1)))
            ((string-match (concat "\\`\\(" shell-pushd-regexp
                       "\\)\\($\\|[ \t]\\)")
                   cmd)
             (shell-process-pushd (comint-substitute-in-file-name arg1)))
            ((string-match (concat "\\`\\(" shell-cd-regexp
                       "\\)\\($\\|[ \t]\\)")
                   cmd)
             (shell-process-cd (comint-substitute-in-file-name arg1)))
            ((string-match (concat "\\`\\(" "upto"
                       "\\)\\($\\|[ \t]\\)")
                   cmd)
             (let* ((name (comint-substitute-in-file-name arg1))
                (here (directory-file-name default-directory)))
               (while (and (not (equal (file-name-nondirectory here) name))
                   (not (equal here "/")))
             (setq here (directory-file-name (file-name-directory here))))
               (shell-process-cd here)))
            ((and shell-chdrive-regexp
              (string-match (concat "\\`\\(" shell-chdrive-regexp
                        "\\)\\($\\|[ \t]\\)")
                    cmd))
             (shell-process-cd (comint-substitute-in-file-name cmd))))
          (setq start (progn (string-match shell-command-separator-regexp
                           str end)
                 ;; skip again
                 (match-end 0)))))
    (error "Couldn't cd"))))








; Stop warning me about undoing big things, because it pops up an additional
; window that is irritating.
(setq warning-suppress-types '((undo discard-info)))
(setq undo-outer-limit 40000000)




; <damd> clop: you can use the variables split-window-preferred-function and split-height/width-threshold

;; possible way to not get all these damn buffer splits
;; when i end up with three or four instead
;; <fledermaus> clop: ,,dv special-display-regexps
;; <fsbot> List of regexps saying which buffers should be displayed specially.
;; <fsbot> Displaying a buffer with `display-buffer' or `pop-to-buffer', if
;; <fsbot> any regexp in this list matches its name, displays it specially ..[Type ,more]
;; <fledermaus> you may be able to hack up the behaviour you want using that variable and related functions/variables 
;; <fledermaus> however the implementation will be semi-non-trivial and is left as an exercise for the reader 
;; <tazle> hm, I wonder if there's a sane way to get Emacs to remember currently open buffers over restart


; Per apropos split-window-sensibly: this basically tells emacs never to
; automatically split a window vertically.
;; split-width-threshold was 160
;; split-height-threshold was 80
(setq split-width-threshold nil)



(defun replace-html-chars-region (start end)
  "Replace “<” to “&lt;” and other chars in HTML.
This works on the current region."
  (interactive "r")
  (save-restriction 
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward "&" nil t) (replace-match "&amp;" nil t))
    (goto-char (point-min))
    (while (search-forward "<" nil t) (replace-match "&lt;" nil t))
    (goto-char (point-min))
    (while (search-forward ">" nil t) (replace-match "&gt;" nil t))
    )
  )

(global-set-key (kbd "C-t h") 'replace-html-chars-region)


(global-set-key (kbd "C-c h")   'hs-hide-block)
(global-set-key (kbd "C-c C-h") 'hs-hide-block)
(global-set-key (kbd "C-c C-M-h") 'hs-hide-all)
(global-set-key (kbd "C-c s")   'hs-show-block)
(global-set-key (kbd "C-c C-s") 'hs-show-block)
(global-set-key (kbd "C-c C-M-s") 'hs-show-all)


(add-hook 'haml-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (define-key haml-mode-map "\C-m" 'newline-and-indent)))




(require 'flyspell)
(let ((map flyspell-mode-map))
  (define-key map [(control ?\,)] nil)
  (define-key map [(control ?\;)] nil)
  )


(defun goto-sexpr (n)
  (interactive "NFly away to sexpr number: ")
  (goto-char (point-min))
  (forward-sexp (+ n 1))
  (backward-sexp))

(global-set-key (kbd "C-t C-g") 'goto-sexpr)


(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
(set-face-background 'show-paren-match-face "#60ffff")
(set-face-foreground 'show-paren-match-face (face-foreground 'default))
(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)
;(setq show-paren-delay 0)


(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq ido-max-directory-size 100000)





(defun my-lisp-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
   (goto-char (1+ (elt state 1)))
   (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
   (if (and (elt state 2)
	    (not (looking-at "\\sw\\|\\s_")))
       ;; car of form doesn't seem to be a symbol
       (progn
	 (if (not (> (save-excursion (forward-line 1) (point))
		     calculate-lisp-indent-last-sexp))
	     (progn (goto-char calculate-lisp-indent-last-sexp)
		    (beginning-of-line)
		    (parse-partial-sexp (point)
					calculate-lisp-indent-last-sexp 0 t)))
	 ;; Indent under the list or under the first sexp on the same
	 ;; line as calculate-lisp-indent-last-sexp.  Note that first
	 ;; thing on that line has to be complete sexp since we are
	 ;; inside the innermost containing sexp.
	 (backward-prefix-chars)
	 (current-column))
     (let ((function (buffer-substring (point)
				       (progn (forward-sexp 1) (point))))
	   method)
       (setq method (or (get (intern-soft function) 'lisp-indent-function)
			(get (intern-soft function) 'lisp-indent-hook)))
       (cond
	((string-match "\\`:" function)
	 ;; If the first element is a keyword, then indent subsequent
	 ;; elements only to the current column.
	 (let ((containing-form-start (elt state 1)))
	   ;; this is the location of the "(" behind the starting keyword symbol
	   (goto-char containing-form-start)
	   ;; after the "("
	   (forward-char 1)
	   ;; after the first symbol
	   (forward-sexp 1)
	   ;; start of the first symbol
	   (backward-sexp 1)
	   (current-column)))

	((or (eq method 'defun)
	     (and (null method)
		  (> (length function) 3)
		  ;; match things beginning with def or with in any package
		  ;; \` means beginning, \' means end.
		  (or (string-match "\\(\\`\\|::\\)def" function)
		      (string-match "\\(\\`\\|::\\)with" function)
		      (string-match "\\(\\`\\|::\\)case" function)
		      (string-match "case\\'" function))))
	 (lisp-indent-defform state indent-point))
	((integerp method)
	 (lisp-indent-specform method state
			       indent-point normal-indent))
	(method
	 (funcall method indent-point state)))))))

(setq lisp-indent-function 'my-lisp-indent-function)

;; do not make me type "yes" sometimes, because switching between "y"
;; and "yes" is horrible and unintuitive
(defalias 'yes-or-no-p 'y-or-n-p)


(instaswitch-to-shell)
(delete-other-windows)