; SLIME 

;;; SLIME setup to startup LISP and FREEDIUS

;;; This version for SLIME version 3.0 as of Fri Feb 13 2009 which has many changes and increased
;;; dependence on the contrib subdir.

(message "Loading $FREEDIUS/emacs/freedius-slime.el\n")


;;; In you emacs init file add
;;; (add-to-list 'load-path (concat (getenv "FREEDIUS") "/emacs"))
;;; (add-hook 'slime-load-hook (lambda () (require 'slime-hacks)))
;;; (require 'freedius-slime)

;;; In your HOME directory add the file .swank.lisp containing:
;;;     #+sbcl (setq swank::*communication-style* :fd-handler)
 

;;; CONFIGURATION PARAMETERS:

;;; Since you got to this file, I assume its directory is already on Emacs load-path.

(defvar *default-slime-load-path* "/m/rom1/downloads/slime/slime")

;;; Set this to t to call (CL-USER::LOAD-CME) when slime-run-freedius is called.
(defvar *freedius-auto-load* t) 

;;; Configure these as needed for your situation  -- this needs to be rethought
;;(defvar *default-freedius-cmucl-arch-name* "linux-cmucl-redhat-9")
(defvar *default-freedius-cmucl-arch-name* "linux-cmucl")
(defvar *default-cmucl-executable* "cmucl")
(defvar *default-freedius-allegro-arch-name* "linux-acl")
(defvar *default-allegro-executable* "allegro")
(defvar *default-sbcl-executable* "sbcl")
(defvar *default-sbcl32-executable* "sbcl32")
(defvar *default-sbcl64-executable* "sbcl64")
(defvar *default-freedius-sbcl32-arch-name* "linux-sbcl32")
(defvar *default-freedius-sbcl64-arch-name* "linux-sbcl64")


;;; Initialization of various SLIME modes.

(defvar slime-enabled nil)

;;; SLIME was recently changed to use selectively loaded modules in the contrib 
;;; subdirectory.  This function 

;;; Configure this in .emacs or emacs.el
;;; Set to NIL for old (non-modular) versions of SLIME

(when nil ;; old
(defvar slime-default-contribs 
  '(slime-autodoc 
    ;;slime-c-p-c
    ;;slime-editing-commands
    slime-fancy-inspector
    ;;slime-fuzzy
    slime-highlight-edits
    slime-presentations
    slime-scratch
    slime-xref-browser
    ;;slime-references
    ))
)

(defvar slime-default-contribs
  '(slime-repl
    slime-autodoc
    ;;slime-c-p-c
    ;;slime-editing-commands
    slime-fancy-inspector
    ;;slime-fuzzy
    slime-highlight-edits
    slime-presentations
    slime-scratch
    slime-references
    ;;slime-xref-browser
    ;;slime-references
    ;; slime-asdf ; 
    ))

(defun enable-slime-contribs ()
  (when (require 'slime-autoloads nil t)
    ;; will be skipped for old (non-modular) versions of SLIME
    (slime-setup slime-default-contribs)
    ))


(defun enable-slime ()
  (unless slime-enabled
    (interactive)
    (add-to-list 'load-path *default-slime-load-path*)
    (add-to-list 'load-path (concat *default-slime-load-path* "/contrib"))
    (require 'slime-autoloads)
    (enable-slime-contribs)
    (require 'slime)
    (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))  ; make slime-mode automatic in lisp buffers
    (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

    (setq slime-keys
	  (append slime-keys
		  '(("\C-z\C-w" slime-compile-region :inferior t)
		    ("\C-z\C-e" slime-compile-defun :inferior t)
		    ("\C-z\a" slime-arglist :inferior t))))
    (slime-init-keymaps)
    ;;(slime-autodoc-mode t)
    (setq slime-enabled t)
    ))

(defvar *last-slime-connected-hook* nil)

;; (remove-hook 'slime-connected-hook 'load-cme)

(defun slime-run-lisp (command-and-args &optional hook)
  (enable-slime)
  (setq *last-slime-run-lisp-cmd* command-and-args)
  (setq inferior-lisp-program command-and-args)
  (when *last-slime-connected-hook*
    (remove-hook 'slime-connected-hook *last-slime-connected-hook*))
  (setq *last-slime-connected-hook* hook)
  (when hook (add-hook 'slime-connected-hook hook t))
  (slime))

(defun slime-run-freedius0 (freedius-arch command-and-args &optional hook)
  (enable-slime)
  (let* ((freedius-path (getenv "FREEDIUS"))
	 (freedius-arch-path (format "%s/arch/%s" freedius-path freedius-arch))
	 ;(f3dlib (format "~s/lib" freedius-arch-path))
	 )
    (setenv "FREEDIUS_EXEC_PREFIX" freedius-arch-path)
    (setenv "FREEDIUS_ARCH" freedius-arch-path)
    (setenv "F3DA" freedius-arch-path)
    (slime-run-lisp command-and-args hook)
    ))


(defun slime-run-freedius (freedius-arch lisp-executable load-cmd &optional hook)
  (slime-run-freedius0 freedius-arch
		       (if load-cmd
			   (format "%s %s %s/lisp/boot.lisp" lisp-executable load-cmd (getenv "FREEDIUS"))
			   lisp-executable)
		       hook))

(defun slime-run-freedius-disksave (freedius-arch lisp-executable run-disksave-cmd disksave-name 
				    &optional hook)
  (slime-run-freedius0 freedius-arch
		       (format "%s %s %s/arch/%s/bin/%s"
			       lisp-executable run-disksave-cmd
			       (getenv "FREEDIUS") freedius-arch disksave-name)
		       hook))

(defun allegro ()
  (interactive)
  (slime-run-lisp *default-allegro-executable*))

(defun cmucl ()
  (interactive)
  (slime-run-lisp *default-cmucl-executable*))

(defun sbcl ()
  (interactive)
  (slime-run-lisp *default-sbcl-executable*))

;;; Quam's customized SBCL with extended SIGSEGV handling.
(defun sbcl-hacked ()
  (interactive)
  (slime-run-lisp "/opt/sbcl/sbcl-1.0.25/run-sbcl.sh"))

(defun sbcl32 ()
  (interactive)
  (slime-run-lisp *default-sbcl32-executable*))

(defun cmucl-experimental ()
  (interactive)
  (slime-run-lisp "/opt/bin/cmucl-experimental"))

(defun cmucl-old ()
  (interactive)
  (slime-run-lisp "/opt/bin/cmucl-old"))

;;; LHQ Wed May 21 2008 
;;;  There are still rough edges in starting FREEDIUS GUI under SLIME.
;;;  Everything appears to be fine using LOAD-CME-NO-START, but LOAD-CME (which starts the GUI)
;;;  often either crashes the CMUCL process or screws up the *slime-repl* buffer.
;;;
;;; Until this problem is fixed, set FREEDIUS-LOAD-FUNCTION to LOAD-CME-NO-START.  
;;; To start the FREEDIUS GUI, evaluate (CL-USER::START-CME) in the *slime-repl* buffer.
(defvar freedius-load-function 'load-cme-no-start)
;;(setq freedius-load-function 'load-cme)

(defun f3dcmu ()
  (interactive)
  (slime-run-freedius *default-freedius-cmucl-arch-name* *default-cmucl-executable* "-load" 
		      'load-cme-no-start2))
(when nil
(defun f3dcx ()
  (interactive)
  (slime-run-freedius *default-freedius-cmucl-arch-name* *default-cmucl-executable* nil 'load-boot-and-cme)))


(defun f3dcmudisksave ()
  (interactive)
  (slime-run-freedius-disksave *default-freedius-cmucl-arch-name* *default-cmucl-executable*
			       "-core" "freedius-save.core" 
			       'start-cme))

;;; For Allegro, run f3dacl in Emacs, then in the *inferior-lisp* buffer eval (load-cme).
;;; Everything appears to be just dandy.

(defun f3dacl ()
  (interactive)
  (slime-run-freedius *default-freedius-allegro-arch-name* *default-allegro-executable* "-L" 
		      freedius-load-function))

(defun f3dacldisksave ()
  (interactive)
  (slime-run-freedius-disksave *default-freedius-allegro-arch-name* *default-allegro-executable*
			       "-I" "freedius-disksave.dxl" 
			       ;; Problems autostarting Allegro FREEDIUS using SLIME.
			       ;; Must manually run (start-cme) in *inferior-lisp* buffer
			       'start-cme))
(defun f3dsb32 ()
  (interactive)
  (slime-run-freedius *default-freedius-sbcl32-arch-name* *default-sbcl32-executable* nil 'load-boot-and-cme))
	
(defun f3dsb64 ()
  (interactive)
  (slime-run-freedius *default-freedius-sbcl64-arch-name* *default-sbcl64-executable* nil  'load-boot-and-cme))

(defun load-boot-and-cme ()
  (interactive)
  (slime-eval-async 
   `(CL-USER::PROGN (CL-USER::LOAD ,(format "%s/lisp/boot.lisp" (getenv "FREEDIUS")))
		    (CL-USER::LOAD-CME)
		    NIL)))

(defun slime-repl-reset ()
  "Clear pending events blocking slime-autodoc-mode."
  (interactive)
  (setf (slime-rex-continuations) '()))

;;; old
(when nil

(defun load-cme ()
  (interactive)
  (slime-eval-async '(CL-USER::PROGN (CL-USER::LOAD-CME) NIL)))

(defun load-cme-no-start ()
  (interactive)
  (slime-switch-to-output-buffer)
  (slime-eval-async '(CL-USER::PROGN (CL-USER::LOAD-CME :START NIL) NIL)))

(defun start-cme ()
  (interactive)
  (slime-eval-async '(CL-USER::PROGN (CL-USER::START-CME) NIL)))

(defun load-systems (system-list)
  (interactive)
  (slime-eval-async `(LOOP FOR S IN ,SYSTEM-LIST DO (ST::LOAD-SYSTEM S))))

); end old

(defun load-cme ()
  (interactive)
  (slime-switch-to-output-buffer)
  (slime-repl-eval-string "(CL-USER::PROGN (CL-USER::LOAD-CME) NIL)"))

(defun load-cme-no-start ()
  (interactive)
  (slime-switch-to-output-buffer)
  (with-current-buffer (slime-output-buffer)
    (slime-repl-eval-string "(CL-USER::PROGN (CL-USER::LOAD-CME :START NIL) NIL)")))

(defun load-cme-no-start ()
  (interactive)
  (slime-eval-async '(CL-USER::PROGN (CL-USER::LOAD-CME :START NIL) NIL))
  (slime-repl-eval-string "\";; (CL-USER::START-CME)\""))

(defun load-cme-no-start2 ()
  (interactive)
  (slime-eval-async '(CL-USER::PROGN (CL-USER::LOAD-CME :START NIL) NIL))
  (slime-repl-eval-string "\";; In *inferior-lisp* buffer evaluate (CL-USER::START-CME)\""))

(defun start-cme ()
  (interactive)
  (slime-switch-to-output-buffer)
  (slime-repl-eval-string "(CL-USER::PROGN (CL-USER::START-CME) NIL)"))

(defun load-systems (system-list)
  (interactive)
  (slime-switch-to-output-buffer)
  (slime-repl-eval-string "(LOOP FOR S IN ,SYSTEM-LIST DO (ST::LOAD-SYSTEM S))"))


(provide 'freedius-slime) 
