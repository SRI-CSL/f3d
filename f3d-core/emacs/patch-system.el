;;; ***********************   A LISPM-like PATCH SYSTEM FOR GNU-EMACS/SLIME   *************************
;;; ***********************       by Lynn H. Quam,  Tue Mar 20 2007    *************************

;;; The files $FREEDIUS/lisp/system-tool/patch-sys.lisp and
;;; $FREEDIUS/lisp/system-tool/emacs-patch-sys.lisp contains the Lisp
;;; side of the patch system.  If supports building the patch
;;; directories and loading patches.


;;; FIXME:  Need to add prefix to all functions and variables in this file
;;;         in order to prevent Emacs symbol conflicts.

;; GNU EMACS PATCH SYSTEM COMMANDS 
;;  ******************************
;;
;; start-patch (system)
;;   Begin a new patch for the specified system.
;;
;; start-private-patch (filename)
;;   Begin a new Private Patch in the specified file.
;;
;; add-patch 
;;   Add selected definition to current patch buffer.
;;
;; add-patch-region
;;   Add selected region to current patch buffer.
;;
;; finish-patch
;;   Finish the current patch.
;;
;; resume-patch (system patch-number)
;;   Resume a previous patch, setting the patch status to :incomplete
;;  
;; continue-patch (system patch-number)
;;   Resume a previous patch, leaving the patch status unchanged.
;;  
;; resume-private-patch (filename)
;;   Resume a previous private patch.
;;
;; abort-patch
;;   Kill the current patch.
;;
;; select-patch
;;   Select an open patch using a pop-up menu.
;;
;; reset-patch-system
;;   Reset state of patch system -- forget about previous systems and patches.
;;
;; edit-patch-comment
;;   Switch to the buffer *CURRENT-PATCH-COMMENT*.
;;
;; show-patch-index (system)
;;   Switch to the buffer containing the Patch Index for the specified system.
;;
;; show-patches 
;;   Show the state of any patches that are open.


;;;; FIXME -- replace use of this with standard Emacs interactive conventions.
(defun read-string-prompt-default (prompt &optional default gmacsp)
  (when (or (null prompt) (zerop (length prompt)))
    (setq prompt "Enter String: ")
    )
  (when (stringp default)
    (setq prompt (concat prompt "(Default: \"" default "\") "))
    )

  (let ((string (read-string prompt)))
    (if (equal string "") default string)
    ))

(defun probe-file (path)
  (file-exists-p path ))

(defvar *patch-systems* nil)
;(setq *patch-systems* nil *current-patchable-system* nil)

(defvar *current-patchable-system* nil)
(defvar *patch-comment-buffer* nil)
(defvar *current-patch* nil)
(defvar *all-open-patches* nil)

(defvar *finish-patch-force-comment-edit* nil
  "Set to Non-NIL to force edit of comment buffer before completing finish-patch.
Must execute M-X finalize-patch to finalize the patch.")

(defvar *start-patch-comment-previous-buffer* nil)


;;;  ***************************   STRUCTURE DEFINITIONS  **************************

(defstruct patchable-system
  name 
  major-version
  patch-directory-pathname
  patch-list
  patches-loaded
  default-package
)

(defstruct (patch)
  number
  status
  date-and-time
  author
  comment
  system entry buffer file)

(defun patch-pathname (patch)
  (patch-file patch))

(defun patch-current-time-string ()
  (format-time-string "%Y-%m-%d %H:%M:%S %Z"))

;;(patch-current-time-string)

(defun make-patch2 (number status date-and-time author comment)
  (make-patch :number number :status status :date-and-time date-and-time 
	      :author author :comment comment))

(defun make-patch3 (patch-number)
  (make-patch2 patch-number ':INCOMPLETE
	       (patch-current-time-string) (user-login-name) nil))

(defun get-patch (system patch-number)
  (let ((l (patchable-system-patch-list system)))
    (while (and l (not (equal (patch-number (car l)) patch-number)))
      (setq l (cdr l)))
    (car l)))

(defun patch-entry-list (patch-entry)
  (list (patch-number patch-entry)
	(patch-status patch-entry)
	(patch-date-and-time patch-entry)
	(patch-author patch-entry)
	(patch-comment patch-entry)))

 ;;; should attempt to get rid of most of this page -- bad use of global variables

(defun current-patch-system ()
  (patch-system *current-patch*))

(defun current-patch-buffer ()
  (and *current-patch*
       (patch-buffer *current-patch*)))

(defun current-patch-number ()
  (patch-number *current-patch*))

(defun current-patch ()
  *current-patch*)

(defun current-patch-pathname ()
  (patch-pathname *current-patch*))

(defun set-current-patch (patch)
  (when patch
    (when *current-patch* (save-patch-comment (current-patch)))
    (open-patch patch)
    (setq *current-patch* patch )
    (set-patch-comment-buffer (patch-comment (current-patch)))
    )
  patch )

(defun close-patch (open-patch)
  (setq *all-open-patches* (delq open-patch *all-open-patches*))
  (when (eq *current-patch* open-patch)
    (setq *current-patch* nil))
  )

;;(close-patch *current-patch*)

(defun open-patch (open-patch)
  (unless (memq open-patch *all-open-patches*)
    (setq *all-open-patches* (cons open-patch *all-open-patches*))) )

(defun find-open-patch (system patch-number)
  (let ((l *all-open-patches*))
    (while (and l (not (and (eq (patch-system (car l)) system)
			    (equal (patch-number (car l)) patch-number ))))
      (setq l (cdr l)))
    (if l (car l))))


;;; Various lisps print NIL in uppercase.  Emacs LISP requires nil to be lowercase.
(defun hack-nil (thing)
  (if (eq thing 'NIL)
      nil
      thing))

;;(make-directory (patchable-system-patch-directory-pathname (find-patch-system-named "image")) t)
;;(substitute-in-file-name (patchable-system-patch-directory-pathname (find-patch-system-named "image")))


;;;
;;; Changed to accomodate systems without major version numbers -- CIC 8/4/97
;;;
(defun system-major-version-name (name version)
  (if (null (hack-nil version))
      name
      (if (or (numberp version)
	      (not (and (>= (length version) (length name))
			(equal (subseq version 0 (length name)) name))))
	  (format "%s-%s" name version)
	  version)))

;;; Not clear that Emacs should ever create the patch directories.
;;; System construction on the Lisp side should do this.
(defun create-patch-directory (system)
  (let ((dir-path (patchable-system-patch-directory-pathname system)))

    (if (probe-file dir-path )
        (message "The patch directory for system %s already exists." (patchable-system-name system) )
        ;(make-directory (file-name-as-directory dir-path) t)
	(make-directory dir-path t)
	)

    (unless (probe-file dir-path )
      (error "Cannot create directory %s~%" dir-path))
    
    ;;(create-file-buffer (patch-index-pathname system))
    (unless (probe-file (patch-index-pathname system))
      ;;(append-to-file ";;; -*- Lisp -*-" nil (patch-index-pathname system))
      ;;(create-file-buffer (patch-index-pathname system))
      (write-patch-index system)
      )
    dir-path))


(defun patch-index-path-prefix (system)
  (format "%s/%s" 
	  (patchable-system-patch-directory-pathname system)
	  (system-major-version-name (patchable-system-name system) 
				     (patchable-system-major-version system))))

(defun patch-index-pathname (system)
  (format "%s.patch-index" (patch-index-path-prefix system)))


;;; LHQ Sat Apr 29 1995
;;; 3-digit-patch-numbers were introduced so that dired would show the patches in the correct order.
;;; I have changed my mind about how to accomplish the desired effect.
;;; Instead of forcing leading zeros in the patch number, START NUMBERING THE PATCHES AT 100.
;;; This hack will probably never be needed.
(defvar *make-3-digit-patch-numbers* nil)
;;; (setq *make-3-digit-patch-numbers* t) ; 

(defun make-patch-pathname (system patch-number &optional file-type 3digits-p)
  (unless file-type (setq file-type ".lisp"))
  (format (if 3digits-p "%s-%03d%s" "%s-%s%s")
	  (patch-index-path-prefix system)
          patch-number
          file-type ))

;(patch-index-path-prefix (car *patch-systems*))

(defun make-existing-patch-pathname (system patch-number)
  (if *make-3-digit-patch-numbers*
      (let ((path-3digs (make-patch-pathname system patch-number nil t)))
	    (if (file-exists-p path-3digs)
		path-3digs
		(make-patch-pathname system patch-number nil nil)))
      (make-patch-pathname system patch-number nil nil)))

;(defun get-patch-buffer (system patch-number)
;  (find-file-noselect (patch-pathname system patch-number)))

(defun read-patch-index (system )
  (let* ((file (patch-index-pathname system))
	 (prev-buf (current-buffer))
	 patch-list
	 (buf (find-file-noselect file t))
	 )
    (unless (probe-file file)
      (create-patch-directory system))
    (save-excursion
      (set-buffer buf)
      (when (probe-file file)
	(revert-buffer nil t))
      (setq buffer-read-only nil)
      (goto-char (point-min))
      (setq foo (read buf)) ; ignore the patch system header = (:PATCHABLE-SYSTEM  system-name :MAJOR-VERSION major-version)
      (while (and (not (eobp)) 
		  (let* ((pos (point))
			 (found (search-forward "(" nil t)))
		    (goto-char pos)
		    found))
	(let ((entry-list (read buf)))
	  (if (eql (length entry-list) 5) 
	      (setq patch-list (cons (apply 'make-patch2 entry-list) patch-list))
	      (error "Patch index entry %s does not contain 5 elements." entry-list)))
		    
	;;(setq foo patch-list)
	)
      
      (setf (patchable-system-patch-list system) (reverse patch-list))
      (set-buffer prev-buf)
      buf)))

(defvar *initial-patch-number* 100) ; start patch-numbers at 100 in empty patch systems.

(defun highest-patch-number-in-list (patch-list)
  (let ((highest 0))
    (dolist (entry patch-list highest)
      (if (and (numberp (patch-number entry))
	       (> (patch-number entry) highest))
	  (setq highest (patch-number entry))))
   (if (= highest 0)
       *initial-patch-number* ; no patch-numbers found in patch-list 
       highest)))

;(patch-index-pathname (find-patch-system-named "image"))
;(read-patch-index (find-patch-system-named "image"))
;(substitute-in-file-name (patch-index-pathname (find-patch-system-named "image")))

(defvar *write-patch-index-recursion* nil)

;;; Bug: This does not make a backup file for a patch-index on a remote
;;; (ANGE-FTP) filesystems.
(defun write-patch-index (system)
  (let ((file (patch-index-pathname system)) )
    ;;(setq *bar* (list file system))
    (unless (or *write-patch-index-recursion* (probe-file file))
      (let ((*write-patch-index-recursion* t))
	(create-patch-directory system)))
   	
    (let* ((prev-buf (current-buffer))
	   patch-list
	   (buf (find-file-noselect file t)) ; 2nd argument = T means do not issue warnings
	   )
   
      (if buf
	  (save-excursion  
	    (set-buffer buf)
	    (setq buffer-read-only nil)
	    (if (probe-file file) 
		(revert-buffer t t)
		t)
	    (when t
	      (erase-buffer)
	      (princ ";;; -*- Lisp -*-" buf)
	      (print (list ':PATCHABLE-SYSTEM (patchable-system-name system)
			   ':MAJOR-VERSION (patchable-system-major-version system) )
		     buf)
	      (terpri buf)
	      (dolist (patch (patchable-system-patch-list system))
		(print (patch-entry-list patch) buf))

	      ;;(write-region 1 (point-max) file)
	      (let ((trim-versions-without-asking t))
		(save-buffer))
	      )
	    (set-buffer prev-buf) )
	  ))))

(defun select-system (system-name)
  (let ((patch-system (or (find-patch-system-named system-name)
			  (find-patch-system-named (upcase system-name )))))
    (if patch-system
	(setq *current-patchable-system* patch-system)
	(error "No such system"))))

(defun start-patch-internal (system patch patch-pathname &optional default-package)
  (let* ((prev-buf (current-buffer))
	 (exists-p (file-exists-p patch-pathname))
	 (buf (find-file-noselect patch-pathname t))
	 (license-string (and system
			      (unless exists-p (get-system-license-string (patchable-system-name system)))))
	 )
    ;; ***** Wed Apr 19 1995 -- temporary until Lucid(Harlequin) fixes compiler bug *****
    (when (eq (patch-status patch) ':ABORTED)
      (error "This patch has been aborted"))
    ;;(setq foo (list system patch patch-pathname package license-string ))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (unless t ;exists-p
	(insert (format ";;; PATCH FILE by %s %s\n\n" 
			(patch-author patch)
			(patch-date-and-time patch)))
	)
      (save-buffer)
      (set-buffer prev-buf)
      (setf (patch-system patch) system
	    (patch-buffer patch) buf
	    (patch-file patch) patch-pathname)
      (set-current-patch patch))))

(defun start-patch (system-name)
  "Begin a new patch for the specified system."
  (interactive (list (read-string-prompt-default "System Name: " 
						 (buffer-patch-system-name (current-buffer)))))
  (unless system-name (error "no system name specified"))
  (let ((system (select-system system-name )) )
    (read-patch-index system)
    (let* ((highest-patch-number (highest-patch-number-in-list (patchable-system-patch-list system)))
	   (patch-number (1+ highest-patch-number))
	   (patch (make-patch3 patch-number )))
   
      (setf (patchable-system-patch-list system)
	    (append (patchable-system-patch-list system) (list patch)))
      (write-patch-index system)
      (start-patch-internal system patch 
			    (make-patch-pathname system patch-number nil *make-3-digit-patch-numbers*) 
			    ))))

(defun resume-patch (system-name patch-number)
  "Resume a previous patch, setting the patch status to :incomplete."
  (interactive (list (read-string-prompt-default 
		      "System Name: "
		      (buffer-patch-system-name (current-buffer)))
		     (string-to-number (read-string "Patch Number to resume: "))
		     ))
  (let* ((system (select-system system-name ) )
	 patch)
    (read-patch-index system)
    (setq patch (get-patch system patch-number))
    (setf (patch-status patch) ':INCOMPLETE)
    (write-patch-index system)
    (start-patch-internal system patch
			  (make-existing-patch-pathname system patch-number)
			  nil)))

(defun get-specified-patch-number (system string)
  (if (equal string "")
      (highest-patch-number-in-list (patchable-system-patch-list  system))
       (string-to-number string)))

;(buffer-patch-system-name (current-buffer))

(defun continue-patch (system-name patch-number)
  "Resume a previous patch, leaving the patch status unchnaged."
  (interactive 
   (let* ((system-name (read-string-prompt-default 
		       "System Name: " 
		       (buffer-patch-system-name (current-buffer))))
	 (system (select-system system-name ) )
	 (highest-patch-number 
	  (progn (read-patch-index system)
		 (and system (highest-patch-number-in-list (patchable-system-patch-list system)))))
	 (patch-number (read-string-prompt-default
			"Patch Number to resume: " (and highest-patch-number
							(format "%d" highest-patch-number)))))
     (list system-name patch-number)))
  (let* ((system (select-system system-name ) )
	 patch)
    (read-patch-index system)
    (unless (numberp patch-number)
      (setq patch-number (get-specified-patch-number system patch-number)))

    (setq patch (get-patch system patch-number))
    (start-patch-internal system patch
			  (make-existing-patch-pathname system patch-number)
			  nil)))

(defun lisp-buffer-package ()
  (slime-find-buffer-package))

;;; if the file contains (in-package 'foo) lisp-buffer-package returns "foo" rather than "FOO"
(defun upcase-lisp-buffer-package ()
  (let ((pkg (lisp-buffer-package)))
    (when pkg (upcase (lisp-buffer-package)))))

(defun start-private-patch (patch-pathname )
  "Begin a new Private Patch in the specified file."
  (interactive "FPrivate Patch File Name: ")
  (start-patch-internal nil (make-patch3 nil) patch-pathname (or (upcase-lisp-buffer-package) "LISP")))

(defun resume-private-patch (patch-pathname )
  "Resume a previous private patch."
  (interactive "FPrivate Patch File Name: ")
  (start-patch-internal nil (make-patch3 nil) patch-pathname nil))

(defun show-patch-index (system-name)
  "Switch to the buffer containing the Patch Index for the specified system"
  (interactive (list (read-string-prompt-default "System Name: "
						 (buffer-patch-system-name (current-buffer)))))
  (let ((system (select-system system-name ) ))
    (switch-to-buffer (read-patch-index system))
    ))

(defvar *show-patches-buffer* nil)

(defun show-patch (open-patch)
  (let* ((patch-system (patch-system open-patch))
	 (patch-number (patch-number open-patch))
	 (patch-file (patch-pathname open-patch))
	 (patch open-patch) ; (get-patch system patch-number )
	 (patch-comment (or (patch-comment patch) "")))
    (insert 
     (if patch-system 
	 (format "System: %s  Patch Number: %s \"%s\"\n\n" 
		 (patchable-system-name patch-system) patch-number patch-comment )
	 (format "Private Patch: %s \"%s\"\n" patch-file patch-comment )))
    ))

(defun show-patches-buffer ()
  (if (and *show-patches-buffer*
	   (buffer-name *show-patches-buffer*))
      *show-patches-buffer*
      (setq *show-patches-buffer* (get-buffer-create "*ACTIVE-PATCHES*"))))

(defun show-patches  ()
  "Show the state of any patches that are open"
  (interactive)
  (show-patches-buffer)
  (switch-to-buffer *show-patches-buffer* )
  ;;(set-buffer *show-patches-buffer*)
  (erase-buffer)
  (dolist (open-patch *all-open-patches*)
    (show-patch open-patch)))


(defun show-current-patch  ()
  "Show the state of currently selected patch"
  (interactive)
  (show-patches-buffer)
  (switch-to-buffer *show-patches-buffer* )
  ;;(set-buffer *show-patches-buffer*)
  (erase-buffer)
  (show-patch *current-patch*))

(defun show-current-patch  ()
  "Show the state of currently selected patch"
  (interactive)
  (switch-to-buffer (current-patch-buffer) )
  )


(defun select-patch ()
  "Select a Currently Open Patch"
  (interactive)
  (if (null *all-open-patches*)
      (warn "There are no open patches.")
      (let* ((menu 
	      (list "Patches Menu"
		    (cons "Select Patch"
			  (let ((tail *all-open-patches* )
				head)
			    (while tail
			      (let* ((patch (car tail))
				     )
				(setq head (cons
					    (cons
					     (format "%s%s"
						     (if (eq patch *current-patch*) "* " "  ")
						     (patch-pathname patch))
					     patch )
					    head)))
			      (setq tail (cdr tail)))
			    (reverse head) ))))
	     (selection (x-popup-menu t menu)))
	(set-current-patch selection)	  
	)))
	
(defun read-number-default (prompt default)
  (string-to-number (read-string-prompt-default prompt 
				      (if (numberp default) (int-to-string default) default ))))

(defvar *abort-patch-always-retain-entry* t)

;;; abort-patch behavior:
;;; When *abort-patch-always-retain-entry* = T, the patch-status is always marked as :ABORTED.
;;; It you really want it removed from the patch-index, do it yourself.

;;; When *abort-patch-always-retain-entry* = NIL, if patch is last entry in patch index the patch is
;;; deleted, otherwise marks entry as :ABORTED. 

(defun abort-patch ()
  "Kill a previous patch."
  (interactive )
  (if (not *current-patch*)
      (warn "No Current Patch")

      (let ((patch-system (patch-system *current-patch*))
	    (patch-number (patch-number *current-patch*)))
	(if patch-system
	    (let* ()
	      (read-patch-index patch-system)
	      (let ((entry (get-patch patch-system patch-number)))
		(if (and (not *abort-patch-always-retain-entry*)
			 (equal entry (car (last (patchable-system-patch-list patch-system)))))
		    (setf (patchable-system-patch-list patch-system)
			  (butlast (patchable-system-patch-list patch-system)))
		    (setf (patch-status entry) ':ABORTED)
		    ))
	      (write-patch-index patch-system)
	      (let ((open-patch (find-open-patch patch-system patch-number )))
		(when open-patch 
		  (close-patch open-patch)))
	      ;; probably should give user option to delete patch source and binary files
	      )

	    (let ((open-patch *current-patch*))
	      (when open-patch 
		(close-patch open-patch)))
	    ;; probably should give user option to delete patch source and binary files
	    )
	) ))

(defun test-system-name (file-system-name)
  ;; If file-system-name is nil, then either the file doesn't know its system,
  ;; or has never been loaded into the system.  This is ok, I guess.  We will
  ;; proceed without complaints
  (let ((system-name (patchable-system-name *current-patchable-system* )))
    (when (and system-name file-system-name)
      (unless (system-name-alias-p file-system-name system-name)
	(error (format "ADD PATCH of file claiming to be in system %s, but current patch is for system %s.\n" file-system-name system-name))))))

(defun add-patch-internal (region-p)
  (let* ((buf (current-patch-buffer)))  
    (if (not buf)
	(error "No patch is selected")
	(if (not (memq major-mode '(lisp-mode l-common-lisp-mode)))
	    (error "The current buffer is not a Lisp Mode buffer")
	    (let* ((lisp-buf (current-buffer))
		   (pkg (upcase-lisp-buffer-package))
		   ;;(rcs-id-string (get-buffer-rcs-id-string lisp-buf))
		   (vcinfo (vc-call mode-line-string buffer-file-name))
		   (pathname (lisp-filename-backtranslate buffer-file-name ))
		   ;;(patch-system-name (patchable-system-name *current-patchable-system* ))
		   (buffer-patch-system-name (buffer-patch-system-name lisp-buf))
		   (region (if region-p (list (point) (mark)) (slime-region-for-defun-at-point))))
	      (test-system-name buffer-patch-system-name)
	      (when (null pkg)
		(error (format "ADD PATCH cannot find the package of buffer %s \n" (buffer-name lisp-buf))))
	      (save-excursion
		(set-buffer buf)
		(goto-char (point-max))
		(insert (format "\n\n(IN-PACKAGE \"%s\")\n"  pkg ))
		(insert-patch-info pathname vcinfo region)
		
		(destructuring-bind (start end) region
		  (insert-buffer-substring lisp-buf start end))
		(insert (format "\n"))
		))))))

(defun insert-patch-info (pathname vcinfo region)
  (let ((pad "               "))
    (insert "(ST:PATCH-INFO ")
    (insert-keyval "" :file pathname) 
    (insert-keyval " " :region region)
    (insert "\n")
    (insert-keyval pad :time (patch-current-time-string)) 
    (insert-keyval " " :vcinfo vcinfo)
    (insert "\n")
    (insert-keyval pad :author (user-login-name))
    (insert-keyval " ":host (system-name))
    (insert ")\n\n")))

(defun add-patch-region ()
  "Add selected region to current patch buffer."
  (interactive )
  (add-patch-internal t)
  )

(defun add-patch ()
  "Add selected definition to current patch buffer."
  (interactive )
  (add-patch-internal nil) )
    
(defun finish-patch ()
  "Finish the current patch."
  (interactive)
  (if (not *current-patch*)
      (error "There is no current patch.")
      (let ((patch-system (current-patch-system))
	    (current-buf (if t (current-buffer) *start-patch-comment-previous-buffer*))
	    (patch (current-patch)) )
	(if (not patch)
	    (error (format "FINISH-PATCH cannot find patch %s in patch index of system %s\n"
			   (patch-number *current-patch*) (patchable-system-name patch-system)))
	    
	    (progn;; need to write out the patch buffer
	      (when patch-system (read-patch-index patch-system))
	      (set-buffer (current-patch-buffer))
	      (save-buffer )
	      (set-buffer current-buf)
	      (if *finish-patch-force-comment-edit*
		  (progn
		    (setq *start-patch-comment-previous-buffer* (current-buffer) )
		    (switch-to-buffer *patch-comment-buffer*)
		    ;; must do M-X end-patch-comment to complete finishing patch
		    )
		  (finalize-patch ))
	      )))))
  

(defun finalize-patch ()
  (interactive)
  (let ((patch-system (current-patch-system)))
    (when patch-system 
      (read-patch-index patch-system)
      (let* ((patch (get-patch patch-system (current-patch-number)) ))
	(setf (patch-status patch) ':FINISHED)
	(save-patch-comment patch)
	(write-patch-index patch-system)
	))

    (close-patch *current-patch*)
    (when *start-patch-comment-previous-buffer*
      (switch-to-buffer *start-patch-comment-previous-buffer*)) ))

(defun set-patch-comment-buffer (comment)
  (let ((current-buff (current-buffer))
	(comment-buf (or (and *patch-comment-buffer*
			      (buffer-name *patch-comment-buffer*))
			 (setq *patch-comment-buffer*
			       (get-buffer-create "*CURRENT-PATCH-COMMENT*")))))
    (save-excursion
      (set-buffer comment-buf)
      (erase-buffer)
      (if comment
	  (insert comment ))
      (set-buffer current-buff)
      )))

;;; Save the current contents of the *patch-comment-buffer* in the specified patch.
;;; TODO: Probably should also update comment at the beginning of the patch file.
;;; That requires some syntax convention so the old comment can be removed.
(defun save-patch-comment (patch)
  (let ((comment-buf *patch-comment-buffer*))
    (save-excursion
      (set-buffer comment-buf)
      (when (fboundp 'font-lock-unfontify-region)
	(font-lock-unfontify-region (point-min) (point-max)))
      (let ((comment (buffer-substring (point-min) (point-max))))
	(when (and (> (length comment) 0) (/= (aref comment 0) (aref "\n" 0)))
	  ;; force comments to begin with a newline
	  (setq comment (concat "\n" comment)))
	(when patch
	  (setf (patch-comment patch) comment))
	comment
	))))

(defun reset-patch-system ()
  "Reset state of patch system -- forget about previous systems and patches."
  (interactive)
  (setq *patch-systems* nil *current-patchable-system* nil
	*current-patch* nil *all-open-patches* nil))



(defun patch-eval-in-lisp (string &optional message)
  (car (read-from-string  (slime-eval `(swank:pprint-eval ,string)))))

(defun buffer-patch-system-name (buffer)
  (patch-eval-in-lisp (format "(st::patch-system-name-for-file \"%s\")" (buffer-file-name buffer))))

(defun system-name-alias-p (file-system-name patch-system-name)
  (equal file-system-name patch-system-name))

(defun get-file-license-string (filename)
  nil)

(defun get-system-license-string (filename)
  nil)

(defun get-patch-system-info (system-name)
  (let* ((result (patch-eval-in-lisp (format "(st::system-info-for-emacs \"%s\")" system-name))))
    ;; result is of form (major-version patch-directory-pathname default-package)
    (if (stringp result)
	(error result) ; we've got a problem
	result)))

(defun get-patch-system-info (patch-system-name)
  (let* ((result (patch-eval-in-lisp (format "(st::patch-system-info-for-emacs \"%s\")" patch-system-name))))
    ;; result is of form (major-version patch-directory-pathname default-package)
    (if (stringp result)
	(error result) ; we've got a problem
	result)))

(defun get-patch-system-patch-directory (patch-system-name)
  (let* ((result (patch-eval-in-lisp (format "(st::patch-system-patch-directory \"%s\")" patch-system-name))))
    ;; result is of form (major-version patch-directory-pathname default-package)
    (or result (error "get-patch-system-patch-directory failed for \"%s\"" patch-system-name))))

(defun get-patch-system-patch-directory-and-version (patch-system-name)
  (let* ((result (patch-eval-in-lisp 
		  (format "(st::patch-system-patch-directory-and-version \"%s\")" patch-system-name))))
    ;; result is of form (major-version patch-directory-pathname default-package)
    (if (consp result)
	result
	(error "get-patch-system-patch-directory-and-version failed for \"%s\"" patch-system-name))))

(defun lisp-filename-backtranslate (filename)
  (patch-eval-in-lisp (format "(st::filename-backtranslate \"%s\")" filename)))


;(setq debug-on-error t)
;(get-patch-system-info "image")
;(find-patch-system-named "image")
;(find-patch-system-named "BASIC-GUI")
;(get-patch-system-info "BASIC-GUI")
;(buffer-patch-system-name (current-buffer))
;(lisp-file-name-backtranslate "/opt/IU/FREEDIUS/default/lisp/image-defs.lisp")
;(setq *patch-systems* nil)
;(find-patch-system-named "FREEDIUS")
;(get-patch-system-patch-directory "FREEDIUS")
;(get-patch-system-patch-directory-and-version "FREEDIUS")
;(consp (list 1 2))
;(patch-eval-in-lisp (format "(st::patch-system-patch-directory-and-version \"%s\")" "FREEDIUS"))

(defun find-patch-system-named (system-name)
  (let ((l *patch-systems*))
    (while (and l (not (equal (patchable-system-name (car l)) system-name)))
      (setq l (cdr l)))
    (or (car l)
	(let ((system-info (get-patch-system-info system-name)))
	  (when system-info
	    (apply 'make-patchable-system2 system-name (hack-nil system-info)))))))

(defun find-patch-system-named (system-name)
  (let ((l *patch-systems*)
	(default-version nil))
    (while (and l (not (equal (patchable-system-name (car l)) system-name)))
      (setq l (cdr l)))
    (or (car l)
	(destructuring-bind (patch-directory system-version)
	    (get-patch-system-patch-directory-and-version system-name)
	  (when patch-directory
	    (make-patchable-system2 system-name default-version patch-directory))))))

(defun strip-trailing-slash (string)
  (if (eql (aref string (1- (length string))) ?/)
      (substring string 0 (1- (length string)))
      string))
;(strip-trailing-slash "foo/")

(defun make-patchable-system2 (system-name major-version patch-directory-pathname 
			       &optional default-package true-system-name)
  ;;(when true-system-name (setq system-name true-system-name))
  (setq patch-directory-pathname (substitute-in-file-name patch-directory-pathname))
  (let ((patch-system (make-patchable-system 
		       :name system-name 
		       :major-version major-version
		       :patch-directory-pathname (strip-trailing-slash patch-directory-pathname)
		       ;;:patch-directory-pathname patch-directory-pathname
		       :default-package default-package
		       )))
    ;;(merge-remote-patch-pathname system)
    (setq *patch-systems* (cons patch-system *patch-systems*))
    patch-system))

;(setq *patch-systems* nil)
;(make-patchable-system2 "FREEDIUS" 0 "/opt/IU/FREEDIUS/default/lisp/patches/test" "cl-user")

(defun get-buffer-rcs-id-string (buf &optional prefix)
  (unless prefix (setq prefix (concat "$" "Id:"))) ; big troubles if it looks like an rcs id
  (let* ((prev-buf (current-buffer)))
    (set-buffer buf) 
    (save-excursion 
      (goto-char (point-min))
      (let* (start-pos end-pos)
	(when (search-forward prefix nil t)
	  (setq start-pos (point)))
	(when (search-forward "$" nil t)
	  (setq end-pos (point)))
	(prog1
	  (when (and start-pos end-pos )
	    (let* ((rcs-id (buffer-substring start-pos (1- end-pos)))
		   (id-length (length rcs-id )))
	      (if (and (< id-length 100) (> id-length 1))
		  rcs-id
		  nil)))
	  (set-buffer prev-buf)
	  )))))

(defun get-file-rcs-id-string (file &optional prefix)
  (interactive "FPathname: ")
  (let* ((buf (find-file-noselect file t)))
    (get-buffer-rcs-id-string buf)))

(provide 'patch-sys)
