(in-package :lisptk)

(defparameter *tk-verbose* nil)
(defvar *capture-tcl-eval-commands* nil)
(defparameter *captured-tcl-eval-commands* nil)

(defmacro capturing-tcl-script (&body body)
  `(let ((*capture-tcl-eval-commands* t)
	 (*captured-tcl-eval-commands* nil)
	 )
    (progn . ,body)
    (reverse *captured-tcl-eval-commands*)))

;;; CMD must be a legitimate TCL command string
;;;(defun tcl-eval-internal (cmd)
;;;  (if *capture-tcl-eval-commands*
;;;      (progn (push cmd *captured-tcl-eval-commands*)
;;;             cmd)
;;;      (let* ((status (tcl-global-eval *the-interpreter* cmd)))
;;;        (when *tk-verbose* (format t "tcl-eval: ~s ~s~%" cmd status))
;;;        (if (zerop (car status))
;;;            (cadr status)
;;;            (error "~a" (cdr status))))))

(defun tcl-interpreter-thread-p ()
  #-sb-thread t
  #+sb-thread (eq sb-thread::*current-thread* *the-thread*))

#+old
(defun tcl-eval-internal (cmd &optional ignore-result)
  (if (not (tcl-interpreter-thread-p))
      (error "Trying to use the TCL interpreter in the wrong thread.")
      (if *capture-tcl-eval-commands*
          (progn (push cmd *captured-tcl-eval-commands*)
                 cmd)
          (multiple-value-bind (status result)
              (tcl-global-eval *the-interpreter* cmd ignore-result)
            (when *tk-verbose* (format t "tcl-eval: ~s ~s~%" cmd status))
            (if (zerop status)
                result
                (error "The tcl interpreter reported an error: ~a" result))))))

(defun tcl-eval-internal (cmd &optional ignore-result)
  (if *capture-tcl-eval-commands*
      (progn (push cmd *captured-tcl-eval-commands*)
             cmd)
      (multiple-value-bind (status result)
          (tcl-global-eval *the-interpreter* cmd ignore-result)
        (when *tk-verbose* (format t "tcl-eval: ~s ~s~%" cmd status))
        (if (zerop status)
            result
            (error "The tcl interpreter reported an error: ~a" result)))))


;;; TCL-CMD-AND-ARGS is a TCL command represented by a Lisp list.
;;;(defun tcl-cmd (tcl-cmd-and-args &optional ignore-result)
;;;  (let ((tcl-op (car tcl-cmd-and-args))
;;;        (args (cdr tcl-cmd-and-args)))
;;;    (if (or (eq tcl-op 'proc) (equal tcl-op "proc"))
;;;        (destructuring-bind (name arglist . cmds) args
;;;          (tcl-eval-internal
;;;           (format nil "proc ~a {~{~a ~}} {~%~{ ~a~%~}}~%"
;;;                   (tcl-string name)
;;;                   (loop for arg in arglist collect (tcl-string arg))
;;;                   (capturing-tcl-script (tcl-script cmds)))
;;;           ignore-result))
;;;
;;;        (let ((cmd (tcl-string tcl-op)))
;;;          (loop for arg in args
;;;                do (setq cmd (string-append cmd " " (tcl-string arg))))
;;;          (tcl-eval-internal cmd ignore-result)))))

;;;(defun tcl-cmd (tcl-cmd-and-args &optional ignore-result)
;;;  (let ((tcl-op (car tcl-cmd-and-args))
;;;        (args (cdr tcl-cmd-and-args)))
;;;    (if (or (eq tcl-op 'proc) (equal tcl-op "proc"))
;;;        (destructuring-bind (name arglist . cmds) args
;;;          (tcl-eval-internal
;;;           (format nil "proc ~a {~{~a ~}} {~%~{ ~a~%~}}~%"
;;;                   (tcl-string name)
;;;                   (loop for arg in arglist collect (tcl-string arg))
;;;                   (capturing-tcl-script (tcl-script cmds)))
;;;           ignore-result))
;;;        (if nil
;;;            (tcl-eval-internal (tcl-string tcl-cmd-and-args) ignore-result)
;;;            (let ((string (tcl-string tcl-cmd-and-args)))
;;;              (tcl-eval-internal (substring string 1 (1- (length string))) ignore-result))))))

(defun tcl-cmd (tcl-cmd-and-args &optional ignore-result)
  (let* ((consp (consp tcl-cmd-and-args))
	 (tcl-op (and consp (car tcl-cmd-and-args)))
	 (args (and consp (cdr tcl-cmd-and-args))))
    (if (or (eq tcl-op 'proc) (equal tcl-op "proc"))
	(destructuring-bind (name arglist . cmds) args
	  (tcl-eval-internal
	   (format nil "proc ~a {~{~a ~}} {~%~{ ~a~%~}}~%"
		   (tcl-string name)
		   (loop for arg in arglist collect (tcl-string arg))
		   (capturing-tcl-script (tcl-script cmds)))
	   ignore-result))
	(if nil
	    (tcl-eval-internal (tcl-string tcl-cmd-and-args) ignore-result)
	    (let* ((string (tcl-string tcl-cmd-and-args))
		   (n (length string)))
	      (if (and (eql (aref string 0) #\[)
		       (eql (aref string (1- n)) #\]))
		  (tcl-eval-internal (substring string 1 (1- n)) ignore-result)
		  (tcl-eval-internal string ignore-result)))))))

(defun tcl-cmd-with-result (tcl-cmd-and-args)
  (tcl-cmd tcl-cmd-and-args nil))

(defun tcl-cmd-ignore-result (tcl-cmd-and-args)
  (tcl-cmd tcl-cmd-and-args t))

(defun tcl-eval (&rest tcl-cmd-and-args)
  (tcl-cmd tcl-cmd-and-args))

(defun tcl-script (cmds)
  (loop for cmd in cmds
	when (stringp cmd)
	     collect (tcl-eval-internal cmd)
	else collect (tcl-cmd cmd)))

(defun write-tcl-script (path script)
  (with-open-file (st path :direction :output)
    (loop for cmd in script
	  do (format st "~a~%" cmd))))

;;; inspector uses this
(defun tcl-eval-to-list (&rest cmd)
  (tcl-list-to-lisp (tcl-cmd cmd)))


;;; CONVERSIONS BETWEEN TCL LISTS AND LISP LISTS AND STRINGS

(defvar *lparen* #\()
(defvar *rparen* #\))
(defparameter *tcl-readtable* nil)

;;; version fed back from Franz Mon Oct 19 1998  - not yet tested
(defun get-tcl-readtable ()
  (or *tcl-readtable*
      (let ((readtable (copy-readtable nil)))
	(set-syntax-from-char #\{ *lparen* readtable ) ;unnecessary (see above)
	(set-syntax-from-char #\} *rparen* readtable )
	;;add this code
	(set-macro-character #\{ #'(lambda (s char)                ;added
				   (declare (ignore char))        ;added
				   (read-delimited-list #\} s t)) ;added
			     nil readtable)                  ;added
	(set-syntax-from-char #\# #\A readtable )
	(setf (readtable-case readtable) :preserve)
	(setq *tcl-readtable* readtable))))


;;; This is primarily used in callbacks from tcl to convert callback args to
;;; numbers and Lisp symbols.  This should work both with and without embedded
;;; curlys.
(defun tcl-list-to-lisp (string &key upcase  (package :tk))
  (when upcase (setq string (string-upcase string)))
  (let ((*readtable* (get-tcl-readtable))
        (*package* (find-package package)))
    (loop with pos = 0
          with eof = (list nil)
          with elem
          do (multiple-value-setq (elem pos)
               (read-from-string string nil eof :start pos))
          until (eql elem eof)
          collect elem
          )))

;;;(defun map-tree (tree fn)
;;;  (if (consp tree)
;;;      (loop for thing in tree
;;;            collect (map-tree thing fn))
;;;      (funcall fn tree)))

;;; This really SUCKS: Conses a symbol for each word in string,
;;; then converts the symbols to strings.
;;;(defun tcl-list-to-strings (string )
;;;  (map-tree (tcl-list-to-lisp string) 'to-string))


(defun tcl-list-to-strings-int (list depth)
  (if (consp list)
      (if (<= depth 0)
	  (format nil "~a~{ ~a~}" (car list) (cdr list))
	  (loop for elem in list
		collect (tcl-list-to-strings-int elem (1- depth))))
      (format nil "~a" list)))
	  

;;; This really SUCKS: Conses a symbol for each word in string,
;;; then converts the symbols to strings.
;;;(defun tcl-list-to-strings (string &optional (depth 1))
;;;  (tcl-list-to-strings-int (tcl-list-to-lisp string) depth))

;;; Better version that does no symbol consing.
(defun tcl-list-to-strings (string &optional (depth 1))
  (and (> (length string) 0)
       (tcl-list-to-strings-int (tcl-list-to-toplevel-strings string 999) depth)))

#|
(TCL-LIST-TO-LISP "image-orientation {Rot 270}")
(TCL-LIST-to-LISP "image-orientation")
(TCL-LIST-to-LISP "image-orientation (Rot 270)")

(tcl-list-to-strings "foo bar")
(tcl-list-to-strings "{{All Roads} Buildings} qListbox" 2)
(tcl-list-to-lisp "{{ } Buildings} qListbox")
(tcl-list-to-lisp "{{All Roads} Buildings} qListbox")
(let ((list (tcl-list-to-lisp "All Roads")))
  (format nil "~a~{ ~a~}" (car list) (cdr list)))
      
|#

;;; TCL sucks! toplevel items with no whitespace are NOT enclosed in curlys {}
;;; toplevel items with whitespace are enclosed in curlys{}
;;; We must process this string, counting left and right curlys.
;;; Also cannot distinguish a list of strings from a string with embedded whitespace.

;;;(defun tcl-list-to-toplevel-strings (string)
;;;  (loop with depth = 0
;;;        with substrs
;;;        with substr-pos 
;;;        for pos from 0 below (length string)
;;;        for ch = (aref string pos)
;;;        
;;;        when (char= ch #\{)
;;;          do (when (= depth 0)
;;;               (setq substr-pos (1+ pos)))
;;;             (incf depth)
;;;
;;;        else when (char= ch #\})
;;;               do (decf depth)
;;;                  (when (= depth 0)
;;;                    (push (subseq string substr-pos pos) substrs)
;;;                    (setq substr-pos nil))
;;;
;;;        else when (= depth 0)
;;;               do (cond ((char= ch #\space)
;;;                         ;; end of a toplevel substr
;;;                         (when substr-pos
;;;                           (push (subseq string substr-pos pos) substrs)
;;;                           ;; do not yet know the next substr start pos
;;;                           (setq substr-pos nil)))
;;;                        ((null substr-pos)
;;;                         ;; this is start on new substring
;;;                         (setq substr-pos pos))
;;;                        ;; otherwise scan on
;;;                        )
;;;        finally (when substr-pos
;;;                  (push (subseq string substr-pos pos) substrs))
;;;                (return (nreverse substrs))))


(defun tcl-list-to-toplevel-strings (string &optional (max-depth 1))
  (if (<= max-depth 0)
      string
      (loop with depth = 0
	    with substrs
	    with substr-pos 
	    for pos from 0 below (length string)
	    for ch = (aref string pos)
	
	    when (char= ch #\{)
	      do (when (= depth 0)
		   (setq substr-pos (1+ pos)))
		 (incf depth)

	    else when (char= ch #\})
		   do (decf depth)
		      (when (= depth 0)
			(push (tcl-list-to-toplevel-strings
			       (subseq string substr-pos pos)
			       (1- max-depth))
			      substrs)
			(setq substr-pos nil))

	    else when (= depth 0)
		   do (cond ((char= ch #\space)
			     ;; end of a toplevel substr
			     (when substr-pos
			       (push (subseq string substr-pos pos) substrs)
			       ;; do not yet know the next substr start pos
			       (setq substr-pos nil)))
			    ((null substr-pos)
			     ;; this is start on new substring
			     (setq substr-pos pos))
			    ;; otherwise scan on
			    )
	    finally (when substr-pos
		      (push (subseq string substr-pos pos) substrs))
		    (return (nreverse substrs)))))

	
;;; Unused
;;;(defun lisp-to-tcl-list (list)
;;;  (string-append "[list "
;;;                 (loop with s = ""
;;;                       for elem in list
;;;                       for ss = (if (consp elem)
;;;                                     (lisp-to-tcl-list elem)
;;;                                     (tcl-string elem))
;;;                       do (setq s (string-append s ss" "))
;;;                       finally (return s)
;;;                       )
;;;                 "]"))

#|
;;; this is only called from these tests
(defun tcl-list-to-strings2 (string &optional (max-depth 1))
  (if (= max-depth 0)
      string
      (loop for str in (tcl-list-to-toplevel-strings string)
	    for strs2 = (tcl-list-to-strings2 str (1- max-depth))
	    collect (if (equal strs2 (list str))
			str
			strs2))))
(lisp-to-tcl-list '("foo" "bar"))
(tcl-list-to-lisp "foo")
(tcl-list-to-lisp "{foo bar}")
(tcl-list-to-lisp "{foo bar} 1 {2 3}")
(tcl-list-to-lisp (tcl-eval "." "configure"))
(tcl-list-to-lisp (tcl-eval "winfo" "children" ".top1.pane2.buttons"  ))
(tcl-list-to-toplevel-strings "{mumble  bar} qEntry")
(setq str "{mumble {foo   garp} bar} qEntry")
(tcl-list-to-toplevel-strings str)
(tcl-list-to-strings2 str)
(tcl-list-to-strings2 str 2)
(tcl-list-to-strings2 str 3)
(tcl-list-to-toplevel-strings "mumble {foo   garp} bar")
|#

;;; *****************   TCL-STRING  *****************

;;; TCL-MAYBE-BRACE-STRING puts braces around a string containing whitespace so
;;; that the string is preserved as a unit.
;;; TCL REALLY SUCKS: Strings with embedded whitespace must be made into a list.
;;; Hence, cannot really distinguish the string "A B" from the list of strings
;;; ("A" "B")

;;; used internal to this file 
(defun tcl-maybe-brace-string (string)
  (let ((n (length string)))
    (cond ((= n 0) "{}")
	  ((and (string-contains-whitespace string)
		(not (and (eql (aref string 0) #\{)
			  (eql (aref string (1- n)) #\}))))
	   ;; allegro has problems with this next -- incorrectly enforces
	   ;; *print-right-margin* when string contains newlines.
	   ;;(format nil "{~a}" string)
	   (lx:string-append "{" string "}"))
	  (t string))))

#|
(equal (tcl-string "A B" nil) (tcl-string '("A"  "B") nil))
(tcl-string "A B" nil)
(tcl-string '("A"  "B") nil)
|#

(defvar *left-square-bracket* (read-from-string "["))
(defvar *left-curly-bracket* (read-from-string "{"))

;;; used internal to this file 
(defun keyword-dash-symbol (key)
  (format nil "-~a" (string-downcase (symbol-name key))))

(defmethod tcl-constant (x)
  (tcl-string x nil))

(defmethod tcl-string (x &optional (tcl-code t))
  (cond ((stringp x) (tcl-maybe-brace-string x))
	;;((eql x nil) (if tcl-code "0" "{}"))
	;;((eql x t)   (if tcl-code "1" "t"))
	((eql x nil) (if tcl-code "0" "nil"))
	((eql x t)   (if tcl-code "1" "t"))
	((keywordp x) (keyword-dash-symbol x))
	((symbolp x)
	 (let ((str (symbol-name x)))
	   (cond ((or;;(not tcl-code)
		   (and (> (length str) 0) (eql (aref str 0) #\{)))
		  str)
		 ;;((eql (aref str 0) #\-) (string-downcase str))
		 ;; this expects widget names to be symbols, not strings
		 (t (cond ((equal str (string-upcase str)) ; all caps -> lowercase
			   ;; This allows tcl ops to be specified by Lisp symobls
			   (string-downcase str)) 
			  (t str)))	; mixed case -- leave alone
		 )))
	((numberp x) (to-string x))
	;;((tk-widget? x) (tk-widget-pathname x))
	((consp x)
	 ;;(format t "tcl-string ~a~%"  x)
	 (cond ((or (eq (car x) *left-square-bracket*)
		    (eq (car x) *left-curly-bracket*))
		;; *****************************************************
		;; THIS LOOKS WRONG -- RETURNS A LIST RATHER THAN A STRING
		;; *****************************************************
		(error "tcl-string: THIS LOOKS WRONG")
		(loop for arg in x collect (tcl-string arg tcl-code)))
	       (tcl-code
		(if (eq (car x) 'quote)
		    ;; Hack to refer to tcl-lists (with curly brackets) from tcl code.
		    ;; No longer need to explicitly generate [list [list ...]]
		    (tcl-string (cadr x) nil)
		    
		    ;; In tcl code, nested lists are assumed to be tcl commands,
		    ;; thus they must be surrounded by square brackets.
		    (format nil "[~{~a ~}]"
			    (loop for arg in x collect (tcl-string arg tcl-code)))))
	       (t
		;; In tcl constants, nested lists are assumed to be tcl lists,
		;; thus they are surrounded by curly brackets.
		(format nil "{~{~a ~}}"
			(loop for arg in x collect (tcl-string arg tcl-code))))
	       ))
	(t (error "only strings, symbols, numbers, keywords or widgets, please."))))


#|
(setq *tk-verbose* t)
(tcl-cmd '(puts (list (list quit-panel "Quit Panel")
		  (list update-panel "Update")
		  (list qtogglebutton lock :text "Lock"))))
(tcl-cmd '(list (list quit-panel "Quit")
		  (list update-panel "Update")
		  (list qtogglebutton lock :text "Lock")))

(tcl-cmd '(puts '((quit-panel "Quit Panel")
		  (update-panel "Update")
		  (qtogglebutton lock :text "Lock"))))

(tcl-string ''((quit-panel "Quit")
	       (update-panel "Update")
	       (qtogglebutton lock :text "Lock")))
"{{quit-panel Quit } {update-panel Update } {qtogglebutton lock -text Lock } }"
|#
