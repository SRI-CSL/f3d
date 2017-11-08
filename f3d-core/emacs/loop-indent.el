;;; from emacs-tmc/cl-indent-patches.el

(defun cl-indent-parse-state-depth (parse-state)
  (car parse-state))

(defun cl-indent-parse-state-start (parse-state)
  (car (cdr parse-state)))

(defun cl-indent-parse-state-prev (parse-state)
  (car (cdr (cdr parse-state))))

;; Regexps matching various varieties of loop macro keyword ...
(defvar cl-indent-body-introducing-loop-macro-keyword
  "do\\|finally\\|initially"
  "Regexp matching loop macro keywords which introduce body-forms")

;; This is so "and when" and "else when" get handled right
;; (not to mention "else do" !!!)
(defvar cl-indent-prefix-loop-macro-keyword
  "and\\|else"
  "Regexp matching loop macro keywords which are prefixes")

(defvar cl-indent-clause-joining-loop-macro-keyword
  "and"
  "Regexp matching 'and', and anything else there ever comes to be
like it ...")

;; This is handled right, but it's incomplete ...
;; (It could probably get arbitrarily long if I did *every* iteration-path)
(defvar cl-indent-indented-loop-macro-keyword
  "into\\|by\\|upto\\|downto\\|above\\|below\\|on\\|being\\|=\\|first\\|then\\|from\\|to"
  "Regexp matching keywords introducing loop subclauses.  Always indented two")

(defvar cl-indent-indenting-loop-macro-keyword
  "when\\|unless\\|if"
  "Regexp matching keywords introducing conditional clauses.
Cause subsequent clauses to be indented")

(defvar cl-indent-loop-macro-else-keyword "else")

;;; Attempt to indent the loop macro ...

(defun cl-indent-indent-loop-macro
    (path parse-state indent-point sexp-column normal-indent)
  (list (cl-indent-indent-loop-macro-1 parse-state indent-point)
	(cl-indent-parse-state-start parse-state)))

(defun cl-indent-indent-loop-macro-1 (parse-state indent-point)
  (catch 'return-indentation
    (save-excursion

      ;; Find first clause of loop macro, and use it to establish
      ;; base column for indentation
      
      (goto-char (cl-indent-parse-state-start parse-state))
      (let ((loop-start-column (current-column)))
	(cl-indent-loop-advance-past-keyword-on-line)
	(if (eolp)
	    (progn
	      (forward-line 1)
	      (end-of-line)
	      
	      ;; If indenting first line after "(loop <newline>"
	      ;; cop out ...
	      
	      (if (<= indent-point (point))
		  (throw 'return-indentation (+ 2 loop-start-column)))
	      (back-to-indentation)))
	
	(let* ((case-fold-search t)
	       (loop-macro-first-clause (point))
	       (previous-expression-start (cl-indent-parse-state-prev parse-state))
	       (default-value (current-column))
	       (loop-body-p nil)
	       (loop-body-indentation nil)
	       (indented-clause-indentation (+ 2 default-value)))
	  ;; Determine context of this loop clause, starting with the
	  ;; expression immediately preceding the line we're trying to indent
	  (goto-char previous-expression-start)

	  ;; Handle a body-introducing-clause which ends a line specially.
	  (if (looking-at cl-indent-body-introducing-loop-macro-keyword)
	      (let ((keyword-position (current-column)))
		(setq loop-body-p t)
		(setq loop-body-indentation
		      (if (cl-indent-loop-advance-past-keyword-on-line)
			  (current-column)
			(back-to-indentation)
			(if (/= (current-column) keyword-position)
			    (+ 2 (current-column))
			  (- keyword-position 3)))))
	      
	    (back-to-indentation)
	    (if (< (point) loop-macro-first-clause)
		(goto-char loop-macro-first-clause))

	    ;; If there's an "and" or "else," advance over it.
	    ;; If it is alone on the line, the next "cond" will treat it
	    ;; as if there were a "when" and indent under it ...
	    (let ((exit nil))
	      (while (and (null exit)
			  (looking-at cl-indent-prefix-loop-macro-keyword))
		(if (null (cl-indent-loop-advance-past-keyword-on-line))
		    (progn (setq exit t)
			   (back-to-indentation)))))

	    ;; Found start of loop clause preceding the one we're trying to indent.
	    ;; Glean context ...
	    (cond
	      ((looking-at "(")
	       ;; We're in the middle of a clause body ...
	       (setq loop-body-p t)
	       (setq loop-body-indentation (current-column)))
	      ((looking-at cl-indent-body-introducing-loop-macro-keyword)
	       (setq loop-body-p t)
	       ;; Know there's something else on the line (or would
	       ;; have been caught above)
	       (cl-indent-loop-advance-past-keyword-on-line)
	       (setq loop-body-indentation (current-column)))
	      (t
	       (setq loop-body-p nil)
	       (if (or (looking-at cl-indent-indenting-loop-macro-keyword)
		       (looking-at cl-indent-prefix-loop-macro-keyword))
		   (setq default-value (+ 2 (current-column))))
	       (setq indented-clause-indentation (+ 2 (current-column)))
	       ;; We still need loop-body-indentation for "syntax errors" ...
	       (goto-char previous-expression-start)
	       (setq loop-body-indentation (current-column)))))
      
	;; Go to first non-blank character of the line we're trying to indent.
	;; (if none, wind up poised on the new-line ...)
	(goto-char indent-point)
	(back-to-indentation)
	(cond
	  ((looking-at "(")
	   ;; Clause body ... 
	   loop-body-indentation)
	  ((or (eolp) (looking-at ";"))
	   ;; Blank line.  If body-p, indent as body, else indent as
	   ;; vanilla clause.
	   (if loop-body-p
	       loop-body-indentation
	     default-value))
	  ((looking-at cl-indent-indented-loop-macro-keyword)
	   indented-clause-indentation)
	  ((looking-at cl-indent-clause-joining-loop-macro-keyword)
	   (let ((stolen-indent-column nil))
	     (forward-line -1)
	     (while (and (null stolen-indent-column)
			 (> (point) loop-macro-first-clause))
	       (back-to-indentation)
	       (if (and (< (current-column) loop-body-indentation)
			(looking-at "\\sw"))
		   (progn
		     (if (looking-at cl-indent-loop-macro-else-keyword)
			 (cl-indent-loop-advance-past-keyword-on-line))
		     (setq stolen-indent-column
			   (current-column)))
		 (forward-line -1)))
	     (if stolen-indent-column
		 stolen-indent-column
	       default-value)))
	  (t default-value)))))))

(defun cl-indent-loop-advance-past-keyword-on-line ()
  (forward-word 1)
  (while (and (looking-at "\\s-") (not (eolp)))
    (forward-char 1))
  (if (eolp)
      nil
    (current-column)))


(put 'loop 'common-lisp-indent-function  'cl-indent-indent-loop-macro )

(provide 'loop-indent)
