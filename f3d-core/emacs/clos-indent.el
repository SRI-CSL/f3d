;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1988, 1989 by Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Suggestions, comments and requests for improvements are welcome.
;;; Please send all comments to lanning.pa@xerox.com
;;; *************************************************************************
;;;
;;; Teach GnuEmacs how to indent CLOS forms.  This follows the indentation style
;;; used in the document
;;; 	Common Lisp Object System Specification
;;; that defines the CLOS additions to CommonLisp.
;;;
;;; Author: Stan Lanning (lanning.pa@xerox.com)
;;;

;;; NOTES:
;;;
;;; (0) I'm the first to admit that I'm not an experienced GnuLisp hacker.
;;; I strongly suspect that much of the code below could be done better.  If you
;;; have any comments/suggestions/improvements, please pass them along.
;;; 
;;; (1) This file is built "on top" of the library file cl-indent.  This is not
;;; the regular indentation facility (which is fine for editing GnuELisp,
;;; but is all wrong for CommonLisp).  You need to set up the lisp-indent-hook
;;; to be common-lisp-indent-hook to get this file to work.  Not that cl-indent
;;; does all that great for CommonLisp, but it's a lot better than the normal
;;; indentation.
;;; 
;;; (2) You need to have the library file cl loaded to compile this file, but
;;; you don't need it to run.

(load "cl-indent")

(put 'defclass 'common-lisp-indent-function '(2 9 4 &rest 2))

(defun backward-sexp-ignore-comments ()
  (backward-sexp))

;; A number of CLOS macros (DEFGENERIC, WITH-ADDED-METHODS) have embedded method
;; descriptions.  The function LISP-INDENT-METHOD-DESCRIPTION will indent them
;; properly. But first, some helper functions...

;; Here's a little function that we use to determine if we are looking at a
;; parameter-list.  It's not perfect (if fails when looking at a non-null
;; symbol that starts with the letters "nil") but it works in most all
;; real-world situations.
(defun looking-at-list ()
  (let ((case-fold-search t))
    (looking-at "\\s-*\\(nil\\|\\s(\\)")))


;; Is the given pos preceeded by N non-lists?
(defun preceeded-by-non-lists-p (pos n)
  (save-excursion
    (catch 'exit
      (goto-char pos)
      (let ((index n))
	(while (> index 0)
	  (backward-sexp-ignore-comments)
	  (when (looking-at-list)
	    ;; Found a list, so break out of loop and return nil
	    (throw 'exit nil))
	  (setq index (1- index))))
      ;; If we didn't throw out, return t
      t)))

(defvar method-descr-paramlist-indent 9
  "Indentation for the specialized-lambda-list in a method-description")

(defvar method-descr-body-indent 2
  "Indentation for forms in the body of a method-description.")

(defun lisp-indent-method-description
       (path state indent-point sexp-column normal-indent)
  ;;
  ;; When called, the indent-point should be inside a method description of the
  ;; form (:method {<qualifier>}* <specialized-lambda-list> {<form>}*)
  ;;
  ;; One hassle here is to figure out if there are qualifiers on the method, and
  ;; if there are how many, and how to indent them.
  ;;
  ;; Another hassle is figuring out what all these arguments mean, since none
  ;; of them are particularly well documented.  Indeed, none are documented at
  ;; all.
  ;;
  ;; Yet another hassle is figuring out how to format the returned value(s).
  ;; Seems that GnuEmacs expects the value to be in one of three formats:  a
  ;; (positive) number, a list of two positive numbers, or a list of a positive
  ;; number and a negative number.  The first case means that this and all
  ;; subsequent elements of the list have the same indent (the returned value);
  ;; the second case means that this element should be indented by the car of
  ;; the returned value, and that there is no additional information known about
  ;; the indentation of subsequent items; the third case means that this item
  ;; should be indented by the car of the returned value, and all subsequent
  ;; items should be indented by (- (cadr <returned-value>)).  How silly.
  ;;
  (let ((line (nth 0 state))		;Not that I actually use all of these,
	(form-start (nth 1 state))	; but it can't hurt to parse the
	(prev-sexp-start (nth 2 state))	; parse-state for the sake of clarity.
	(in-string-p (nth 3 state))	; I mean, maybe someday I'll try to
	(in-comment-p (nth 4 state))	; write another one of these, and this
	(quoted-p (nth 5 state))	; example might prove handly.
	(min-paren-depth (nth 6 state))
	(depth (length path))
	(elt-number (car (last path)))
	method-descr-indent)	;Zero-based
    (cond ((save-excursion
	     (goto-char indent-point)
	     (not (let ((case-fold-search t))
		    (backward-up-list 1)
		    (setq method-descr-indent (current-column))	;Side-effect...
		    (looking-at "(\\s-*:method\\s-+"))))
	   ;; If not at the top-level of the (:method ...), indent normally
	   normal-indent)
	  ((or in-comment-p in-string-p quoted-p (= elt-number 0))
	   ;; If not at a form, forget it (shouldn't happend, but why not be
	   ;; on the safe side)?
	   normal-indent)
	  ((not (preceeded-by-non-lists-p indent-point elt-number))
	   ;; There was a list prior to this form, so we must be in the body of
	   ;; the method.
	   (+ method-descr-indent method-descr-body-indent))
	  ((save-excursion
	     (goto-char indent-point)
	     (looking-at-list))
	   ;; At the specialized-lambda-list
	   (list (+ method-descr-indent method-descr-paramlist-indent)
		 (- (+ method-descr-indent method-descr-body-indent))))
	  (t
	   ;; At a qualifier
	   (list (+ method-descr-indent method-descr-paramlist-indent) 1)))))

(defvar defgeneric-option-indent 2
  "Indentation for the options & method-descriptions in a DEFGENERIC.")

(defvar defgeneric-paramlist-indent 12
  "Indentation for the parameter list of a DEFGENERIC.")

(defun lisp-indent-defgeneric
       (path state indent-point sexp-column normal-indent)
  (let ((line (nth 0 state))
	(form-start (nth 1 state))
	(prev-sexp-start (nth 2 state))
	(in-string-p (nth 3 state))
	(in-comment-p (nth 4 state))
	(quoted-p (nth 5 state))
	(min-paren-depth (nth 6 state))
	(depth (length path))
	(elt-number (car (last path))))
    (cond ((= depth 1)
	   (if (<= elt-number 2)
	       (list defgeneric-paramlist-indent 1)
	     defgeneric-option-indent))
	  ((save-excursion
	     (goto-char indent-point)
	     (let ((depth depth))
	       (while (>= depth 2)
		 (backward-up-list 1)
		 (setq depth (1- depth))))
	     (let ((case-fold-search t))
	       (looking-at "(\\s-*:method\\s-+")))
	   ;; In a method-description
	   (lisp-indent-method-description path state indent-point sexp-column
					   normal-indent))
	  (t
	   ;; Some other option
	   normal-indent))))

(put 'defgeneric 'common-lisp-indent-function 'lisp-indent-defgeneric)


;; Indentation for DEFINE-METHOD-COMBINATION is difficult because there are two
;; different forms.  The short form is easy, the long form requires some
;; parsing.

(defun lisp-indent-define-method-combination
              (path state indent-point sexp-column normal-indent)
  (let ((line (nth 0 state))
	(form-start (nth 1 state))
	(prev-sexp-start (nth 2 state))
	(in-string-p (nth 3 state))
	(in-comment-p (nth 4 state))
	(quoted-p (nth 5 state))
	(min-paren-depth (nth 6 state))
	(depth (length path))
	(elt-number (car path)))
    (cond ((not (= 1 depth))
	   ;; Not at top level of the DEFINE-METHOD-COMBINATION
	   normal-indent)
	  ((<= elt-number 1)
	   ;; The initial DEFINE-METHOD-COMBINATION or name
	   normal-indent)
	  ((save-excursion
	     (goto-char indent-point)
	     (let ((index elt-number))
	       (while (> index 2)
		 (backward-sexp-ignore-comments)
		 (setq index (1- index))))
	     (looking-at-list))
	   ;; The second (zero-based) element of the form is a list, so this is
	   ;; a long-form.
	   (lisp-indent-define-method-combination-long
	       path state indent-point sexp-column normal-indent))
	  (t
	   ;; The second element is a symbol, so this is a short form
	   (lisp-indent-define-method-combination-short
	       path state indent-point sexp-column normal-indent)))))

(defun lisp-indent-define-method-combination-short
       (path state indent-point sexp-column normal-indent)
  (let ((line (nth 0 state))
	(form-start (nth 1 state))
	(prev-sexp-start (nth 2 state))
	(in-string-p (nth 3 state))
	(in-comment-p (nth 4 state))
	(quoted-p (nth 5 state))
	(min-paren-depth (nth 6 state))
	(depth (length path))
	(elt-number (car path)))
    (cond ((evenp elt-number)
	   ;; This is a keyword position
	   (list 2 1))
	  (t
	   ;; A value position
	   (list 4 1)))))

(defvar define-method-combination-option-indent 8
  "Indentation for the options to the long form of DEFINE-METHOD-COMBINATION.")

(defvar define-method-combination-body-indent 2
  "Indentation for forms in the body of a DEFINE-METHOD-COMBINATION.")

(defun lisp-indent-define-method-combination-long
       (path state indent-point sexp-column normal-indent)
  (let ((line (nth 0 state))
	(form-start (nth 1 state))
	(prev-sexp-start (nth 2 state))
	(in-string-p (nth 3 state))
	(in-comment-p (nth 4 state))
	(quoted-p (nth 5 state))
	(min-paren-depth (nth 6 state))
	(depth (length path))
	(elt-number (car path)))
    (cond ((= elt-number 3)
	   ;; The method-group-specifiers
	   (list (+ sexp-column define-method-combination-option-indent) 1))
	  ((save-excursion
	     (goto-char indent-point)
	     (catch 'exit
	       (let ((index elt-number)
		     (case-fold-search t))
		 (cond ((<= index 3)
			;; Must be an option
			(throw 'exit t))
		       ((not (or (looking-at "\\s-*(\\s-*:arguments")
				 (looking-at "\\s-*(\\s-*:generic-function")))
			;; In the body
			(throw 'exit nil))
		       (t
			;; Try the previous one
			(backward-sexp-ignore-comments)
			(setq index (1- index)))))))
	   ;; An option
	   (list (+ sexp-column define-method-combination-option-indent) 1))
	  (t
	   ;; Must be in the body
	   (+ sexp-column define-method-combination-body-indent)))))


(put 'define-method-combination 'common-lisp-indent-function
     'lisp-indent-define-method-combination)


;; Indentation for DEFMETHOD is difficult since there are an unknown number of
;; method qualifiers.  Fortunatly, we developed a lot of useful machinery above.

(defvar defmethod-body-indent 2
  "Indentation for forms in the body of a DEFMETHOD.")

(defvar defmethod-paramlist-indent 11
  "Indentation for the parameter list of a DEFMETHOD.")

(defvar defmethod-qualifier-indent 11
  "Indentation for a method-qualifier in a DEFMETHOD.")

(defun lisp-indent-defmethod (path state indent-point sexp-column normal-indent)
  (let ((line (nth 0 state))
	(form-start (nth 1 state))
	(prev-sexp-start (nth 2 state))
	(in-string-p (nth 3 state))
	(in-comment-p (nth 4 state))
	(quoted-p (nth 5 state))
	(min-paren-depth (nth 6 state))
	(depth (length path))
	(elt-number (car path)))
    (cond ((not (= 1 depth))
	   ;;Not at top level of the DEFMETHOD
	   normal-indent)
	  ((<= elt-number 1)
	   ;;The initial DEFMETHOD or name
	   normal-indent)
	  ((not (preceeded-by-non-lists-p indent-point (- elt-number 2)))
	   ;; Must be in the body of the method
	   (+ sexp-column defmethod-body-indent))
	  ((save-excursion
	     (goto-char indent-point)
	     (looking-at-list))
	   ;; Looking at the specialized-lambda-list
	   (list (+ sexp-column defmethod-paramlist-indent)
		 (- (+ sexp-column defmethod-body-indent))))
	  (t
	   ;; At a qualifier, so we don't know about indentation for subsequent
	   ;; items.
	   (list (+ sexp-column defmethod-qualifier-indent) 1)))))

(put 'defmethod 'common-lisp-indent-function 'lisp-indent-defmethod)


(defvar generic-flet-arg-indent 4
  "Indentation for the first subform to a GENERIC-FLET")

(defvar generic-flet-body-indent 2
  "Indentation for forms in the body of a GENERIC-FLET")

(defvar generic-flet-lambda-list-indent 4
  "Indentation for the lambda-list in a gf definition in a GENERIC-FLET")

(defvar gf-spec-clause-indent 2
  "Indentation for a clause in a gf definition.")

;; This needs to be set slightly larger to accomodate the deeply nested forms of
;; GENERIC-FLET.
(setq lisp-indent-maximum-backtracking
      (if (boundp 'lisp-indent-maximum-backtracking)
	  (max 4 lisp-indent-maximum-backtracking)
	4))

(defun lisp-indent-generic-flet
       (path state indent-point sexp-column normal-indent)
  (let ((line (nth 0 state))
	(form-start (nth 1 state))
	(prev-sexp-start (nth 2 state))
	(in-string-p (nth 3 state))
	(in-comment-p (nth 4 state))
	(quoted-p (nth 5 state))
	(min-paren-depth (nth 6 state))
	(depth (length path))
	(elt-number (car (last path))))
    (cond ((and (= depth 1)
		(= elt-number 1))
	   ;; Indenting the gf-spec list
	   (list (+ sexp-column generic-flet-arg-indent)
		 (- (+ sexp-column generic-flet-body-indent))))
	  ((and (= depth 1)
		(> elt-number 1))
	   ;; One of the forms in the body
	   (+ sexp-column generic-flet-body-indent))
	  ((and (= depth 3)
		(= (car path) 1)
		(= (cadr path) 0)
		(= (caddr path) 2))
	   ;; Indenting the first clause in a gf-spec
	   (+ sexp-column gf-spec-clause-indent))
	  ((not (and (>= depth 4)
		     (= (car path) 1)))
	   ;; Not inside a gf-spec, so nothing special
	   (list normal-indent 1))
	  ((save-excursion
	     (goto-char indent-point)
	     (let ((depth depth))
	       (while (> depth 3)
		 (backward-up-list 1)
		 (setq depth (1- depth))))
	     (let ((case-fold-search t))
	       (looking-at "(\\s-*:method\\s-+")))
	   ;; In a method-description
	   (lisp-indent-method-description path state indent-point sexp-column
					   normal-indent))
	  (t
	   ;; Inside some other option
	   normal-indent))))

(put 'generic-flet 'common-lisp-indent-function 'lisp-indent-generic-flet)


(defun lisp-indent-generic-function
       (path state indent-point sexp-column normal-indent)
  (let ((line (nth 0 state))
	(form-start (nth 1 state))
	(prev-sexp-start (nth 2 state))
	(in-string-p (nth 3 state))
	(in-comment-p (nth 4 state))
	(quoted-p (nth 5 state))
	(min-paren-depth (nth 6 state))
	(depth (length path))
	(elt-number (car (last path))))
    (cond ((= depth 1)
	   (if (<= elt-number 1)
	       (list defgeneric-paramlist-indent 1)
	     defgeneric-option-indent))
	  ((save-excursion
	     (goto-char indent-point)
	     (let ((depth depth))
	       (while (>= depth 2)
		 (backward-up-list 1)
		 (setq depth (1- depth))))
	     (let ((case-fold-search t))
	       (looking-at "(\\s-*:method\\s-+")))
	   ;; In a method-description
	   (lisp-indent-method-description path state indent-point sexp-column
					   normal-indent))
	  (t
	   ;; Some other option
	   normal-indent))))

(put 'generic-function 'common-lisp-indent-function 'lisp-indent-generic-function)

(put 'generic-labels 'common-lisp-indent-function 'lisp-indent-generic-flet)

(put 'symbol-macrolet 'common-lisp-indent-function 1)

(put 'with-accessors 'common-lisp-indent-function
     '((&whole 6 &rest (&whole 1 1 2)) 4 &body))


(defvar with-added-methods-body-indent 2
  "Indentation for forms in the body of a WITH-ADDED-METHODS")

(defvar with-added-methods-arg-indent 4
  "Indentation for the first subform in a WITH-ADDED-METHODS")

(defun lisp-indent-with-added-methods
       (path state indent-point sexp-column normal-indent)
  (let ((line (nth 0 state))
	(form-start (nth 1 state))
	(prev-sexp-start (nth 2 state))
	(in-string-p (nth 3 state))
	(in-comment-p (nth 4 state))
	(quoted-p (nth 5 state))
	(min-paren-depth (nth 6 state))
	(depth (length path))
	(elt-number (car (last path))))
    (cond ((and (= depth 1)
		(= elt-number 1))
	   ;; Indenting the first subform
	   (list (+ sexp-column with-added-methods-arg-indent)
		 (- (+ sexp-column with-added-methods-body-indent))))
	  ((and (= depth 1)
		(> elt-number 1))
	   ;; One of the forms in the body
	   (+ sexp-column with-added-methods-body-indent))
	  ((and (= depth 2)
		(= (car path) 1)
		(= (cadr path) 2))
	   ;; Indenting the first clause in a gf-spec
	   (+ sexp-column gf-spec-clause-indent))
	  ((not (and (>= depth 3)
		     (= (car path) 1)))
	   ;; Not inside the spec list, so nothing special
	   (list normal-indent 1))
	  ((save-excursion
	     (goto-char indent-point)
	     (let ((depth depth))
	       (while (> depth 2)
		 (backward-up-list 1)
		 (setq depth (1- depth))))
	     (let ((case-fold-search t))
	       (looking-at "(\\s-*:method\\s-+")))
	   ;; In a method-description
	   (lisp-indent-method-description path state indent-point sexp-column
					   normal-indent))
	  (t
	   ;; Inside some other option
	   normal-indent))))

(put 'with-added-methods 'common-lisp-indent-function 'lisp-indent-with-added-methods)

(put 'with-slots 'common-lisp-indent-function
     '((&whole 6 &rest (&whole 1 1 2)) 4 &body))


;; These aren't really a part of CLOS, but they often appear in PCL programs.

(put 'defconstructor 'common-lisp-indent-function 3)

;;; This looks better than the default.  Thu Aug 11 1994 heller
(put 'defpackage 'common-lisp-indent-function 1)




;; While we're at it, let's PROVIDE a little something

(provide 'clos-indent)
