;;; This is a file of useful hacks to SLIME.

(defun slime-eval-print-last-expression-without-output  (string)
  "Evaluate sexp before point; print value into the current buffer"
  (interactive (list (slime-last-expression)))
  (insert "\n")
  (slime-eval-print-without-output string))

(defun slime-eval-print-without-output (string)
  "Eval STRING in Lisp; insert any output and the result at point."
  (slime-eval-async `(swank:eval-and-grab-output ,string)
                    (lexical-let ((buffer (current-buffer)))
                      (lambda (result)
                        (with-current-buffer buffer
                          (destructuring-bind (output value) result
			    ;; ignore output
                            (insert value)))))))

(slime-define-keys slime-mode-map
  ("\C-j" 'slime-eval-print-last-expression-without-output))

(when nil

;;; Sun Nov 14 2004 LHQ: I do not remember why this code was here.  Perhaps to
;;; fix a SLIME problem.  SLIME's CVS version of this appears to be ok now.

(defvar slime-highlight-suppressed-forms t
  "*If true then highlight reader conditionalized forms where the test
evaluates to false.")

(defun slime-search-suppressed-forms (limit)
  "Find reader conditionalized forms where the test is false."
  (when (and slime-highlight-suppressed-forms
             (slime-connected-p)
	     (re-search-forward "#[-+]" limit t))
    (ignore-errors
      (let* ((char (char-before))
             (e (read (current-buffer)))
             (val (slime-eval-feature-conditional e)))
        (when (<= (point) limit)
          (if (or (and (eq char ?+) (not val))
                  (and (eq char ?-) val))
              (let ((start (point)))
                (forward-sexp)
                (assert (<= (point) limit))
                (let ((md (match-data)))
                  (fill md nil)
                  (setf (first md) start)
                  (setf (second md) (point))
                  (set-match-data md)
                  t))
            (slime-search-suppressed-forms limit)))))))

(defun slime-activate-font-lock-magic ()
  (font-lock-add-keywords
   'lisp-mode
   '((slime-search-suppressed-forms 0 font-lock-comment-face t))))

) ; end when


(defvar cme-lisp-mode-init-done nil)

(defun cme-lisp-mode-init ()
  (unless cme-lisp-mode-init-done
    (require 'lisp-mode)

    (require 'cl)			; ELISP common-lisp extensions

    ;; **************************  COMMON LISP AND CLOS INDENTATION **************************
    (setq lisp-indent-function 'common-lisp-indent-function)
    ;;(load "cl-indent")
    (require 'clos-indent) ; loads cl-indent
    (require 'loop-indent)
 
    ;; In recent EMACS distributions, and the SLIME distributions, common-lisp-indent-function
    ;; in cl-indent.el has been changed to use a BRAINDEAD loop indenter.
    ;; This fixes the problem, but must be loaded after cl-indent.el is loaded.
    (if (fboundp 'common-lisp-indent-function-1)
	(defun common-lisp-indent-function (indent-point state)
	  (common-lisp-indent-function-1 indent-point state)))
;; use m-x slime-update-indentation 
    (setq slime-conservative-indentation nil) ; no, do it smartly 
    (setq cme-lisp-mode-init-done t)))

;;; why isn't this a setq?
(defvar slime-inhibit-pipelining nil
  "*If true, don't send background requests if Lisp is already busy.")

(add-hook 'lisp-mode-hook 'cme-lisp-mode-init)



;;; **************************  patches for patches **************************
;;;
;;; This code works with the patch code in system-tool to apply
;;; patches in an organized way, similar in style to the lispm patch
;;; facility... -CC 3/7/2007
;;;

(defun patch-string ()
  (apply #'buffer-substring-no-properties
	 (slime-region-for-defun-at-point)))
    

(defun add-patch ()
  (interactive)
  (slime-eval
   `(st::slime-add-patch
     ,(slime-current-package)
     ,(slime-read-from-minibuffer "Comment: ")
     ,(patch-string)
     ,(buffer-file-name)
     )))


(defun abort-patches ()
  (interactive)
  (slime-eval
   `(st::abort-patches
     ,(buffer-file-name)
     )))

(defun finish-patches ()
  (interactive)
  (slime-eval
   `(st::finish-patches ,(buffer-file-name))))


(provide 'slime-hacks)



