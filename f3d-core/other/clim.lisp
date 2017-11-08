(in-package :clim-listener)

;;;
;;; Places where we might insert tk::do-events
;;;

#+never
(defmethod stream-present :around 
    ((stream listener-interactor-pane) object type
     &rest args &key (single-box nil sbp) &allow-other-keys)
  (declare (ignore single-box sbp))
  (apply #'call-next-method stream object type :single-box t args)
  ;; we would do this, but CLIM:PRESENT calls STREAM-PRESENT with all
  ;; the keyword arguments explicitly.  *sigh*.
  #+nil 
  (if sbp
      (call-next-method)
      (apply #'call-next-method stream object type :single-box t args)))

(in-package :clim-internals)

(defmethod default-frame-top-level
    ((frame application-frame)
     &key (command-parser 'command-line-command-parser)
          (command-unparser 'command-line-command-unparser)
          (partial-command-parser
           'command-line-read-remaining-arguments-for-partial-command)
          (prompt "Command: "))
  ;; Give each pane a fresh start first time through.
  (let ((first-time t))
    (loop
       ;; The variables are rebound each time through the loop because the
       ;; values of frame-standard-input et al. might be changed by a command.
       (let* ((*standard-input*  (or (frame-standard-input frame)
                                     *standard-input*))
              (*standard-output* (or (frame-standard-output frame)
                                     *standard-output*))
              (query-io  (frame-query-io frame))
              (*query-io* (or query-io *query-io*))
              (*pointer-documentation-output*
               (frame-pointer-documentation-output frame))
              ;; during development, don't alter *error-output*
              ;; (*error-output* (frame-error-output frame))
              (*command-parser* command-parser)
              (*command-unparser* command-unparser)
              (*partial-command-parser* partial-command-parser)
              (interactorp (typep *query-io* 'interactor-pane)))
         (restart-case
             (progn
               (redisplay-frame-panes frame :force-p first-time)
               (setq first-time nil)
               (if query-io
                   ;; For frames with an interactor:
                   (progn
                     ;; Hide cursor, so we don't need to toggle it during
                     ;; command output.
                     (setf (cursor-visibility (stream-text-cursor *query-io*))
                           nil)
                     (when (and prompt interactorp)
                       (with-text-style (*query-io* +default-prompt-style+)
                         (if (stringp prompt)
                             (write-string prompt *query-io*)
                             (funcall prompt *query-io* frame))
                         (force-output *query-io*)))
                     (let ((command (read-frame-command frame
                                                        :stream *query-io*)))
                       (when interactorp
                         (fresh-line *query-io*))
                       (when command
                         (execute-frame-command frame command))
                       (when interactorp
                         (fresh-line *query-io*))))
                   ;; Frames without an interactor:
                   (let ((command (read-frame-command frame :stream nil)))
                     (when command (execute-frame-command frame command)))))
           (abort ()
             :report "Return to application command loop"
             (if interactorp
                 (format *query-io* "~&Command aborted.~&")
                 (beep))))))))


#+never
(defmethod read-frame-command :around ((frame application-frame)
				       &key (stream *standard-input*))
  ;; This is not called frequently enough:
  (tk::do-events)
  (with-input-context ('menu-item)
      (object)
      (call-next-method)
    (menu-item
     (let ((command (command-menu-item-value object))
           (table (frame-command-table frame)))
       (unless (listp command)
	 (setq command (partial-command-from-name command table)))
       (if (partial-command-p command)
	   (funcall *partial-command-parser* table stream command 0)
	   command)))))