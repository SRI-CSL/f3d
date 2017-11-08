(in-package :lx)


(defvar *default-timezone* nil)

(defun decode-universal-time2 (universal-time &optional (time-zone *default-timezone* ))
  (if time-zone
      (decode-universal-time universal-time time-zone)
      (decode-universal-time universal-time )))

(defun encode-universal-time2 (sec min hr day mon yr &optional (time-zone *default-timezone* ))
  (if time-zone
      (encode-universal-time sec min hr day mon yr time-zone)
      (encode-universal-time sec min hr day mon yr )))

;;; PRINTED-UNIVERSAL-TIME FORMAT:    month/day/year hh:mm:ss
;;; where month and day are 1 or 2 digit numbers, year is a 4 digit number,
;;; and hh mm ss are 2 digit numbers
;;; Example "7/2/95 10:22:26"

(defun print-universal-time (universal-time &optional (stream *standard-output*)
			     (time-zone *default-timezone* ))
  (mv-bind (sec min hour day month year day-of-week daylight-savings-p time-zone)
      (decode-universal-time2 universal-time time-zone)
    (ignore day-of-week daylight-savings-p time-zone)
    ;(format stream "~d/~d/~d ~2,48d/~2,48d/~2,48d" month day year hour min sec)
    ;(format stream "~d/~d/~d ~2,vd:~2,vd:~2,vd~a" month day (- year 1900) #\0 hour #\0 min #\0 sec (if time-zone " DST" ""))
    (format stream "~d/~d/~d ~2,vd:~2,vd:~2,vd~a" month day year #\0 hour #\0 min #\0 sec
	    (if daylight-savings-p " DST" ""))
    ))

;;; Need to look at TIMEZONE-PARSE-DATE in $ELISP/timezone.el.
(defun parse-printed-universal-time (time-string)
  (let ((pos 0))
    (flet ((get-next ()
	     (let ((pos2 (string-search-set '(#\space #\/) time-string :start pos)))
	       (prog1 (substring time-string pos pos2)
		 (setq pos (1+ pos2))))))
	       
      (let* ((month (get-next))
	     (day (get-next))
	     (year (get-next))
	     (time-of-day (substring time-string pos))
	     (daytime (substring time-of-day 0 8)) 
	     (hour (substring daytime 0 2))
	     (min (substring daytime 3 5))
	     (sec (substring daytime 6 8))
	     )
	(encode-universal-time2 (read-from-string sec)
			       (read-from-string min)
			       (read-from-string hour)
			       (read-from-string day)
			       (read-from-string month)
			       (read-from-string year)
			       )))))

(defvar *month-vector*
    (vector "January" "February" "March" "April" "May" "June" "July"
	    "August" "September" "October" "November" "December"))

(defvar *day-of-week-vector* (vector "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defun print-universal-date (universal-time &optional (stream *standard-output*))
  (mv-bind (sec min hour day month year day-of-week daylight-savings-p time-zone)
      (decode-universal-time2 universal-time )
    (ignore day-of-week daylight-savings-p time-zone)
    (format stream "~a the ~:R of ~a, ~d; ~d:~2,vd:~2,vd ~a~a"
	    (aref *day-of-week-vector* day-of-week)
	    day
	    (aref *month-vector* (1- month))
	    year
	    (mod hour 12) #\0 min #\0 sec
	    (if (>= hour 12) "pm" "am")
	    (if daylight-savings-p " DST" "")
	    )))


(defvar *abbrev-month-vector*
    (vector "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
	    "Aug" "Sep" "Oct" "Nov" "Dec"))

(defvar *abbrev-day-of-week-vector* (vector "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

;;; This is compatible with better-get-current-time in lisp/patch-sys.el.
(defun abbrev-print-universal-date (universal-time &optional (stream *standard-output*) date-only )
  (mv-bind (sec min hour day month year day-of-week daylight-savings-p time-zone)
      (decode-universal-time2 universal-time)
    (ignore day-of-week daylight-savings-p time-zone)
    (if date-only
        (format stream "~a ~d ~d"
                (aref *abbrev-month-vector* (1- month))
                day
                year
                )
        (format stream "~a ~a ~2d ~d ~2,vd:~2,vd:~2,vd"
                (aref *abbrev-day-of-week-vector* day-of-week)
                (aref *abbrev-month-vector* (1- month))
                day
                year
                #\0 hour #\0 min #\0 sec
                ))))

(defun print-yyyymmdd-universal-time (universal-time &optional stream)
  (mv-bind (sec min hour day month year day-of-week daylight-savings-p time-zone)
      (decode-universal-time2 universal-time)
    (ignore sec min hour day-of-week daylight-savings-p time-zone)
    (format stream "~4d~2,'0d~2,'0d" year month day)))



;;; time-string of form [day-name] month-name day-number year-number [hh:mm:ss]
(defun parse-abbrev-timestring (time-string)
  (let ((pos 0))
    (flet ((get-next ()
	     (if (>= pos (length time-string))
		 nil
		 (let* ((pos1 (or (position #\space time-string :test-not #'eql :start pos) pos))
			(pos2 (or (position #\space time-string :start pos1) (length time-string)))
			(item (substring time-string pos1 pos2)))
		   (setq pos (1+ pos2))
		   item))))
	       
      (let* ((day-name (get-next))			   
	     (month-name (if (find day-name *abbrev-day-of-week-vector* :test 'equal)
			     (get-next)
			     (prog1 day-name (setq day-name nil))))
	     (day-of-month (get-next))
	     (year-string (get-next))
	     time-of-day daytime hour min sec)
	(when (>= (length time-string) (+ pos 8))
	  (setq time-of-day (substring time-string pos)
		daytime (substring time-of-day 0 8)
		hour (substring daytime 0 2)
		min (substring daytime 3 5)
		sec (substring daytime 6 8)
		))
	(ignore day-name)
	(encode-universal-time2 (if sec (read-from-string sec) 0)
				(if min (read-from-string min) 0)
				(if hour (read-from-string hour) 0)
				(read-from-string day-of-month)
				(loop for i from 0 below 12
				      when (equal (aref *abbrev-month-vector* i) month-name)
					return (1+ i))
				(read-from-string year-string))))))

;;; timestring of form yy/mm/dd hh:mm:ss
(defun parse-abbrev-timestring2 (time-string)
  (let* ((year-string  (substring time-string 0 2))
	 (month-num (substring time-string 3 5))
	 (day-of-month (substring time-string 6 8))
	 (hour (substring time-string 9 11))
	 (min (substring time-string 12 14))
	 (sec (substring time-string 15 17))
	 )
    (encode-universal-time2 (read-from-string sec)
			   (read-from-string min)
			   (read-from-string hour )
			   (read-from-string day-of-month)
			   (read-from-string month-num)
			   (read-from-string year-string))))

