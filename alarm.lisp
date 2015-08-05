(eval-when (:compile-toplevel :load-toplevel :execute)
  (:asd :qt)
  (:asd :sb-daemon))

(defpackage alarm
  (:use :qt :cl)
  (:export encode-time
           start))

(in-package alarm)
(named-readtables:in-readtable :qt)

(defconstant +unix-offset+
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun encode-time (time)
  (when time
    (let* ((column (position #\: time))
           (hour (and column
                      (parse-integer time :end column :junk-allowed t)))
           (minute (and column
                        (parse-integer time :start (1+ column) :junk-allowed t)))
           (now (get-universal-time)))
      (multiple-value-bind (s m h date month year) (decode-universal-time now)
        (declare (ignore s m h))
        (let ((encoded (encode-universal-time 0 minute hour date month year)))
          (- (if (> encoded now)
                 encoded
                 (+ encoded 86400))
             +unix-offset+))))))


(defvar *alarm-threshold* 60)

(defun read-saved-time ()
  (with-open-file (stream "/tmp/lalarm-time")
    (+ (parse-integer (read-line stream))
       +unix-offset+)))

(defun start-p ()
  (and (probe-file "/tmp/lalarm-time")
       (< 0 (- (get-universal-time) (read-saved-time))
          *alarm-threshold*)))

(defun start ()
  (when (start-p)
    (ignore-errors (delete-file "/tmp/lalarm-time"))
    (sb-ext:run-program "killall" '("redshift") :search t)
    (with-main-window (window (#_new QDialog))
      (let ((palette (#_palette window)))
        (#_setColor palette (#_backgroundRole window)
                    (#_new QColor (#_Qt::white)))
        (#_setPalette window palette)
        (#_showFullScreen window)))))

#-swank
(sb-ext:save-lisp-and-die "lalarm"
                          :toplevel
                          (lambda ()
                            (sb-daemon:daemonize :exit-parent t)
                            (handler-case
                                (let ((arg (cadr sb-ext:*posix-argv*)))
                                  (if (equal arg "start")
                                      (alarm:start)
                                      (format t "~a~%" (alarm:encode-time arg))))
                              (error (c)
                                (format t "An error occured:~% ~s" c)
                                (sb-ext:exit :code 1))
                              (:no-error (result)
                                (sb-ext:exit :code (if (numberp result)
                                                       0
                                                       1)))))
                          :executable t
                          :compression t)
