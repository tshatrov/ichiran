(defpackage :ichiran/web
  (:use :cl :ichiran/all :hunchentoot)
  (:export :start-server :stop-server))

(in-package :ichiran/web)

(defvar *server* nil)
(defvar *default-port* 8080)
(defparameter *max-concurrent-requests* 10)
(defparameter *request-semaphore* (sb-thread:make-semaphore :count *max-concurrent-requests*))
(defparameter *server-ready* nil)

(defclass ichiran-acceptor (easy-acceptor)
  ((cache :initform (make-hash-table :test 'equal) :accessor acceptor-cache)
   (connection-spec :initarg :connection-spec :accessor connection-spec)))

(defmethod initialize-instance :after ((acceptor ichiran-acceptor) &key)
  (setf (hunchentoot:acceptor-input-chunking-p acceptor) t))

(defun set-cors-headers ()
  (setf (header-out "Access-Control-Allow-Origin") "*")
  (setf (header-out "Access-Control-Allow-Methods") "GET, POST, OPTIONS")
  (setf (header-out "Access-Control-Allow-Headers") "Content-Type")
  (setf (header-out "Content-Type") "application/json; charset=utf-8"))

(defun json-response (data)
  (set-cors-headers)
  (jsown:to-json data))

(defun format-word-info (romaji info)
  (jsown:new-js
    ("romaji" romaji)
    ("words" (loop for (word . gloss) in info
                   collect (jsown:new-js
                            ("word" word)
                            ("gloss" gloss))))))

(defmethod jsown:to-json ((word-info ichiran/dict:word-info))
  (jsown:to-json (ichiran/dict:word-info-gloss-json word-info)))

(defmacro with-thread-connection (&body body)
  `(progn
     (sb-thread:wait-on-semaphore *request-semaphore*)
     (unwind-protect
         (let ((ichiran/conn:*connection* (connection-spec *acceptor*)))
           (handler-case
               (postmodern:with-connection 
                   ichiran/conn:*connection*
                 ,@body)
             (cl-postgres:database-connection-error (e)
               (format t "~&Database connection error: ~A~%" e)
               (setf (hunchentoot:return-code*) 503)
               (json-response 
                (jsown:new-js
                  ("error" "Database connection error")
                  ("message" (princ-to-string e)))))
             (error (e)
               (format t "~&Error in request: ~A~%" e)
               (setf (hunchentoot:return-code*) 500)
               (json-response 
                (jsown:new-js
                  ("error" "Internal server error")
                  ("message" (princ-to-string e)))))))
       (sb-thread:signal-semaphore *request-semaphore*))))

(define-easy-handler (analyze :uri "/api/analyze") (text info full)
  (with-thread-connection
    (json-response 
      (when text
        (cond
          (full
           (let* ((limit-value 1)
                  (result (romanize* text :limit limit-value)))
             result))
          (info
           (multiple-value-bind (romaji info) 
               (romanize text :with-info t)
             (format-word-info romaji info)))
          (t
           (jsown:new-js
             ("romaji" (romanize text)))))))))

(define-easy-handler (word-info :uri "/api/word-info") (text reading)
  (json-response
    (when text
      (find-word-info-json text :reading reading))))

(defun health-check ()
  (setf (hunchentoot:content-type*) "application/json")
  (if *server-ready*
      "{\"status\": \"ok\"}"
      (progn
        (setf (hunchentoot:return-code*) 503)
        "{\"status\": \"initializing\"}")))

(defun start-server (&key (port *default-port*))
  (when *server*
    (stop-server))
  (setf *server-ready* nil)
  (ichiran/conn:load-settings :keep-connection t)
  (setf *server* 
        (make-instance 'ichiran-acceptor 
                      :port port
                      :address "0.0.0.0"
                      :connection-spec ichiran/conn:*connection*))
  (push (create-prefix-dispatcher "/health" 'health-check)
        *dispatch-table*)
  (start *server*)
  (handler-case
      (progn
        (postmodern:with-connection ichiran/conn:*connection*
          (postmodern:query (:select 1)))
        (setf *server-ready* t)
        (format t "~&Server started and ready on port ~A~%" port))
    (error (e)
      (format t "~&Server failed to initialize: ~A~%" e)
      (stop-server)
      (error e))))

(defun stop-server ()
  (when *server*
    (stop-server))
  (setf *server* nil))
