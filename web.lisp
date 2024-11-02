(defpackage :ichiran/web
  (:use :cl :ichiran/all :hunchentoot)
  (:export :start-server :stop-server))

(in-package :ichiran/web)

(defvar *server* nil)
(defvar *default-port* 8080)
(defparameter *max-concurrent-requests* 50)
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
  `(handler-case
       (sb-thread:wait-on-semaphore *request-semaphore*) ; Wait for available slot
       (unwind-protect
           (let ((ichiran/conn:*connection* (connection-spec *acceptor*)))
             (postmodern:with-connection 
                 (append ichiran/conn:*connection* '(:pooled-p t))
               ,@body))
         (sb-thread:signal-semaphore *request-semaphore*)) ; Release slot
     (error (e)
       (format t "~&Error in request: ~A~%" e)
       (sb-thread:signal-semaphore *request-semaphore*)
       (signal e))))

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
          (postmodern:query "SELECT 1"))
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
