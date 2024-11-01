(defpackage :ichiran/web
  (:use :cl :ichiran/all :hunchentoot)
  (:export :start-server :stop-server))

(in-package :ichiran/web)

(defvar *server* nil)
(defvar *default-port* 8080)

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
  `(let ((ichiran/conn:*connection* (connection-spec *acceptor*)))
     (postmodern:with-connection ichiran/conn:*connection*
       ,@body)))

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

(defun start-server (&key (port *default-port*))
  (when *server*
    (stop-server))
  (ichiran/conn:load-settings :keep-connection t)  ; Ensure DB connection
  (setf *server* 
        (make-instance 'ichiran-acceptor 
                      :port port
                      :address "0.0.0.0"))
  (start *server*)
  (format t "~&Server started on port ~A~%" port))

(defun stop-server ()
  (when *server*
    (stop-server))
  (setf *server* nil))
