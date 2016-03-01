;;;; ichiran db connection manager

(in-package #:ichiran/conn)

;; main connection
(defvar *connection* '("jmdict" "postgres" "" "localhost"))

;; secondary connections (alist)
(defvar *connections* '((:a "jmdict2" "postgres" "" "localhost")
                        (:b "jmdict3" "postgres" "" "localhost")))

(load (asdf:system-relative-pathname :ichiran "settings.lisp") :if-does-not-exist nil)

(defun get-spec (dbid)
  (cond ((not dbid) *connection*)
        ((listp dbid) dbid)
        (t (let ((spec (cdr (assoc dbid *connections*))))
             (if spec spec
                 (error "Invalid connection!"))))))

(defmacro let-db (dbid &body body)
  `(let ((*connection* (get-spec ,dbid)))
     ,@body))

(defmacro with-db (dbid &body body)
  `(let ((*connection* (get-spec ,dbid)))
     (with-connection *connection*
       ,@body)))
