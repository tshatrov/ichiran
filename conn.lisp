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


(defvar *conn-vars* nil)

(defvar *conn-var-cache* (make-hash-table :test #'equal))

(defmacro def-conn-var (name initial-value &rest args)
  `(progn
     (defvar ,name ,initial-value ,@args)
     (pushnew (cons ',name ,initial-value) *conn-vars* :key 'car)))

(defmacro with-db (dbid &body body)
  (alexandria:with-gensyms (pv-pairs var vars val vals iv key exists)
    `(let* ((*connection* (get-spec ,dbid))
            (,pv-pairs (when ,dbid
                         (loop for (,var . ,iv) in *conn-vars*
                            for ,key = (cons ,var *connection*)
                            for (,val ,exists) = (multiple-value-list (gethash ,key *conn-var-cache*))
                            collect ,var into ,vars
                            if ,exists collect ,val into ,vals
                            else collect ,iv into ,vals
                            finally (return (cons ,vars ,vals))))))
       (progv (car ,pv-pairs) (cdr ,pv-pairs)
         (unwind-protect
              (with-connection *connection*
                ,@body)
           (loop for ,var in (car ,pv-pairs)
              for ,key = (cons ,var *connection*)
              do (setf (gethash ,key *conn-var-cache*) (symbol-value ,var))))))))

(defun switch-conn-vars (dbid)
  (setf *connection* (get-spec dbid)) 
  (loop
     for (var . iv) in *conn-vars*
     for key = (cons var *connection*)
     for (val exists) = (multiple-value-list (gethash key *conn-var-cache*))
     for value = (if exists val iv)
     do (setf (symbol-value var) value))) 

(def-conn-var *test-var* 10)
