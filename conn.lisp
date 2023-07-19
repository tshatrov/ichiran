;;;; ichiran db connection manager

(in-package #:ichiran/conn)

;; main connection
(defvar *connection* nil) ;; '("jmdict" "postgres" "" "localhost"))

;; secondary connections (alist)
(defvar *connections* '((:a "jmdict2" "postgres" "" "localhost")
                        (:b "jmdict3" "postgres" "" "localhost")))

(defvar *debug* nil "Enables debug printouts")

(defvar *conn-vars* nil)

(defvar *conn-var-cache* (make-hash-table :test 'equal))

(defun switch-conn-vars (dbid)
  (setf *connection* (get-spec dbid))
  (loop
     for (var . iv) in *conn-vars*
     for key = (cons var *connection*)
     for (val exists) = (multiple-value-list (gethash key *conn-var-cache*))
     for value = (if exists val iv)
     do (setf (symbol-value var) value)))

(defun get-spec (dbid)
  (cond ((not dbid) *connection*)
        ((listp dbid) dbid)
        (t (let ((spec (cdr (assoc dbid *connections*))))
             (if spec spec
                 (error "Invalid connection!"))))))

(defun load-settings (&key keep-connection)
  (set-ichiran-ssl)
  (let ((old-connection *connection*))
    (load (asdf:system-relative-pathname :ichiran "settings.lisp") :if-does-not-exist nil)
    (let ((env-connection (get-ichiran-connection)))
      (if env-connection
          (setf *connection* env-connection)))
    (if (and old-connection keep-connection)
        (setf *connection* old-connection)
        (unless (equal old-connection *connection*)
          (switch-conn-vars *connection*)))))

(defun get-ichiran-connection ()
    (let*
        ((env-connection
            (uiop:getenv "ICHIRAN_CONNECTION"))
        (connection
            (if env-connection
                (cl-ppcre:split "\\s+" env-connection)
                nil)))
    connection))

(defun set-ichiran-ssl ()
  (setf postmodern:*default-use-ssl*
    (let* ((env-ssl (uiop:getenv "ICHIRAN_SSL")))
        (if env-ssl
            (cond
                ((string= env-ssl "no") :no)
                ((string= env-ssl "try") :try)
                ((string= env-ssl "require") :require)
                ((string= env-ssl "yes") :yes)
                ((string= env-ssl "full") :full)
                (t (error (format nil "Invalid environment variable ICHIRAN_SSL=~a. Expected no, try, require, yes or full." env-ssl))))
            :no))))

(load-settings)

(register-sql-operators :2+-ary (:=== "IS NOT DISTINCT FROM"))
(register-sql-operators :2+-ary (:=/= "IS DISTINCT FROM"))

(defmacro let-db (dbid &body body)
  `(let ((*connection* (get-spec ,dbid)))
     ,@body))

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

(def-conn-var *test-var* 10)

(defmacro with-log ((path &key (if-exists :append)) &body body)
  (alexandria:with-gensyms (stream)
    `(with-open-file (,stream ,path :direction :output :if-does-not-exist :create :if-exists ,if-exists)
       (let ((cl-postgres:*query-log* ,stream))
         ,@body))))


(defclass cache ()
  ((mapping :initform nil :allocation :class)
   (name :initarg :name :reader cache-name)
   (var :initarg :var :reader cache-var)
   (lock :reader cache-lock)
   ))

(defmethod initialize-instance :after ((cache cache) &key &allow-other-keys)
  (let* ((name (cache-name cache))
         (old-cache (getf (slot-value cache 'mapping) name)))
    (setf (slot-value cache 'lock)
          (if old-cache
              (cache-lock old-cache)
              (sb-thread:make-mutex :name (symbol-name name))))
    (setf (getf (slot-value cache 'mapping) name) cache)))

(defun all-caches ()
  (slot-value (sb-mop:class-prototype (find-class 'cache)) 'mapping))

(defun get-cache (name)
  (getf (all-caches) name))

(defgeneric init-cache (cache-name)
  (:documentation "Should return a value to initialize cache with"))

(defgeneric reset-cache (cache-name)
  (:method (cache-name)
    (let ((val (init-cache cache-name))
          (cache (get-cache cache-name)))
      (sb-thread:with-mutex ((cache-lock cache))
        (setf (symbol-value (cache-var cache)) val)))))

(defgeneric ensure (cache-name)
  (:method (cache-name)
    (let ((cache (get-cache cache-name)))
      (or (symbol-value (cache-var cache))
          (sb-thread:with-mutex ((cache-lock cache))
            (or (symbol-value (cache-var cache))
                (let ((val (init-cache cache-name)))
                  (setf (symbol-value (cache-var cache)) val))))))))

(defmacro defcache (name var &body init-body)
  (alexandria:with-gensyms (cache-var)
    `(progn
       (def-conn-var ,var nil)
       (make-instance 'cache :name ',name :var ',var)
       (defmethod init-cache ((,cache-var (eql ,name)))
         (with-connection *connection*
           ,@init-body)))))

(defun init-all-caches (&optional reset)
  (loop with fn = (if reset 'reset-cache 'ensure)
     for (name) on (all-caches) by #'cddr
     do (funcall fn name)))

(defun dp (value &key (fn 'print))
  (when *debug*
    (funcall fn value))
  value)
