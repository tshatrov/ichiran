(defpackage :ichiran/cli
  (:use :cl :ichiran/all)
  (:export :build)
  )

(in-package :ichiran/cli)

(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :eval
   :description "evaluate arbitrary expression and print the result"
   :short #\e
   :long "eval")
  (:name :info
   :description "print dictionary info"
   :short #\i
   :long "with-info")
  (:name :full
   :description "full split info (as JSON)"
   :short #\f
   :long "full")
   (:name :limit
   :description "limit segmentations to the specified number (useful only with -f or --full) [Example: ichiran-cli -f -l 5 \"一覧は最高だぞ\"]"
   :short #\l
   :long "limit"
   :arg-parser #'parse-integer
   :default 1
   :meta-var "LIMIT"))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defun print-error (fmt &rest args)
  (apply 'format *error-output* fmt args)
  (finish-output *error-output*))

(defmethod jsown:to-json ((word-info word-info))
  (jsown:to-json (word-info-gloss-json word-info)))

(defun print-romanize-info (info)
  (loop for (word . gloss) in info
        do (format t "~%~%* ~a  ~a" word gloss)))

(defun main ()
  (load-connection-from-env)
  (multiple-value-bind (options free-args)
      (handler-case
        (handler-bind ((opts:unknown-option #'unknown-option))
          (opts:get-opts))

        (opts:missing-arg (condition)
          (print-error "fatal: option ~s needs an argument!~%"
                       (opts:option condition))
          (opts:exit 1))
        (opts:arg-parser-failed (condition)
          (print-error "fatal: cannot parse ~s as argument of ~s~%"
                       (opts:raw-arg condition)
                       (opts:option condition))
          (opts:exit 1))
        (opts:missing-required-option (con)
          (print-error "fatal: ~a~%" con)
          (opts:exit 1)))
    (cond
      ((getf options :help)
       (opts:describe
        :prefix "Command line interface for Ichiran"
        :suffix "By default calls ichiran:romanize, other options change this behavior"
        :usage-of "ichiran-cli"
        :args     "[input]"))
      ((getf options :eval)
       (let ((input (car free-args)))
         (use-package :ichiran/all)
         (mapcar 'print (multiple-value-list (eval (read-from-string input))))))
      ((getf options :info)
       (let ((input (join " " free-args)))
         (multiple-value-bind (r info) (romanize input :with-info t)
           (princ r)
           (print-romanize-info info))))
      ((getf options :full)
       (let* ((input (join " " free-args))
              (limit-value (getf options :limit))
              (result (romanize* input :limit limit-value)))
         (princ (jsown:to-json result))))
      (t (let ((input (join " " free-args)))
           (princ (romanize input :with-info t))))
      ))
  (terpri)
  (finish-output))


(defun setup-debugger ()
  (setf *debugger-hook*
        (lambda (condition old-hook)
          (declare (ignore old-hook))
          (print-error "ERROR: ~a" condition)
          (opts:exit 2))))

(defun build (&key conn debug)
  (when conn
    (switch-conn-vars conn))

  (format t "Initializing caches~%")
  (init-all-caches)
  (init-suffixes t)

  ;; remove references to db connections which become obsolete once the image is loaded
  (postmodern:clear-connection-pool)

  (unless debug
    (setup-debugger))

  (asdf:make :ichiran/cli))
