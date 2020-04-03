;; loaders for custom data

(in-package :ichiran/custom)

(defgeneric slurp (source)
  (:documentation "Read custom data from the source file"))

(defgeneric insert (source)
  (:documentation "Insert slurped data into database"))

(defgeneric process-entry (source entry)
  (:documentation "Converts a source chunk into one or several entries")
  (:method (source entry) (list entry)))

(defclass custom-source ()
  ((description :reader description :initform "unknown")))

(defclass csv-loader (custom-source)
  ((description :initform "csv")
   (source-file :reader source-file :initarg :source-file)
   (entries :accessor entries :initform nil)
   (csv-options :reader csv-options :initform '(:separator #\, :skip-first-p nil))
   ))

(defmethod slurp ((loader csv-loader) &aux (counter 0))
  (setf (entries loader)
        (loop for row in (apply 'cl-csv:read-csv (source-file loader) (csv-options loader))
           nconc (process-entry loader row)
           do (incf counter)))
  counter)

(defclass municipality-csv (csv-loader)
  ((description :initform "municipalities")))


(defparameter *municipality-types*
  '((#\都 "ﾄ")
    (#\道 "ﾄﾞｳ")
    (#\府 "ﾌ")
    (#\県 "ｹﾝ")
    (#\市 "ｼ")
    (#\町 "ﾁｮｳ" "ﾏﾁ")
    (#\村 "ｿﾝ" "ﾑﾗ")))

(defun municipality-short (text reading)
  (if (alexandria:ends-with #\道 text)
      (cons text reading)
      (let* ((type (char text (1- (length text))))
             (short-text (subseq text 0 (1- (length text))))
             (type-readings (cdr (assoc type *municipality-types*)))
             (short-reading
              (loop for tpr in type-readings
                 thereis (and (alexandria:ends-with-subseq tpr reading)
                              (subseq reading 0 (- (length reading) (length tpr)))))))
        (cons short-text short-reading))))

(defun romanize-municipality (text reading &optional prefecture-p)
  (let ((short-reading (cdr (municipality-short text reading))))
    (format nil "~a~:[~; Prefecture~]"
            (ichiran:romanize-word-geo short-reading)
            (and prefecture-p
                 (not (find text '("北海道" "東京都") :test 'equal))))))

(defstruct municipality text reading definition prefecture-p)

(defmethod process-entry ((loader municipality-csv) row)
  (destructuring-bind (id pref muni rpref rmuni) row
    (declare (ignore id))
    (let* ((prefecture-p (alexandria:emptyp muni))
           (text (if prefecture-p pref muni))
           (reading (if prefecture-p rpref rmuni))
           (short (municipality-short text reading))
           (definition (format nil "~a~@[, ~a~]"
                               (romanize-municipality text reading prefecture-p)
                               (unless prefecture-p (romanize-municipality pref rpref t))
                               ))
           (muni (make-municipality :text text
                                    :reading (as-hiragana (normalize reading))
                                    :definition definition :prefecture-p prefecture-p))
           (muni-short
            (unless (equal (car short) text)
              (make-municipality :text (car short)
                                 :reading (as-hiragana (normalize (cdr short)))
                                 :definition definition :prefecture-p prefecture-p))))
      (if muni-short
          (list muni muni-short)
          (list muni)))))


(defclass wards-csv (csv-loader)
  ((description :initform "wards")))


(defun source-path (file)
  (asdf:system-relative-pathname :ichiran (format nil "data/sources/~a" file)))

(defparameter *custom-data*
  (mapcar
   (lambda (args) (apply 'make-instance args))
   `((municipality-csv :source-file ,(source-path "jichitai.csv"))
     ;; (wards-csv :source-file ,(source-path "gyoseiku.csv"))
     )))
