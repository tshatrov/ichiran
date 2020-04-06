;; loaders for custom data

(in-package :ichiran/custom)

(defgeneric slurp (source)
  (:documentation "Read custom data from the source file"))

(defmethod slurp :around (source)
  (call-next-method)
  (length (entries source)))

(defgeneric insert (source)
  (:documentation "Insert slurped data into database"))

;; only `slurp` and `insert` are necessary to implement, the others are optional

(defgeneric process-entry (source entry)
  (:documentation "Converts a source chunk into one or several entries")
  (:method (source entry) (list entry)))

(defgeneric test-entry (source entry)
  (:documentation "Tests if the entry should be inserted into database

Returns 2 values, whether the entry should be added or updated, and which SEQ to update if any.
")
  (:method (source entry) t))

(defclass custom-source ()
  ((description :reader description :initform "unknown")
   (entries :accessor entries :initform nil)))


(defclass xml-loader (custom-source)
  ((description :initform "extra XML data")
   (source-file :reader source-file :initarg :source-file)))

(defstruct xml-entry seq content)

(defmethod slurp ((loader xml-loader))
  (let* ((content (uiop:read-file-string (source-file loader)))
         (parsed (cxml:parse content (cxml-dom:make-dom-builder))))
    (dom:do-node-list (entry (dom:get-elements-by-tag-name parsed "entry"))
      (let* ((seq (ichiran/dict:node-text (dom:item (dom:get-elements-by-tag-name entry "ent_seq") 0)))
             (nseq (handler-case (parse-integer seq) (error () seq))))
        (push (make-xml-entry :seq nseq :content (rune-dom:create-document entry)) (entries loader)))))
  (setf (entries loader) (nreverse (entries loader))))

(defmethod insert ((loader xml-loader))
  (loop for entry in (entries loader)
     do (ichiran/dict::load-entry (xml-entry-content entry) :if-exists :skip :seq (xml-entry-seq entry))))

(defclass csv-loader (custom-source)
  ((description :initform "csv")
   (source-file :reader source-file :initarg :source-file)
   (csv-options :reader csv-options :initform '(:separator #\, :skip-first-p nil))
   ))

(defmethod slurp ((loader csv-loader))
  (setf (entries loader)
        (loop for row in (apply 'cl-csv:read-csv (source-file loader) (csv-options loader))
           nconc (process-entry loader row))))

(defclass municipality-csv (csv-loader)
  ((description :initform "municipalities")))


(defparameter *municipality-types*
  '((#\都 "ﾄ")
    (#\道 "ﾄﾞｳ")
    (#\府 "ﾌ")
    (#\県 "ｹﾝ")
    (#\市 "ｼ")
    (#\町 "ﾁｮｳ" "ﾏﾁ")
    (#\村 "ｿﾝ" "ﾑﾗ")
    (#\区 "ｸ")))

(defparameter *municipality-types-description*
  '((#\都 . "Metropolis")
    (#\道)
    (#\府 . "Prefecture")
    (#\県 . "Prefecture")
    (#\市 . "(city)")
    (#\町 . "(town)")
    (#\村 . "(village)")
    (#\区 . "Ward")
    ))

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

(defun romanize-municipality (text reading)
  (let ((short-reading (cdr (municipality-short text reading)))
        (type (char text (1- (length text)))))
    (format nil "~a~@[ ~a~]"
            (ichiran:romanize-word-geo short-reading)
            (cdr (assoc type *municipality-types-description*)))))

(defstruct municipality text reading definition prefecture-p type)

(defmethod process-entry ((loader municipality-csv) row)
  (destructuring-bind (id pref muni rpref rmuni) row
    (declare (ignore id))
    (let* ((prefecture-p (alexandria:emptyp muni))
           (text (if prefecture-p pref muni))
           (type (char text (1- (length text))))
           (reading (if prefecture-p rpref rmuni))
           (short (municipality-short text reading))
           (definition (format nil "~a~@[, ~a~]"
                               (romanize-municipality text reading)
                               (unless prefecture-p (romanize-municipality pref rpref))
                               ))
           (muni (make-municipality :text text :type type
                                    :reading (as-hiragana (normalize reading))
                                    :definition definition :prefecture-p prefecture-p))
           (muni-short
            (unless (find type '(#\区 #\道))
              (make-municipality :text (car short) :type type
                                 :reading (as-hiragana (normalize (cdr short)))
                                 :definition definition :prefecture-p prefecture-p))))
      (if muni-short
          (list muni muni-short)
          (list muni)))))

(defun normalize-geo (word)
  (simplify-ngrams (string-downcase word) '("ū" "u" "ō" "o")))

(defgeneric get-words (entry)
  (:method ((entry municipality))
    (let* ((typeword (cdr (assoc (municipality-type entry) *municipality-types-description*)))
           (name (car (split-sequence #\Space (municipality-definition entry)))))
      (if typeword (list name typeword) (list name)))))

(defmethod test-entry (loader (entry municipality))
  (multiple-value-bind (seq match-p)
      (ichiran/dict:match-glosses
       (municipality-text entry)
       (municipality-reading entry)
       (get-words entry)
       :normalize 'normalize-geo)
    (cond ((not seq) (values t nil))
          (match-p (values nil seq))
          (t (values t seq)))))

(defmethod insert ((loader municipality-csv))
  )

(defclass wards-csv (csv-loader)
  ((description :initform "wards")))


(defun source-path (file)
  (asdf:system-relative-pathname :ichiran (format nil "data/sources/~a" file)))

(defun get-custom-data ()
  (mapcar
   (lambda (args) (if (keywordp args) args (apply 'make-instance args)))
   `(
     :extra (xml-loader :source-file ,(source-path "extra.xml"))
     :municipality (municipality-csv :source-file ,(source-path "jichitai.csv"))
     ;; (wards-csv :source-file ,(source-path "gyoseiku.csv"))
     )))

(defun load-custom-data (&optional keys silent-p)
  (let ((loaders (loop for (key loader . rest) on (get-custom-data) by #'cddr
                    if (or (not keys) (find key keys)) collect loader)))
    (dolist (loader loaders)
      (unless silent-p (format t "Loading ~a~%" (description loader)))
      (let ((n-entries (slurp loader)))
        (unless silent-p (format t "Inserting ~a entries~%" n-entries)))
      (insert loader))))
