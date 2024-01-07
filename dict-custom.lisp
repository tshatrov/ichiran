;; loaders for custom data

(in-package :ichiran/custom)

(defparameter *silent-p* nil)

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

Returns 2 values, whether the entry should be either added or updated, and which SEQ to update if any.
")
  (:method (source entry) t))

(defgeneric insert-entry (source entry seq)
  (:documentation "Insert entry into database"))

(defgeneric update-entry (source entry seq)
  (:documentation "Update entry in database"))

(defgeneric update-entry-gloss (source entry seq gloss)
  (:documentation "Update entry gloss in database"))

(defmethod insert (source)
  (with-connection *connection*
    (loop
       with cur-seq = (ichiran/dict::next-seq)
       for entry in (entries source)
       do (multiple-value-bind (ok seq) (test-entry source entry)
            (when ok
              (cond
                ((consp seq) (apply 'update-entry-gloss source entry seq))
                (seq (update-entry source entry seq))
                (t
                 (insert-entry source entry cur-seq)
                 (incf cur-seq))))))))


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
        do (ichiran/dict::load-entry
            (xml-entry-content entry)
            :if-exists :skip
            :seq (xml-entry-seq entry)
            :conjugate-p t)))

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
  '((#\都 "ﾄ" "と")
    (#\道 "ﾄﾞｳ" "どう")
    (#\府 "ﾌ" "ふ")
    (#\県 "ｹﾝ" "けん")
    (#\市 "ｼ" "し")
    (#\町 "ﾁｮｳ" "ﾏﾁ" "ちょう" "まち")
    (#\村 "ｿﾝ" "ﾑﾗ" "そん" "むら")
    (#\区 "ｸ" "く")))

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

(defparameter *municipality-types-order* "都道府県市区町村")

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

(defun romanize-municipality (text reading &key (include-type t))
  (let ((short-reading (cdr (municipality-short text reading)))
        (type (char text (1- (length text)))))
    (format nil "~a~@[ ~a~]"
            (ichiran:romanize-word-geo short-reading)
            (and include-type
                 (cdr (assoc type *municipality-types-description*))))))

(defstruct municipality text reading definition type prefecture)

(defmethod process-entry ((loader municipality-csv) row)
  (destructuring-bind (id pref muni rpref rmuni) row
    (declare (ignore id))
    (let* ((prefecture-p (alexandria:emptyp muni))
           (text (if prefecture-p pref muni))
           (type (char text (1- (length text))))
           (reading (if prefecture-p rpref rmuni))
           (short (municipality-short text reading))
           (prefecture (unless prefecture-p (romanize-municipality pref rpref)))
           (definition (format nil "~a~@[, ~a~]"
                               (romanize-municipality text reading)
                               prefecture))
           (muni (make-municipality :text text :type type
                                    :reading (as-hiragana (normalize reading))
                                    :definition definition
                                    :prefecture prefecture))
           (muni-short
            (unless (find type "道")
              (make-municipality :text (car short) :type type
                                 :reading (as-hiragana (normalize (cdr short)))
                                 :definition definition
                                 :prefecture prefecture))))
      (if muni-short
          (list muni muni-short)
          (list muni)))))

(defmethod slurp :after ((loader municipality-csv))
  (setf (entries loader)
        (stable-sort (entries loader) '<
                     :key (lambda (e) (position (municipality-type e) *municipality-types-order*)))))

(defun normalize-geo (word)
  (simplify-ngrams (string-downcase word) '("ū" "u" "ō" "o")))

(defgeneric get-words (entry)
  (:method ((entry municipality))
    (let* ((typeword (cdr (assoc (municipality-type entry) *municipality-types-description*)))
           (name (car (split-sequence #\Space (municipality-definition entry)))))
      (when (and typeword (alexandria:ends-with #\) typeword))
        (setf typeword (subseq typeword 0 (1- (length typeword)))))

      (remove-if 'null (list name typeword (municipality-prefecture entry))))))

(defmethod test-entry (loader (entry municipality))
  (multiple-value-bind (seq match-p)
      (let ((words (get-words entry)))
        (ichiran/dict:match-glosses
         (municipality-text entry)
         (municipality-reading entry)
         words
         :normalize 'normalize-geo
         :update-gloss (case (municipality-type entry)
                         (#\市
                          `(:sequence
                            :case-insensitive-p
                            :start-anchor
                            ,(car words)
                            " (city"
                            (:alternation
                            :void
                            (:sequence " in " ,(car (split-sequence #\Space (municipality-prefecture entry))))
                            )
                           ")"
                            :end-anchor))
                         (#\県
                          `(:sequence
                            :case-insensitive-p
                            :start-anchor
                            ,(car words)
                            " ("
                            (:alternation :void "city, ")
                            "prefecture)"
                            :end-anchor)))))
    (cond ((not seq) (values t nil))
          ((consp seq) (values t seq))
          (match-p (values nil seq))
          (t (values t seq)))))

(defgeneric as-xml (entry)
  (:documentation "Representation of entry as XML to be loaded by load-entry"))

(defun as-xml-simple (text reading definition)
  (cxml:with-xml-output (cxml-dom:make-dom-builder)
    (cxml:with-element "entry"
      (cxml:with-element "ent_seq"
        (cxml:text ""))
      (cond ((test-word text :kana)
             (cxml:with-element "r_ele"
               (cxml:with-element "reb"
                 (cxml:text text))))
            (t
             (cxml:with-element "k_ele"
               (cxml:with-element "keb"
                 (cxml:text text)))
             (cxml:with-element "r_ele"
               (cxml:with-element "reb"
                 (cxml:text reading)))))
      (cxml:with-element "sense"
        (cxml:with-element "pos"
          (cxml:text "n"))
        (cxml:with-element "gloss"
          (cxml:attribute "xml:lang" "eng")
          (cxml:text definition))))))

(defmethod as-xml ((entry municipality))
  (as-xml-simple (municipality-text entry) (municipality-reading entry) (municipality-definition entry)))

(defmethod insert-entry ((loader municipality-csv) entry seq)
  (unless *silent-p*
    (format t "Inserting ~a[~a] ~a (seq=~a)~%" (municipality-text entry) (municipality-reading entry) (municipality-definition entry) seq))
  (ichiran/dict::load-entry (as-xml entry) :seq seq))

(defmethod update-entry ((loader municipality-csv) entry seq)
  (unless *silent-p*
    (format t "Updating ~a[~a] ~a (seq=~a)~%" (municipality-text entry) (municipality-reading entry) (municipality-definition entry) seq))
  (ichiran/dict::add-new-sense seq '("n") (list (municipality-definition entry))))

(defmethod update-entry-gloss ((loader municipality-csv) entry seq gloss)
  (let ((new-gloss (municipality-definition entry)))
    (unless *silent-p*
      (format t "Updating gloss ~a[~a] ~a -> ~a (seq=~a)~%" (municipality-text entry) (municipality-reading entry) gloss new-gloss seq))
    (query (:update 'gloss :set 'text new-gloss
                    :from 'sense :where (:and (:= 'gloss.sense-id 'sense.id) (:= 'sense.seq seq) (:= 'gloss.text gloss))))))

(defclass ward-csv (csv-loader)
  ((description :initform "wards")))

(defstruct ward text reading definition city)

(defmethod slurp ((loader ward-csv))
  (setf (entries loader)
        (loop
           with city-text and city-reading and city-romanized
           for (id text reading) in (apply 'cl-csv:read-csv (source-file loader) (csv-options loader))
           if (alexandria:ends-with #\区 text)
           collect
             (let* ((ward-text (subseq text (length city-text)))
                    (ward-reading (subseq reading (length city-reading)))
                    (definition (format nil "~a, ~a" (romanize-municipality ward-text ward-reading) city-romanized)))
               (make-ward :text ward-text :reading ward-reading :definition definition :city city-romanized))
           else do (setf city-text text
                         city-reading reading
                         city-romanized (romanize-municipality text reading :include-type nil)))))

(defmethod get-words ((entry ward))
  (let ((name (car (split-sequence #\Space (ward-definition entry)))))
    (list name "Ward" (ward-city entry))))

(defmethod test-entry (loader (entry ward))
  (multiple-value-bind (seq match-p)
      (let ((words (get-words entry)))
        (ichiran/dict:match-glosses
         (ward-text entry)
         (ward-reading entry)
         words
         :normalize 'normalize-geo))
    (cond ((not seq) (values t nil))
          (match-p (values nil seq))
          (t (values t seq)))))

(defmethod as-xml ((entry ward))
  (as-xml-simple (ward-text entry) (ward-reading entry) (ward-definition entry)))

(defmethod insert-entry ((loader ward-csv) entry seq)
  (unless *silent-p*
    (format t "Inserting ~a[~a] ~a (seq=~a)~%" (ward-text entry) (ward-reading entry) (ward-definition entry) seq))
  (ichiran/dict::load-entry (as-xml entry) :seq seq))

(defmethod update-entry ((loader ward-csv) entry seq)
  (unless *silent-p*
    (format t "Updating ~a[~a] ~a (seq=~a)~%" (ward-text entry) (ward-reading entry) (ward-definition entry) seq))
  (ichiran/dict::add-new-sense seq '("n") (list (ward-definition entry))))

(defun source-path (file)
  (asdf:system-relative-pathname :ichiran (format nil "data/sources/~a" file)))

(defun get-custom-data ()
  (mapcar
   (lambda (args) (if (keywordp args) args (apply 'make-instance args)))
   `(
     :extra (xml-loader :source-file ,(source-path "extra.xml"))
     :municipality (municipality-csv :source-file ,(source-path "jichitai.csv"))
     :ward (ward-csv :source-file ,(source-path "gyoseiku.csv"))
     )))

(defun load-custom-data (&optional keys silent-p)
  (let ((loaders (loop for (key loader) on (get-custom-data) by #'cddr
                    if (or (not keys) (find key keys)) collect loader))
        (*silent-p* silent-p))
    (dolist (loader loaders)
      (unless *silent-p* (format t "Loading ~a~%" (description loader)))
      (let ((n-entries (slurp loader)))
        (unless *silent-p* (format t "Inserting ~a entries~%" n-entries)))
      (insert loader))))
