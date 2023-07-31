(in-package :ichiran/kanji)

(defvar *kanjidic-path* #P"e:/dump/kanjidic2.xml")

(load-settings :keep-connection t)

(defgeneric to-json (obj &key &allow-other-keys)
  (:documentation "Convert object to json"))

(defclass kanji ()
  ((id :reader id :col-type serial)
   (text :reader text :col-type string :initarg :text)
   (radical-c :reader radical-c :col-type integer :initarg :radical-c)
   (radical-n :reader radical-n :col-type integer :initarg :radical-n)
   (grade :reader grade :col-type (or db-null integer) :initarg :grade)
   (strokes :reader strokes :col-type integer :initarg :strokes)
   (freq :reader freq :col-type (or db-null integer) :initarg :freq)

   (stat-common :documentation "Number of common words with this kanji"
             :accessor stat-common :col-type integer :initform 0)
   (stat-irregular :documentation "Out of those, how many have irregular readings"
                   :accessor stat-irregular :col-type integer :initform 0)
   )
  (:metaclass dao-class)
  (:keys id))

(defmethod print-object ((obj kanji) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a ~a" (id obj) (text obj))))

(deftable kanji
  (!dao-def)
  (!index 'text)
  (!index 'radical-c)
  (!index 'radical-n)
  (!index 'grade)
  (!index 'strokes)
  (!index 'freq)
  (!index 'stat-common)
  (!index 'stat-irregular))

(defclass reading ()
  ((id :reader id :col-type serial)
   (kanji-id :reader kanji-id :col-type integer :initarg :kanji-id)
   (type :reader reading-type :col-type string :initarg :type)
   (text :reader text :col-type string :initarg :text)
   (suffixp :reader suffixp :col-type boolean :initarg :suffixp :initform nil)
   (prefixp :reader prefixp :col-type boolean :initarg :prefixp :initform nil)

   (stat-common :documentation "Number of common words that use this reading"
                :accessor stat-common :col-type integer :initform 0))
  (:metaclass dao-class)
  (:keys id))

(defmethod print-object ((obj reading) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a ~a ~a" (kanji-id obj) (reading-type obj) (text obj))))

(deftable reading
  (!dao-def)
  (!index 'kanji-id)
  (!index 'type)
  (!index 'text)
  (!index 'stat-common)
  (!foreign 'kanji 'kanji-id 'id :on-delete :cascade))

(defclass okurigana ()
  ((id :reader id :col-type serial)
   (reading-id :reader reading-id :col-type integer :initarg :reading-id)
   (text :reader text :col-type string :initarg :text))
  (:metaclass dao-class)
  (:keys id))

(defmethod print-object ((obj okurigana) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a ~a" (reading-id obj) (text obj))))

(deftable okurigana
  (!dao-def)
  (!index 'reading-id)
  (!foreign 'reading 'reading-id 'id :on-delete :cascade))

(defclass meaning ()
  ((id :reader id :col-type serial)
   (kanji-id :reader kanji-id :col-type integer :initarg :kanji-id)
   (text :reader text :col-type string :initarg :text))
  (:metaclass dao-class)
  (:keys id))

(defmethod print-object ((obj meaning) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a ~a" (kanji-id obj) (text obj))))

(deftable meaning
  (!dao-def)
  (!index 'kanji-id)
  (!index 'text)
  (!foreign 'kanji 'kanji-id 'id :on-delete :cascade))

(defun init-tables ()
  (with-connection *connection*
    (let ((tables '(kanji reading okurigana meaning)))
      (loop for table in (reverse tables)
         do (drop-table table :if-exists t))
      (loop for table in tables
         do (create-table table)))))

(defun first-node-text (nodes &key (default :null) wrapper)
  (if (> (dom:length nodes) 0)
      (let ((text (node-text (dom:item nodes 0))))
        (if wrapper
            (funcall wrapper text)
            text))
      default))

(defun load-readings (nodes kanji-id)
  (let ((readings (make-hash-table :test 'equal)))
    (dom:do-node-list (node nodes)
      (let ((type (dom:get-attribute node "r_type"))
            (text (node-text node)))
        (when (member type '("ja_on" "ja_kun") :test 'equal)
          (let ((text (as-hiragana text))
                reading prefixp suffixp okuri)
            (when (char= (char text 0) #\-)
              (setf suffixp t text (subseq text 1)))
            (when (char= (char text (1- (length text))) #\-)
              (setf prefixp t text (subseq text 0 (1- (length text)))))
            (let ((dot (position #\. text)))
              (if dot
                  (setf reading (subseq text 0 dot)
                        okuri (subseq text (1+ dot)))
                  (setf reading text)))
            (let ((old-reading (gethash reading readings)))
              (cond (old-reading
                     (unless (equal (getf old-reading :type) type)
                       (setf (getf old-reading :type) "ja_onkun"))
                     (when okuri
                       (push okuri (getf old-reading :okuri)))
                     (when suffixp (setf (getf old-reading :suffixp) t))
                     (when prefixp (setf (getf old-reading :prefixp) t)))
                    (t
                     (setf (gethash reading readings)
                           `(:okuri ,(if okuri (list okuri) nil) :type ,type :suffixp ,suffixp :prefixp ,prefixp)))))))))
    (maphash
     (lambda (text rinfo)
       (let ((okuri (getf rinfo :okuri)))
         (remf rinfo :okuri)
         (let ((robj (apply #'make-dao 'reading :text text :kanji-id kanji-id rinfo)))
           (loop with rid = (id robj)
              for of in okuri
              do (make-dao 'okurigana :text of :reading-id rid)))))
     readings)))


(defun load-kanji (content)
  (let* ((parsed (cxml:parse content (cxml-dom:make-dom-builder)))
         (literal (first-node-text (dom:get-elements-by-tag-name parsed "literal")))
         (node-radical (dom:get-elements-by-tag-name parsed "rad_value"))
         radical-c radical-n
         (grade (first-node-text (dom:get-elements-by-tag-name parsed "grade")
                                 :wrapper 'parse-integer))
         (strokes (first-node-text (dom:get-elements-by-tag-name parsed "stroke_count")
                                        :wrapper 'parse-integer))
         (freq (first-node-text (dom:get-elements-by-tag-name parsed "freq")
                                     :wrapper 'parse-integer))
         (node-reading (dom:get-elements-by-tag-name parsed "reading"))
         (node-nanori (dom:get-elements-by-tag-name parsed "nanori"))
         (node-meaning (dom:get-elements-by-tag-name parsed "meaning")))
    (dom:do-node-list (node node-radical)
      (let ((type (dom:get-attribute node "rad_type"))
            (radical (parse-integer (node-text node))))
        (cond
          ((equal type "classical") (unless radical-c (setf radical-c radical)))
          ((equal type "nelson_c") (unless radical-n (setf radical-n radical))))))
    (unless radical-n (setf radical-n radical-c))
    (let ((kanji-id (id (make-dao 'kanji :text literal :radical-c radical-c :radical-n radical-n
                                  :grade grade :strokes strokes :freq freq))))
      (load-readings node-reading kanji-id)
      (dom:do-node-list (node node-nanori)
        (make-dao 'reading :type "ja_na" :text (as-hiragana (node-text node)) :kanji-id kanji-id))
      (dom:do-node-list (node node-meaning)
        (let ((lang (dom:get-attribute node "m_lang"))
              (text (node-text node)))
          (when (member lang '("" "en") :test 'equal)
            (make-dao 'meaning :text text :kanji-id kanji-id)))))))

(defun load-kanjidic (&key (path *kanjidic-path*))
  (init-tables)
  (with-connection *connection*
    (klacks:with-open-source (source (cxml:make-source path))
      (klacks:find-element source "kanjidic2")
      (loop for cnt from 1
           while (klacks:find-element source "character")
           do (let ((content (klacks:serialize-element source (cxml:make-string-sink))))
                (load-kanji content))
           if (zerop (mod cnt 500)) do (format t "~a entries loaded~%" cnt)
           finally (query "ANALYZE") (format t "~a entries total~%" cnt)))))

(defparameter *reading-cache* (make-hash-table :test 'equal))

(defun get-readings-cache (str typeset)
  (with-connection *connection*
    (let* ((key (cons str typeset))
           (val (gethash key *reading-cache* :none)))
      (if (eql val :none)
          (let ((result (query (:select 'r.text 'r.type :from (:as 'kanji 'k)
                                        :inner-join (:as 'reading 'r) :on (:= 'r.kanji-id 'k.id)
                                        :where (:and (:= 'k.text str)
                                                     (:not (:in 'r.type (:set typeset))))))))
            (setf (gethash key *reading-cache*) result))
          val))))

(defun get-readings (char &key names)
  (let ((str (if (typep char 'character) (make-string 1 :initial-element char) char))
        (typeset (if names nil '("ja_na"))))
    (get-readings-cache str typeset)))

(defun get-reading-alternatives (reading type &key rendaku)
  (let* ((end (1- (length reading)))
         (lst `(,(list reading type nil)
                ,@(when (and (> end 0)
                             (string= type "ja_on")
                             (find (char reading end) "つくきち"))
                        (list (list (geminate reading :fresh t) type nil (format nil "~c" (char reading end))))))))
    (if rendaku
        (append lst (loop for (rd nil nil gem) in lst
                       collect (list (rendaku rd :fresh t) type :rendaku gem)
                       collect (list (rendaku rd :fresh t :handakuten t) type :rendaku gem)))
        lst)))

(defun get-normal-readings (char &key rendaku)
  (let* ((str (if (typep char 'character) (make-string 1 :initial-element char) char))
         (readings (get-readings-cache str '("ja_na")))
         (readings* (loop for (reading type) in readings
                         for (main . rest) = (get-reading-alternatives reading type :rendaku rendaku)
                         collect main into main-readings
                         nconc rest into alt-readings
                         finally (return (append main-readings alt-readings)))))
    (remove-duplicates readings* :test 'equal :key 'car :from-end t)))

(defun match-readings* (rmap reading &key (start 0))
  (unless rmap
    (return-from match-readings*
      (if (>= start (length reading))
          (values nil 0)
          :none)))
  (when (>= start (length reading))
    (return-from match-readings* :none))

  (let ((item (car rmap))
        matches)
    (cond ((listp item)
           (loop for end from (1+ start) to (length reading)
                for (match score) = (multiple-value-list (match-readings* (cdr rmap) reading :start end))
                unless (eql match :none)
                do (unless (loop for r in item
                              unless (mismatch reading (car r) :start1 start :end1 end)
                              do (push (cons (cons r match) score) matches) (return t))
                     (push (cons (cons (list (subseq reading start end) "irr") match) (- score (- end start))) matches)))
           (if matches
               (loop with max-score and best-match
                    for (match . score) in matches
                    if (or (not max-score) (> score max-score))
                    do (setf max-score score best-match match)
                    finally (return (values best-match max-score)))
               :none))
          (t (if (eql item (char reading start))
                 (multiple-value-bind (match score) (match-readings* (cdr rmap) reading :start (1+ start))
                     (if (eql match :none) :none
                         (values (cons item match) score)))
                 :none)))))

(defun make-rmap (str)
  (loop with prev-kanji
     for start from 0 below (length str)
     for end = (1+ start)
     for char = (char str start)
     if (ppcre:scan *kanji-regex* str :start start :end end)
     collect (cond ((eql char #\々)
                    (prog1 (when prev-kanji (get-normal-readings prev-kanji :rendaku t))
                      (setf prev-kanji nil)))
                   ((eql char #\ヶ)
                    (setf prev-kanji nil)
                    '(("か" "ja_on") ("が" "abbr")))
                   ((eql char #\〆)
                    (setf prev-kanji #\締)
                    '(("しめ" "ja_kun") ("じめ" "ja_kun" :rendaku)))
                   (t (setf prev-kanji char)
                      (get-normal-readings char :rendaku (> start 0))))
     else collect char))

(defun match-readings (str reading)
  (let* ((rmap (make-rmap str))
         (match (match-readings* rmap reading)))
    (unless (eql match :none)
      (loop with charbag and result
         for m in match
         for c across str
         if (listp m)
           when charbag do (push (coerce (nreverse charbag) 'string) result) (setf charbag nil) end
           and do (push (cons (make-string 1 :initial-element c) m) result)
         else
           do (push c charbag)
         finally
           (when charbag (push (coerce (nreverse charbag) 'string) result))
           (return (nreverse result))))))

(defun get-original-reading (rtext &optional rendaku geminated)
  (when rendaku
    (setf rtext (unrendaku rtext :fresh t)))
  (when geminated
    (setf rtext (copy-seq rtext))
    (setf (char rtext (1- (length rtext))) (char geminated 0)))
  rtext)

(defun kanji-word-stats (char)
  (let* ((str (if (typep char 'character) (make-string 1 :initial-element char) char))
         (words (get-kanji-words str))
         (r-stat (make-hash-table :test 'equal))
         (irregular 0))
    (loop for (seq k r common) in words
         for reading = (assoc str (remove-if-not 'listp (match-readings k r)) :test 'equal)
         if reading do
         (destructuring-bind (rtext rtype &rest options) (cdr reading)
           (if (equal rtype "irr")
               (incf irregular)
               (let ((key (list (apply #'get-original-reading rtext options) rtype)))
                 (incf (gethash key r-stat 0)))))
         else do (incf irregular))
    (values (alexandria:hash-table-alist r-stat) irregular (length words))))

(defun load-kanji-stats ()
  (with-connection *connection*
    (query (:update 'kanji :set 'stat-common 0 'stat-irregular 0))
    (query (:update 'reading :set 'stat-common 0))
    (loop for kanji in (select-dao 'kanji (:<= 'grade 8))
         for cnt from 1
         for (reading-stats irregular total) = (multiple-value-list (kanji-word-stats (text kanji)))
         for readings = (select-dao 'reading (:= 'kanji-id (id kanji)))
         do (setf (stat-common kanji) total
                  (stat-irregular kanji) irregular)
         (update-dao kanji)
         (loop for ((rtext rtype) . rcount) in reading-stats
              for reading = (find-if (lambda (r) (and (equal (text r) rtext) (equal (reading-type r) rtype))) readings)
              if reading do (setf (stat-common reading) rcount) (update-dao reading))
         if (zerop (mod cnt 100)) do (format t "~a kanji processed~%" cnt)
         finally (query "ANALYZE") (format t "~a kanji total~%" cnt))))

(defun calculate-perc (sample total)
  (if (= total 0)
      "--.--%"
      (format nil "~,2,,,'0F%" (* 100 (/ sample total)))))

(defun reading-info-json (reading total)
  (with-connection *connection*
    (let ((js (jsown:new-js
                ("text" (text reading))
                ("rtext" (romanize-word (text reading) :method *hepburn-basic* :original-spelling ""))
                ("type" (reading-type reading))
                ("okuri" (query (:select 'text :distinct :from 'okurigana :where (:= 'reading-id (id reading))) :column))
                ("sample" (stat-common reading))
                ("perc" (calculate-perc (stat-common reading) total)))))
      (when (prefixp reading)
        (jsown:extend-js js ("prefixp" t)))
      (when (suffixp reading)
        (jsown:extend-js js ("suffixp" t)))
      js)))

(defmethod to-json ((kanji kanji) &key)
  (let* ((total (stat-common kanji))
         (js (jsown:new-js
               ("text" (text kanji))
               ("rc" (radical-c kanji))
               ("rn" (radical-n kanji))
               ("strokes" (strokes kanji))
               ("total" (stat-common kanji))
               ("irr" (stat-irregular kanji))
               ("irr_perc" (calculate-perc (stat-irregular kanji) total))
               ("readings" (mapcar (lambda (r) (reading-info-json r total))
                                   (select-dao 'reading
                                               (:and (:= 'kanji-id (id kanji))
                                                     (:not (:= 'type "ja_na")))
                                               (:desc 'type) (:desc 'stat-common))))
               ("meanings" (mapcar 'text (select-dao 'meaning (:= 'kanji-id (id kanji)) 'id)))
               )))
    (when (freq kanji)
      (jsown:extend-js js ("freq" (freq kanji))))
    (when (grade kanji)
      (jsown:extend-js js ("grade" (grade kanji))))
    js))

(defun kanji-info-json (char)
  (with-connection *connection*
    (let* ((str (if (typep char 'character) (make-string 1 :initial-element char) char))
           (kanji (car (select-dao 'kanji (:= 'text str)))))
      (when kanji
        (to-json kanji)))))

(defun get-reading-stats (kanji reading type)
  (with-connection *connection*
    (let ((query (query (:select 'r.stat-common 'k.stat-common 'k.grade :from (:as 'kanji 'k) (:as 'reading 'r)
                                 :where (:and (:= 'k.id 'r.kanji-id)
                                              (:= 'k.text kanji)
                                              (:= 'r.text reading)
                                              (:= 'r.type type))) :row)))
      (when query
        (destructuring-bind (sample total grade) query
          (list sample total (calculate-perc sample total) grade))))))

(defun kanji-reading-json (kanji reading type &optional rendaku geminated)
  (let ((js (jsown:new-js ("kanji" kanji) ("reading" reading) ("type" type))))
    (when (ppcre:scan *kanji-char-regex* kanji)
      (jsown:extend-js js ("link" t)))
    (when rendaku
      (jsown:extend-js js ("rendaku" rendaku)))
    (when geminated
      (jsown:extend-js js ("geminated" geminated)))
    (let ((stats (get-reading-stats kanji (get-original-reading reading rendaku geminated) type)))
      (when stats
        (jsown:extend-js js ("stats" t))
        (destructuring-bind (sample total perc grade) stats
          (jsown:extend-js js ("sample" sample) ("total" total) ("perc" perc))
          (when (not (eql grade :null))
            (jsown:extend-js js ("grade" grade)))
          )))
    js))

(defun process-match-json (match)
  (loop with irrbag and result
     with empty-bag = (lambda (&aux (ib (nreverse irrbag)))
                        (let ((js (jsown:new-js
                                    ("kanji" (apply 'concatenate 'string (mapcar 'first ib)))
                                    ("reading" (apply 'concatenate 'string (mapcar 'second ib)))
                                    ("type" "irr"))))
                          (when (ppcre:scan *kanji-char-regex* (jsown:val js "kanji"))
                            (jsown:extend-js js ("link" t)))
                          (push js result))
                        (setf irrbag nil))
     for item in match
     if (listp item) do
         (cond
           ((equal (third item) "irr") (push item irrbag))
           (t (when irrbag
                (funcall empty-bag))
              (push (apply 'kanji-reading-json item) result)))
     else do
       (when irrbag (funcall empty-bag))
       (push (jsown:new-js ("text" item)) result)
     finally (when irrbag (funcall empty-bag))
       (return (nreverse result))))

(defun match-readings-json (str reading)
  (and (ppcre:scan *kanji-regex* str)
       (let ((match (match-readings str reading)))
         (when match
           (process-match-json match)))))

(defmacro query-kanji-json (var query &body extra-fields)
  (alexandria:with-gensyms  (js)
    `(with-connection *connection*
       (mapcar (lambda (,var)
               (let ((,js (to-json ,var)))
                 (jsown:extend-js ,js ,@extra-fields)))
             (query-dao 'kanji ,query)))))
