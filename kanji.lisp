(in-package :ichiran/kanji)

(defvar *kanjidic-path* #P"e:/dump/kanjidic2.xml")

(defvar *connection* '("jmdict" "postgres" "" "localhost"))

(load (asdf:system-relative-pathname :ichiran "settings.lisp") :if-does-not-exist nil)


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
    (let ((tables '(kanji reading meaning)))
      (loop for table in (reverse tables)
         do (query (:drop-table :if-exists table)))
      (loop for table in tables
         do (create-table table)))))

(defun first-node-text (nodes &key (default :null) wrapper)
  (if (> (dom:length nodes) 0)
      (let ((text (node-text (dom:item nodes 0))))
        (if wrapper
            (funcall wrapper text)
            text))
      default))

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
      (dom:do-node-list (node node-reading)
        (let ((type (dom:get-attribute node "r_type"))
              (text (node-text node)))
          (when (member type '("ja_on" "ja_kun") :test 'equal)
            (make-dao 'reading :type type :text (as-hiragana text) :kanji-id kanji-id))))
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
                                                     (:in 'r.type (:set typeset)))))))
            (setf (gethash key *reading-cache*) result))
          val))))
  
(defun get-readings (char &key names)
  (let ((str (if (typep char 'character) (make-string 1 :initial-element char) char))
        (typeset (if names '("ja_on" "ja_kun" "ja_na") '("ja_on" "ja_kun"))))
    (get-readings-cache str typeset)))

(defun get-normal-readings (char &key rendaku)
  (let* ((str (if (typep char 'character) (make-string 1 :initial-element char) char))
         (readings (get-readings-cache str '("ja_on" "ja_kun")))
         (readings* (loop for (text type) in readings
                       for dot = (position #\. text)
                       for reading = (if dot (subseq text 0 dot) text)
                       collect (list reading type)
                       if rendaku
                         collect (list (rendaku reading :fresh t) type :rendaku))))
    (remove-duplicates readings* :test 'equal :key 'car :from-end t)))

;; (defun make-rmap-regex (rmap)
;;   `(:sequence
;;     :start-anchor
;;     ,@(loop for r in rmap
;;            if (listp r)
;;            if (> (length r) 1)
;;              collect `(:register (:alternation ,@(mapcar 'car r)))
;;            else
;;              collect `(:register ,(caar r))
;;            else collect r)
;;     :end-anchor))

;; (defun match-readings* (rmap reading)
;;   (let* ((regex (make-rmap-regex rmap)))
;;     (multiple-value-bind (scan groups) (ppcre:scan-to-strings regex reading)
;;       (if scan
;;           (loop with gr = (coerce groups 'list)
;;              for r in rmap
;;              if (listp r)
;;                collect (assoc (car gr) r :test 'equal)
;;                and do (setf gr (cdr gr))
;;              else
;;                collect r)))))

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
                    for (match . score) in (nreverse matches)
                    if (or (not max-score) (> score max-score))
                    do (setf max-score score best-match match)
                    finally (return (values best-match score)))
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
                    (get-normal-readings #\締 :rendaku (> start 0)))
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
           when charbag do (push (coerce (nreverse charbag) 'string) result) end
           and do (push (cons (make-string 1 :initial-element c) m) result)
         else
           do (push c charbag)
         finally
           (when charbag (push (coerce (nreverse charbag) 'string) result))
           (return (nreverse result))))))
               
(defun kanji-word-stats (char)
  (let* ((str (if (typep char 'character) (make-string 1 :initial-element char) char))
         (words (get-kanji-words str))
         (r-stat (make-hash-table :test 'equal))
         (irregular 0))
    (loop for (seq k r common) in words
         for reading = (assoc str (remove-if-not 'listp (match-readings k r)) :test 'equal)
         if reading do 
         (destructuring-bind (rtext rtype &optional rendaku) (cdr reading)
           (if (equal rtype "irr")
               (incf irregular)
               (let ((key (list (if rendaku (unrendaku rtext :fresh t) rtext) rtype)))
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
    
         
    
