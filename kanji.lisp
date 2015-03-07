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
   )
  (:metaclass dao-class)
  (:keys id))

(defmethod print-object ((obj kanji) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a" (text obj))))

(deftable kanji
  (!dao-def)
  (!index 'text)
  (!index 'radical-c)
  (!index 'radical-n)
  (!index 'grade)
  (!index 'strokes)
  (!index 'freq))   

(defclass reading ()
  ((id :reader id :col-type serial)
   (kanji-id :reader kanji-id :col-type integer :initarg :kanji-id)
   (type :reader reading-type :col-type string :initarg :type)
   (text :reader text :col-type string :initarg :text))
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


(defun get-readings (char &key names)
  (let ((str (if (typep char 'character) (make-string 1 :initial-element char) char))
        (typeset (if names '("ja_on" "ja_kun" "ja_na") '("ja_on" "ja_kun"))))
    (query (:select 'r.text 'r.type :from (:as 'kanji 'k)
                    :inner-join (:as 'reading 'r) :on (:= 'r.kanji-id 'k.id)
                    :where (:and (:= 'k.text str)
                                 (:in 'r.type (:set typeset)))))))

(defun get-normal-readings (char &key rendaku)
  (let* ((str (if (typep char 'character) (make-string 1 :initial-element char) char))
         (readings (query (:select 'r.text 'r.type :from (:as 'kanji 'k)
                                   :inner-join (:as 'reading 'r) :on (:= 'r.kanji-id 'k.id)
                                   :where (:and (:= 'k.text str)
                                                (:in 'r.type (:set "ja_on" "ja_kun"))))))
         (readings* (loop for (text type) in readings
                       for dot = (position #\. text)
                       for reading = (if dot (subseq text 0 dot) text)
                       collect (list reading type)
                       if rendaku
                         collect (list (rendaku reading :fresh t) type :rendaku))))
    (remove-duplicates readings* :test 'equal :key 'car :from-end t)))

(defun make-rmap-regex (rmap)
  `(:sequence
    :start-anchor
    ,@(loop for r in rmap
           if (listp r)
           collect `(:register (:alternation ,@(mapcar 'car r)))
           else collect r)
    :end-anchor))

(defun match-readings* (rmap reading)
  (let* ((regex (make-rmap-regex rmap)))
    (multiple-value-bind (scan groups) (ppcre:scan-to-strings regex reading)
      (if scan
          (loop with gr = (coerce groups 'list)
             for r in rmap
             if (listp r)
               collect (assoc (car gr) r :test 'equal)
               and do (setf gr (cdr gr))
             else
               collect r)))))

(defun match-readings (str reading)
  (let* ((rmap
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
         (match (match-readings* rmap reading)))
    (when match
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
               
