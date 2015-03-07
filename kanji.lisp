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
            (make-dao 'reading :type type :text text :kanji-id kanji-id))))
      (dom:do-node-list (node node-nanori)
        (make-dao 'reading :type "ja_na" :text (node-text node) :kanji-id kanji-id))
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
           if (zerop (mod cnt 100)) do (format t "~a entries loaded~%" cnt)
           finally (query "ANALYZE") (format t "~a entries total~%" cnt)))))

