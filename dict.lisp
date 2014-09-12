;; JMdict crawler

(in-package #:ichiran/dict)

(defparameter *jmdict-path* #p"foobar")

(defparameter *connection* '("jmdict" "postgres" "" "localhost"))

(eval-when (:load-toplevel)
  (load (asdf:system-relative-pathname :ichiran "settings.lisp") :if-does-not-exist nil))

(defclass entry ()
  ((seq :reader seq :col-type integer :initarg :seq)
   (content :reader content :col-type string :initarg :content))
  (:metaclass dao-class)
  (:keys seq))

(deftable entry
  (!dao-def))

(defclass kanji-text ()
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (text :reader text :col-type string :initarg :text))
  (:metaclass dao-class)
  (:keys id))

(deftable kanji-text
  (!dao-def)
  (!index 'seq)
  (!index 'text)
  (!foreign 'entry 'seq))

(defclass reading-text ()
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (text :reader text :col-type string :initarg :text))
  (:metaclass dao-class)
  (:keys id))

(deftable reading-text
  (!dao-def)
  (!index 'seq)
  (!index 'text)
  (!foreign 'entry 'seq))

(defun init-tables ()
  (with-connection *connection*
    (let ((tables '(entry kanji-text reading-text)))
      (loop for table in (reverse tables)
         do (query (:drop-table :if-exists table)))
      (loop for table in tables
         do (create-table table)))))

(defun insert-readings (node-list tag table seq)
  (dom:do-node-list (node node-list)
    (let* ((reading-node (dom:item (dom:get-elements-by-tag-name node tag) 0))
           (reading-text (node-text reading-node)))
      (make-dao table :seq seq :text reading-text))))

(defun load-entry (content)
  (let* ((parsed (cxml:parse content (cxml-dom:make-dom-builder)))
         (entseq-node (dom:item (dom:get-elements-by-tag-name parsed "ent_seq") 0))
         (seq (parse-integer (node-text entseq-node))))
    (make-dao 'entry :seq seq :content content)
    (let ((kana-nodes (dom:get-elements-by-tag-name parsed "r_ele"))
          (kanji-nodes (dom:get-elements-by-tag-name parsed "k_ele")))
      (insert-readings kana-nodes "reb" 'reading-text seq)
      (insert-readings kanji-nodes "keb" 'kanji-text seq))))
    
(defun load-jmdict (&optional (path *jmdict-path*))
  (with-connection *connection*
    (klacks:with-open-source (source (cxml:make-source path))
      (loop for cnt from 1
         while (klacks:find-element source "entry")
         do
           (let ((content (klacks:serialize-element source (cxml:make-string-sink))))
             (load-entry content))
         if (zerop (mod cnt 1000)) do (format t "~a entries loaded~%" cnt)
         finally (return cnt)))))

(defun word-readings (word)
  (let* ((kana-seq (query (:select 'seq :from 'reading-text :where (:= 'text word)) :column))
         (readings
          (if kana-seq (list word)
              (let* ((kanji-seq (query (:select 'seq :from 'kanji-text
                                                :where (:= 'text word)) :column)))
                (query (:order-by 
                        (:select 'text :from 'reading-text :where
                                 (:in 'seq (:set kanji-seq)))
                        'id) :column)))))
    (values readings (mapcar #'ichiran:romanize-word readings))))
