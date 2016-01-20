;; ichiran dictionary module
;; based on JMDict

(in-package #:ichiran/dict)

(defvar *jmdict-path* #p"foobar")

(defvar *jmdict-data* #p"foobar")

(defvar *connection* '("jmdict" "postgres" "" "localhost"))

(load (asdf:system-relative-pathname :ichiran "settings.lisp") :if-does-not-exist nil)

(defgeneric get-kana (obj)
  (:documentation "most popular kana representation"))

(defgeneric get-kanji (obj)
  (:documentation "most popular kanji representation"))

(defgeneric get-text (obj)
  (:documentation "most popular text representation (kanji or kana)")
  (:method (obj) (text obj)))

(defgeneric word-type (obj)
  (:documentation "returns :kanji or :kana or :gap")
  (:method (obj) :gap))

(defclass entry ()
  ((seq :reader seq :col-type integer :initarg :seq)
   (content :reader content :col-type string :initarg :content)
   (root-p :reader root-p :col-type boolean :initform nil :initarg :root-p)
   (n-kanji :accessor n-kanji :col-type integer :initform 0 :initarg :n-kanji)
   (n-kana :accessor n-kana :col-type integer :initform 0 :initarg :n-kana)
   (primary-nokanji :reader primary-nokanji :col-type boolean :initform nil)
   )
  (:metaclass dao-class)
  (:keys seq))

(deftable entry
  (!dao-def))

(defmethod print-object ((obj entry) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a ~a:~a" (seq obj) (n-kanji obj) (n-kana obj))))

(defmethod get-kana ((obj entry))
  (text (car (select-dao 'kana-text (:and (:= 'seq (seq obj)) (:= 'ord 0))))))

(defmethod get-text ((obj entry))
  (text (car (select-dao (if (> (n-kanji obj) 0) 'kanji-text 'kana-text)
                         (:and (:= 'seq (seq obj)) (:= 'ord 0))))))

(defmethod get-kanji ((obj entry))
  (when (> (n-kanji obj) 0)
    (text (car (select-dao 'kanji-text (:and (:= 'seq (seq obj)) (:= 'ord 0)))))))

(defun recalc-entry-stats ()
  (query (:update 'entry :set
                  'n-kanji (:select (:count 'id) :from 'kanji-text :where (:= 'kanji-text.seq 'entry.seq))
                  'n-kana (:select (:count 'id) :from 'kana-text :where (:= 'kana-text.seq 'entry.seq)))))

(defun entry-digest (entry)
  (list (seq entry) (get-text entry) (get-kana entry)))

(defclass simple-text () 
  ((conjugations :accessor word-conjugations :initform nil)
   ))

(defmethod print-object ((obj simple-text) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a ~a" (seq obj) (text obj))))

(defclass kanji-text (simple-text)
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (text :reader text :col-type string :initarg :text)
   (ord :reader ord :col-type integer :initarg :ord)
   (common :reader common :col-type (or db-null integer) :initarg :common)
   (common-tags :reader common-tags :col-type string :initform "" :initarg :common-tags)
   (conjugate-p :reader conjugate-p :col-type boolean :initform t :initarg :conjugate-p)
   (nokanji :reader nokanji :col-type boolean :initform nil :initarg :nokanji)
   (best-kana :accessor best-kana :col-type (or db-null string) :initform :null :initarg :best-kana)
   )
  (:metaclass dao-class)
  (:keys id))

(deftable kanji-text
  (!dao-def)
  (!index 'seq)
  (!index 'ord)
  (!index 'text)
  (!index 'common)
  (!foreign 'entry 'seq :on-delete :cascade))

(defmethod get-kanji ((obj kanji-text))
  (text obj))

(defmethod get-kana :around ((obj kanji-text))
  (let ((bk (best-kana-conj obj)))
    (if (eql bk :null)
        (call-next-method)
        bk)))

(defun kanji-regex (txt)
  (ppcre:create-scanner
   `(:sequence :start-anchor
               ,@(loop for part in (ppcre:split *kanji-regex* txt)
                    for first = t then nil
                    unless first collect '(:GREEDY-REPETITION 0 NIL :EVERYTHING)
                    collect part)
               :end-anchor)))

(defmethod get-kana ((obj kanji-text))
  "old get-kana, used when everything else fails"
  (loop with regex = (kanji-regex (text obj))
     and kts = (select-dao 'kana-text (:= 'seq (seq obj)) 'ord)
     for kt in kts
     for tkt = (text kt)
     if (ppcre:scan regex tkt) do (return tkt)
     finally (return (text (car kts)))))

(defmethod word-type ((obj kanji-text)) :kanji)

(defclass kana-text (simple-text)
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (text :reader text :col-type string :initarg :text)
   (ord :reader ord :col-type integer :initarg :ord)
   (common :reader common :col-type (or db-null integer) :initarg :common)
   (common-tags :reader common-tags :col-type string :initform "" :initarg :common-tags)
   (conjugate-p :reader conjugate-p :col-type boolean :initform t :initarg :conjugate-p)
   (nokanji :reader nokanji :col-type boolean :initform nil :initarg :nokanji)
   (best-kanji :accessor best-kanji :col-type (or db-null string) :initform :null :initarg :best-kanji)
   )
  (:metaclass dao-class)
  (:keys id))

(deftable kana-text
  (!dao-def)
  (!index 'seq)
  (!index 'ord)
  (!index 'text)
  (!index 'common)
  (!foreign 'entry 'seq :on-delete :cascade))

(defmethod get-kana ((obj kana-text))
  (text obj))

(defmethod get-kanji ((obj kana-text))
  (let ((bk (best-kanji-conj obj)))
    (unless (eql bk :null) bk)))

(defmethod word-type ((obj kana-text)) :kana)

(defclass sense ()
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (ord :reader ord :col-type integer :initarg :ord))
  (:metaclass dao-class)
  (:keys id))

(deftable sense
  (!dao-def)
  (!index 'seq)
  (!foreign 'entry 'seq :on-delete :cascade))

(defclass gloss ()
  ((id :reader id :col-type serial)
   (sense-id :reader sense-id :col-type integer :initarg :sense-id)
   (text :reader text :col-type string :initarg :text)
   (ord :reader ord :col-type integer :initarg :ord)
   )
  (:documentation "English meaning")
  (:metaclass dao-class)
  (:keys id))

(defmethod print-object ((obj gloss) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "(~a) ~a" (ord obj) (text obj))))

(deftable gloss
  (!dao-def)
  (!index 'sense-id)
  (!foreign 'sense 'sense-id 'id :on-delete :cascade))

(defclass sense-prop ()
  ((id :reader id :col-type serial)
   (tag :reader tag :col-type string :initarg :tag)
   (sense-id :reader sense-id :col-type integer :initarg :sense-id)
   (text :reader text :col-type string :initarg :text)
   (ord :reader ord :col-type integer :initarg :ord)
   (seq :reader seq :col-type integer :initarg :seq)
   )
  (:documentation "sense properties")
  (:metaclass dao-class)
  (:keys id))
  
(defmethod print-object ((obj sense-prop) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a:~a" (tag obj) (text obj))))

(deftable sense-prop
  (!dao-def)
  (!index 'sense-id 'tag)
  (!index 'tag 'text)
  (!index 'seq 'tag 'text)
  (!foreign 'entry 'seq :on-delete :cascade)
  (!foreign 'sense 'sense-id 'id :on-delete :cascade))

(defclass restricted-readings ()
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (reading :reader reading :col-type string :initarg :reading)
   (text :reader text :col-type string :initarg :text))
  (:metaclass dao-class)
  (:keys id))

(defmethod print-object ((obj restricted-readings) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a -> ~a" (reading obj) (text obj))))

(deftable restricted-readings
  (!dao-def)
  (!index 'seq 'reading)
  (!foreign 'entry 'seq :on-delete :cascade))

(defclass conjugation ()
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (from :reader seq-from :col-type integer :initarg :from)
   (via :reader seq-via :col-type (or integer db-null) :initform :null :initarg :via)
   )
  (:documentation "conjugation link")
  (:metaclass dao-class)
  (:keys id))

(deftable conjugation
  (!dao-def)
  (!index 'seq)
  (!index 'from)
  (!foreign 'entry 'seq :on-delete :cascade)
  (!foreign 'entry 'from 'seq :on-delete :cascade))

(defmethod print-object ((obj conjugation) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (let ((via (seq-via obj)))
      (if (eql via :null)
          (format stream "~a -> ~a" (seq-from obj) (seq obj))
          (format stream "~a -> ~a -> ~a" (seq-from obj) (seq-via obj) (seq obj))))))

(defclass conj-prop ()
  ((id :reader id :col-type serial)
   (conj-id :reader conj-id :col-type integer :initarg :conj-id)
   (conj-type :reader conj-type :col-type integer :initarg :conj-type)
   (pos :reader pos :col-type string :initarg :pos)
   (neg :reader conj-neg :col-type (or db-null boolean) :initarg :neg)
   (fml :reader conj-fml :col-type (or db-null boolean) :initarg :fml))
  (:metaclass dao-class)
  (:keys id))

(deftable conj-prop
  (!dao-def)
  (!index 'conj-id)
  (!foreign 'conjugation 'conj-id 'id :on-delete :cascade))

(defun conj-info-short (obj)
  (format nil "[~a] ~a~[ Affirmative~; Negative~]~[ Plain~; Formal~]"
          (pos obj) 
          (get-conj-description (conj-type obj))
          (case (conj-neg obj) ((nil) 0) ((t) 1))
          (case (conj-fml obj) ((nil) 0) ((t) 1))
          ))

(defun conj-prop-json (obj)
  (let ((js (jsown:new-js
              ("pos" (pos obj))
              ("type" (get-conj-description (conj-type obj)))))
        (neg (conj-neg obj))
        (fml (conj-fml obj)))
    (unless (or (not neg) (eql neg :null))
      (jsown:extend-js js ("neg" neg)))
    (unless (or (not fml) (eql fml :null))
      (jsown:extend-js js ("fml" fml)))
    js))

(defun delete-duplicate-props ()
  (query "DELETE FROM conj_prop cp1 USING conj_prop cp2
          WHERE cp1.id < cp2.id AND cp1.conj_id = cp2.conj_id
            AND cp1.conj_type = cp2.conj_type
            AND cp1.pos = cp2.pos
            AND cp1.neg IS NOT DISTINCT FROM cp2.neg
            AND cp1.fml IS NOT DISTINCT FROM cp2.fml"))

(defmethod print-object ((obj conj-prop) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (princ (conj-info-short obj) stream)))

(defclass conj-source-reading ()
  ((id :reader id :col-type serial)
   (conj-id :reader conj-id :col-type integer :initarg :conj-id)
   (text :reader text :col-type string :initarg :text)
   (source-text :reader source-text :col-type string :initarg :source-text)
   )
  (:metaclass dao-class)
  (:keys id))

(deftable conj-source-reading
  (!dao-def)
  (!index 'conj-id 'text)
  (!foreign 'conjugation 'conj-id 'id :on-delete :cascade))

(defmethod print-object ((obj conj-source-reading) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a -> ~a" (source-text obj) (text obj))))

(defstruct conj-data seq from via prop src-map)

(defun get-conj-data (seq &optional from/conj-ids)
  "from/conj-ids can be either from which word to find conjugations or a list of conj-ids"
  (loop for conj in (cond
                      ((eql from/conj-ids :root) nil)
                      ((null from/conj-ids)
                       (select-dao 'conjugation (:= 'seq seq)))
                      ((listp from/conj-ids)
                       (select-dao 'conjugation (:and (:= 'seq seq) (:in 'id (:set from/conj-ids)))))
                      (t (select-dao 'conjugation (:and (:= 'seq seq) (:= 'from from/conj-ids)))))
       for src-map = (query (:select 'text 'source-text :from 'conj-source-reading
                                     :where (:= 'conj-id (id conj))))
       nconcing (loop for prop in (select-dao 'conj-prop (:= 'conj-id (id conj)))
                     collect (make-conj-data :seq (seq conj) :from (seq-from conj)
                                             :via (let ((via (seq-via conj)))
                                                    (if (eql via :null) nil via))
                                             :prop prop
                                             :src-map src-map
                                             ))))

(defun get-original-text (conj-datas texts)
  (unless (listp texts)
    (setf texts (list texts)))
  (unless (listp conj-datas)
    (setf conj-datas (list conj-datas)))
  (loop for conj-data in conj-datas
       nconc
       (let ((src-text (loop for (txt src-txt) in (conj-data-src-map conj-data)
                          if (find txt texts :test 'equal) collect src-txt)))
         (if (not (conj-data-via conj-data))
             src-text
             (let ((new-cd (get-conj-data (conj-data-via conj-data) (conj-data-from conj-data))))
                     (get-original-text new-cd src-text))))))


(defun init-tables ()
  (with-connection *connection*
    (let ((tables '(entry kanji-text kana-text sense gloss sense-prop conjugation conj-prop
                    conj-source-reading restricted-readings)))
      (loop for table in (reverse tables)
         do (query (:drop-table :if-exists table)))
      (loop for table in tables
         do (create-table table)))))

;; Taken from webgunk so that ichiran doesn't depend on it
;; strip-whitespace option is removed as it doesn't look necessary in this case
(defun node-text (node &rest args &key test)
  (let (values result)
    (when (or (not test) (funcall test node))
      (dom:do-node-list (node (dom:child-nodes node))
        (let ((val (case (dom:node-type node)
                     (:element (apply #'node-text node args))
                     (:text (dom:node-value node)))))
          (push val values))))
    (setf result (apply #'concatenate 'string (nreverse values)))))

(defmacro do-node-list-ord ((ord-var node-var node-list) &body body)
  `(let ((,ord-var 0))
     (dom:do-node-list (,node-var ,node-list)
       ,@body
       (incf ,ord-var))))

(defun insert-readings (node-list tag table seq pri)
  (let (to-add primary-nokanji)
    (do-node-list-ord (ord node node-list)
      (let* ((reading-node (dom:item (dom:get-elements-by-tag-name node tag) 0))
             (reading-text (node-text reading-node))
             (common :null) (skip nil) (nokanji nil)
             (pri-tags nil))
        (dom:do-node-list (node (dom:get-elements-by-tag-name node "re_inf"))
          (when (equal (node-text node) "ok")
            (setf skip t)))
        (unless skip
          (when (< 0 (dom:length (dom:get-elements-by-tag-name node "re_nokanji")))
            (setf nokanji t))
          (dom:do-node-list (node (dom:get-elements-by-tag-name node "re_restr"))
            (let ((restr (node-text node)))
              (make-dao 'restricted-readings :seq seq :reading reading-text :text restr)))
          (dom:do-node-list (node (dom:get-elements-by-tag-name node pri))
            (let ((pri-tag (node-text node)))
              (push pri-tag pri-tags)
              (if (eql common :null) (setf common 0))
              (when (alexandria:starts-with-subseq "nf" pri-tag)
                (setf common (parse-integer pri-tag :start 2)))))
          (push (list reading-text common nokanji (format nil "~{[~a]~}" (nreverse pri-tags)))
                to-add))))
    (loop for (reading-text common nokanji pri-tags) in (nreverse to-add)
       for ord from 0
       when nokanji do (setf primary-nokanji t)
       do (make-dao table :seq seq :text reading-text :ord ord :common common :nokanji nokanji 
                    :common-tags pri-tags))
    (when primary-nokanji
      (query (:update 'entry :set 'primary-nokanji t :where (:= 'seq seq))))))
         
  
(defun insert-sense-traits (sense-node tag sense-id seq)
  (do-node-list-ord (ord node (dom:get-elements-by-tag-name sense-node tag))
    (make-dao 'sense-prop :sense-id sense-id :tag tag :text (node-text node) :ord ord :seq seq)))

(defun insert-senses (node-list seq)
  (do-node-list-ord (ord node node-list)
    (let ((sense-id (id (make-dao 'sense :seq seq :ord ord))))
      (do-node-list-ord (ord node (dom:get-elements-by-tag-name node "gloss"))
        (make-dao 'gloss :sense-id sense-id :text (node-text node) :ord ord))
      (insert-sense-traits node "pos" sense-id seq)
      (insert-sense-traits node "misc" sense-id seq)
      (insert-sense-traits node "dial" sense-id seq))))

(defun load-entry (content)
  (let* ((parsed (cxml:parse content (cxml-dom:make-dom-builder)))
         (entseq-node (dom:item (dom:get-elements-by-tag-name parsed "ent_seq") 0))
         (seq (parse-integer (node-text entseq-node))))
    (make-dao 'entry :seq seq :content content :root-p t)
    (let* ((kanji-nodes (dom:get-elements-by-tag-name parsed "k_ele"))
           (kana-nodes (dom:get-elements-by-tag-name parsed "r_ele"))
           (sense-nodes (dom:get-elements-by-tag-name parsed "sense")))
      (insert-readings kanji-nodes "keb" 'kanji-text seq "ke_pri")
      (insert-readings kana-nodes "reb" 'kana-text seq "re_pri")
      (insert-senses sense-nodes seq))))

(defun fix-entities (source)
  "replaces entity definitions with abbreviations"
  (let ((entity-hash (cxml::dtd-gentities (cxml::dtd (slot-value source 'cxml::context)))))
    (maphash 
     (lambda (name entdef)
       (unless (member name '("lt" "gt" "amp" "apos" "quot") :test 'equal)
         (setf (cxml::entdef-value (cdr entdef)) name)))
     entity-hash)))
    
(defun load-jmdict (&key (path *jmdict-path*) (load-extras t))
  (init-tables)
  (with-connection *connection*
    (klacks:with-open-source (source (cxml:make-source path))
      (klacks:find-element source "JMdict")
      (fix-entities source)
      (loop for cnt from 1 ;; to 1000
         while (klacks:find-element source "entry")
         do
           (let ((content (klacks:serialize-element source (cxml:make-string-sink))))
             (load-entry content))
         if (zerop (mod cnt 1000)) do (format t "~a entries loaded~%" cnt)
         finally (recalc-entry-stats) (query "ANALYZE") (format t "~a entries total~%" cnt)))
    (when load-extras (load-extras))))

(defun load-extras ()
  (format t "Loading conjugations...~%")
  (load-conjugations)
  (format t "Loading secondary conjugations...~%")
  (load-secondary-conjugations)
  (add-errata)
  (recalc-entry-stats)
  (query "ANALYZE"))

(defun drop-extras ()
  (query (:delete-from 'conj-prop))
  (query (:delete-from 'conj-source-reading))
  (query (:delete-from 'conjugation))
  (query (:delete-from 'entry :where (:not 'root-p)))
  )

;;; conjugations generator (warning: terrible code ahead)

(defmacro csv-hash (hash-name (filename &key skip-first ((:errata errata-fn))) loader-opts &rest accessor-opts-list)
  (let ((base-name (string-trim "*" hash-name))
        (forms (list `(defparameter ,hash-name nil)))
        (loader-opts-length (length loader-opts)) ;;([loader-name] row-def row-key value-form)
        loader-name
        (row-count-var (gensym "ROW"))
        )
    (assert (member loader-opts-length '(3 4)))
    (setf loader-name
          (if (= loader-opts-length 4)
              (pop loader-opts)
              (intern (concatenate 'string (symbol-name :load-) base-name))))
    (destructuring-bind (row-def row-key-form value-form) loader-opts
      (push
       `(defun ,loader-name ()
          (setf ,hash-name (make-hash-table :test 'equal))
          (loop :for ,row-def :in (cl-csv:read-csv 
                                   (merge-pathnames *jmdict-data* ,filename)
                                   :separator #\Tab :skip-first-p ,skip-first)
             :for ,row-count-var :from 0
             :do (setf (gethash ,row-key-form ,hash-name) ,value-form))
          ,(when errata-fn `(funcall ,errata-fn ,hash-name)))
       forms))
    (loop with accessor-name
       for accessor-opts in accessor-opts-list
       for accessor-opts-length = (length accessor-opts) ;; ([accessor-name] val-var val-var-form)
       for param = (gensym "KEY")
       do (assert (member accessor-opts-length '(2 3)))
         (setf accessor-name
               (if (= accessor-opts-length 3)
                   (pop accessor-opts)
                   (intern (concatenate 'string (symbol-name :get-) base-name))))
         (destructuring-bind (val-var val-var-form) accessor-opts
           (push
            `(defun ,accessor-name (,param)
               (unless ,hash-name (,loader-name))
               (let ((,val-var (gethash ,param ,hash-name)))
                 ,val-var-form))
            forms)))
    `(progn ,@(nreverse forms))))
               
(csv-hash *pos-index* ("kwpos.csv")
          ((pos-id pos description) pos (cons (parse-integer pos-id) description))
          (val (car val)))

(csv-hash *pos-by-index* ("kwpos.csv")
          ((pos-id pos description) (parse-integer pos-id) pos)
          (get-pos val val))

(csv-hash *conj-description* ("conj.csv" :skip-first t
                                         :errata 'errata-conj-description-hook)
          ((conj-id description) (parse-integer conj-id) description)
          (val val))

(defstruct (conjugation-rule
             (:conc-name cr-)
             (:constructor make-conjugation-rule (pos conj neg fml onum stem okuri euphr euphk)))
  pos conj neg fml onum stem okuri euphr euphk)


(csv-hash *conj-rules* ("conjo.csv" :skip-first t
                                    :errata 'errata-conj-rules-hook)
          ((pos-id conj-id neg fml onum stem okuri euphr euphk pos2)
           (parse-integer pos-id)
           (let ((pos (parse-integer pos-id)))
             (cons (make-conjugation-rule pos
                                          (parse-integer conj-id)
                                          (case (char neg 0) (#\t t) (#\f nil))
                                          (case (char fml 0) (#\t t) (#\f nil))
                                          (parse-integer onum)
                                          (parse-integer stem)
                                          okuri euphr euphk)
                   (gethash pos *conj-rules* nil))))
          (val (reverse val)))


(defun construct-conjugation (word rule)
  (let* ((iskana (test-word (subseq word (max 0 (- (length word) 2))) :kana))
         (euphr (cr-euphr rule))
         (euphk (cr-euphk rule))
         (stem (+ (cr-stem rule)
                  (if (or (and iskana (> (length euphr) 0))
                          (and (not iskana) (> (length euphk) 0)))
                      1 0))))
    (concatenate 'string (subseq word 0 (- (length word) stem))
                 (if iskana euphr euphk)
                 (cr-okuri rule))))

(defun conjugate-word (word pos)
  (let* ((pos-id (get-pos-index pos))
         (rules (get-conj-rules pos-id)))
    (loop for rule in rules
         collect (cons rule (construct-conjugation word rule)))))

(defun get-all-readings (seq)
  (query (:union
          (:select 'text :from 'kanji-text :where (:= 'seq seq))
          (:select 'text :from 'kana-text :where (:= 'seq seq)))
         :column))

(defmethod common ((obj entry) &aux (seq (seq obj)))
  (query (:select (:max 'common) :from (:as (:union
          (:select 'common :from 'kanji-text :where (:= 'seq seq))
          (:select 'common :from 'kana-text :where (:= 'seq seq))) 'tmp))
         :single))

(defparameter *do-not-conjugate* '("n" "vs" "adj-na"))

(defparameter *pos-with-conj-rules*
 '("adj-i" "adj-ix" "cop-da" "v1" "v1-s" "v5aru" 
   "v5b" "v5g" "v5k" "v5k-s" "v5m" "v5n" "v5r" "v5r-i" "v5s"
   "v5t" "v5u" "v5u-s" "vk" "vs-s" "vs-i"))

(defparameter *secondary-conjugation-types-from* '(5 6 7 8))

(defparameter *secondary-conjugation-types* '(2 3 4 9 10 11 12 13))

(defun conjugate-entry-inner (seq &key conj-types as-posi)
  (let ((posi (or as-posi (query (:select 'text :distinct :from 'sense-prop
                                          :where (:and (:= 'tag "pos") (:= 'seq seq))) :column))))
    (loop with conj-matrix = (make-hash-table :test 'equal) ;; (cons pos-id conj-id) -> 2x2 array
       for pos in posi
       for pos-id = (get-pos-index pos)
       for rules = (get-conj-rules pos-id)
       if (and rules (not (member pos *do-not-conjugate* :test 'equal)))
         do (loop 
               for (reading ord kanji-flag) in (query (:union (:select 'text 'ord 1 :from 'kanji-text
                                                                       :where (:and (:= 'seq seq) 'conjugate-p))
                                                              (:select 'text 'ord 0 :from 'kana-text
                                                                       :where (:and (:= 'seq seq) 'conjugate-p))))
               do (loop for rule in rules
                        for conj-id = (cr-conj rule)
                     when (or (not conj-types)
                              (member conj-id conj-types))
                     do (let ((key (list pos-id conj-id))
                              (conj-text (construct-conjugation reading rule)))
                          (unless (gethash key conj-matrix)
                            (setf (gethash key conj-matrix)
                                  (make-array '(2 2) :initial-element nil)))
                          (push (list conj-text kanji-flag reading ord (cr-onum rule))
                                (aref (gethash key conj-matrix)
                                   (if (cr-neg rule) 1 0)
                                   (if (cr-fml rule) 1 0))))))
         finally (return conj-matrix))))

(defun conjugate-entry-outer (seq* &key via conj-types as-posi)
  (let* ((seq (or via seq*))
         (conj-matrix (conjugate-entry-inner seq :conj-types conj-types :as-posi as-posi))
         (original-readings (get-all-readings seq))
         (max-seq (query (:select (:max 'seq) :from 'entry) :single))
         (next-seq (1+ max-seq)))
    (loop for (pos-id conj-id) being the hash-key of conj-matrix using (hash-value matrix)
       for ignore-neg = (not (or (aref matrix 1 0) (aref matrix 1 1)))
       for ignore-fml = (not (or (aref matrix 0 1) (aref matrix 1 1)))
       for pos = (get-pos pos-id)
         do (loop for ii from 0 below 4
               for neg = (>= ii 2)
               for fml = (oddp ii)
               for readings = (remove-if (lambda (item) 
                                           (member (car item) original-readings :test 'equal))
                                         (row-major-aref matrix ii))
               when readings
               do (when (insert-conjugation readings :seq next-seq :via via
                                            :from seq* :pos pos
                                            :conj-type conj-id
                                            :neg (if ignore-neg :null neg)
                                            :fml (if ignore-fml :null fml))
                    (incf next-seq))))))

(defun lex-compare (predicate)
  "Only can sort sequences of equal length"
  (lambda (seq1 seq2)
    (block nil
      (map nil 
           (lambda (e1 e2) 
             (cond ((funcall predicate e1 e2) (return t))
                   ((funcall predicate e2 e1) (return nil))))
           seq1 seq2))))

(defun insert-conjugation (readings &key seq from pos conj-type neg fml via)
  "returns true if new entry is created, nil otherwise"
  (loop for (reading kanji-flag orig-reading) in (sort readings (lex-compare #'<) :key #'cdddr)
     collect (list reading orig-reading) into source-readings
     if (= kanji-flag 1) collect reading into kanji-readings
     else collect reading into kana-readings
     finally
       (unless kana-readings (return nil))
       (let* ((kanji-readings (remove-duplicates kanji-readings :test 'equal))
              (kana-readings (remove-duplicates kana-readings :test 'equal))
              (seq-candidates
               (if kanji-readings
                   (query (:intersect 
                           (:select 'seq :from 'kanji-text
                                    :where (:in 'text (:set kanji-readings))
                                    :group-by 'seq
                                    :having (:= (:count 'id) (length kanji-readings)))
                           (:select 'seq :from 'kana-text
                                 :where (:in 'text (:set kana-readings))
                                 :group-by 'seq
                                 :having (:= (:count 'id) (length kana-readings))))
                          :column)
                   (query (:select 'r.seq
                                   :from (:as 'kana-text 'r)
                                   :left-join (:as 'kanji-text 'k) :on (:= 'r.seq 'k.seq)
                                   :where (:and (:is-null 'k.text)
                                                (:in 'r.text (:set kana-readings)))
                                   :group-by 'r.seq
                                   :having (:= (:count 'r.id) (length kana-readings)))
                          :column))))
         (when (or (member from seq-candidates) (member via seq-candidates))
           (return nil))
         (if seq-candidates
             (setf seq (car seq-candidates))
             (progn
               (make-dao 'entry :seq seq :content "")
               (let ((conjugate-p (when (member conj-type *secondary-conjugation-types-from*) t)))
                 (loop for kr in kanji-readings
                    for ord from 0
                    do (make-dao 'kanji-text :seq seq :text kr :ord ord :common :null :conjugate-p conjugate-p))
                 (loop for kr in kana-readings
                    for ord from 0
                    do (make-dao 'kana-text :seq seq :text kr :ord ord :common :null :conjugate-p conjugate-p)))))
         
         (let* ((old-conj (if via
                              (select-dao 'conjugation (:and (:= 'from from) (:= 'seq seq) (:= 'via via)))
                              (select-dao 'conjugation (:and (:= 'from from) (:= 'seq seq) (:is-null 'via)))))
                (conj (or (car old-conj) (make-dao 'conjugation :seq seq :from from :via (or via :null))))
                (conj-id (id conj)))
           (make-dao 'conj-prop :conj-id conj-id :conj-type conj-type :pos pos :neg neg :fml fml)
           
           (let* ((old-csr (when old-conj (query (:select 'text 'source-text :from 'conj-source-reading :where (:= 'conj-id conj-id)))))
                  (source-readings (remove-duplicates (set-difference source-readings old-csr :test 'equal) :test 'equal)))
             (loop for (text source-text) in source-readings
                  do (make-dao 'conj-source-reading :conj-id conj-id :text text :source-text source-text))))

         (return (not seq-candidates)))))

(defun load-conjugations ()
  (with-connection *connection*
    (let ((seqs (query (:select 'seq :distinct :from 'sense-prop
                                :where (:and (:= 'tag "pos")
                                             (:in 'text (:set *pos-with-conj-rules*))))
                       :column)))
      (loop for cnt from 1
           for seq in seqs
           do (conjugate-entry-outer seq)
           if (zerop (mod cnt 500)) do (format t "~a entries processed~%" cnt)))))


(defun load-secondary-conjugations ()
  (let ((to-conj (query-dao 'conjugation 
                            (:select 'conj.* :distinct :from (:as 'conjugation 'conj)
                                     :left-join 'conj-prop :on (:= 'conj.id 'conj-prop.conj-id)
                                     :where (:and (:in 'conj-prop.conj-type (:set *secondary-conjugation-types-from*))
                                                  (:not (:in 'conj-prop.pos (:set "vs-i" "vs-s")))
                                                  (:or (:not 'neg) (:is-null 'neg))
                                                  (:or (:not 'fml) (:is-null 'fml)))))))
    (loop for cnt from 1
       for conj in to-conj
       do (conjugate-entry-outer (seq-from conj) :via (seq conj) :as-posi '("v1")
                                 :conj-types *secondary-conjugation-types*)
       if (zerop (mod cnt 1000)) do (format t "~a entries processed~%" cnt))
    ))

;;; end conjugations

;;; precalculating kanji/kana links

(defgeneric set-reading (obj)
  (:documentation "find and set best associated reading (kana/kanji) for this object"))


(defmethod set-reading ((obj kanji-text))
  (let* ((seq (seq obj))
         (cur-best (best-kana obj))
         (restricted (query (:select 'reading 'text :from 'restricted-readings :where (:= 'seq seq)))))
    (loop for reading in (select-dao 'kana-text (:= 'seq seq) 'ord)
         for rtext = (text reading)
         for restr = (loop for (rt kt) in restricted when (equal rtext rt) collect kt)
         when (and (not (nokanji reading))
                   (or (not restr)
                       (member (text obj) restr :test 'equal)))
         do (unless (equal cur-best (text reading))
              (setf (best-kana obj) (text reading)) (update-dao obj))
            (return))
    (unless (equal cur-best :null)
      (setf (best-kana obj) :null) (update-dao obj))
    ))

(defmethod set-reading ((obj kana-text))
  (when (nokanji obj)
    (return-from set-reading))
  (let* ((seq (seq obj))
         (cur-best (best-kanji obj))
         (rtext (text obj))
         (restricted (query (:select 'text :from 'restricted-readings
                                     :where (:and (:= 'seq seq)
                                                  (:= 'reading rtext)))
                            :column))
         (kanji-list (if restricted
                         (select-dao 'kanji-text (:and (:= 'seq seq)
                                                       (:in 'text (:set restricted)))
                                     'ord)
                         (select-dao 'kanji-text (:= 'seq seq) 'ord))))
    (cond (kanji-list
           (let ((ktext (text (car kanji-list))))
             (unless (equal cur-best ktext)
               (setf (best-kanji obj) ktext)
               (update-dao obj))))
          (t
           (unless (equal cur-best :null)
             (setf (best-kanji obj) :null)
             (update-dao obj))))))

;; (defgeneric set-reading-conj (obj)
;;   (:documentation "find and set best associated reading (kana/kanji) for this non-root object"))

(defun best-kana-conj (obj)
  (cond ((not (eql (best-kana obj) :null)) (best-kana obj))
        (t (let* ((parents (query (:select 'kt.id 'conj.id
                                           :from (:as 'kanji-text 'kt)
                                           (:as 'conj-source-reading 'csr)
                                           (:as 'conjugation 'conj)
                                           :where (:and
                                                   (:= 'conj.seq (seq obj))
                                                   (:= 'conj.id 'csr.conj-id)
                                                   (:= 'csr.text (text obj))
                                                   (:= 'kt.seq (:case ((:not-null 'conj.via) 'conj.via)
                                                                 (:else 'conj.from)))
                                                   (:= 'kt.text 'csr.source-text))))))
             (loop for (pid cid) in parents
                  for parent-bk = (best-kana-conj (get-dao 'kanji-text pid))
                  unless (eql parent-bk :null)
                  do (let ((readings (query (:select 'text :from 'conj-source-reading
                                                     :where (:and (:= 'conj-id cid)
                                                                  (:= 'source-text parent-bk)))
                                            :column)))
                       (when readings
                         (return
                           (if (= (length readings) 1)
                               (car readings)
                               (loop with regex = (kanji-regex (text obj))
                                  for rd in readings
                                  if (ppcre:scan regex rd) do (return rd)
                                  finally (return (car readings)))))))
                  finally (return :null))))))


(defun best-kanji-conj (obj)
  (cond ((not (eql (best-kanji obj) :null)) (best-kanji obj))
        ((or (nokanji obj) (= (n-kanji (get-dao 'entry (seq obj))) 0))
         :null)
        (t (let* ((parents (query (:select 'kt.id 'conj.id
                                           :from (:as 'kana-text 'kt)
                                           (:as 'conj-source-reading 'csr)
                                           (:as 'conjugation 'conj)
                                           :where (:and
                                                   (:= 'conj.seq (seq obj))
                                                   (:= 'conj.id 'csr.conj-id)
                                                   (:= 'csr.text (text obj))
                                                   (:= 'kt.seq (:case ((:not-null 'conj.via) 'conj.via)
                                                                 (:else 'conj.from)))
                                                   (:= 'kt.text 'csr.source-text))))))
             (loop for (pid cid) in parents
                  for parent-bk = (best-kanji-conj (get-dao 'kana-text pid))
                  unless (eql parent-bk :null)
                  do (let ((readings (query (:select 'text :from 'conj-source-reading
                                                     :where (:and (:= 'conj-id cid)
                                                                  (:= 'source-text parent-bk)))
                                            :column)))
                       (when readings
                         (return (car readings))))
                  finally (return :null))))))
                    

;; (defmethod set-reading-conj ((obj kanji-text))
;;   (let ((cur-best (best-kana obj))
;;         (bk (best-kana-conj obj)))
;;     (unless (equal cur-best bk)
;;       (setf (best-kana obj) bk)
;;       (update-dao obj))))

;; (defmethod set-reading-conj ((obj kana-text))
;;   (let ((cur-best (best-kanji obj))
;;         (bk (best-kanji-conj obj)))
;;     (unless (equal cur-best bk)
;;       (setf (best-kanji obj) bk)
;;       (update-dao obj))))

(defun load-best-readings (&key reset)
  (with-connection *connection*
    (when reset
      (query (:update 'kanji-text :set 'best-kana :null))
      (query (:update 'kana-text :set 'best-kanji :null)))
    (loop for kanji in (query-dao 'kanji-text
                                  (:select 'kt.* :from (:as 'kanji-text 'kt) 'entry
                                           :where (:and (:= 'kt.seq 'entry.seq)
                                                        (:is-null 'kt.best-kana)
                                                        'root-p)))
       do (set-reading kanji))
    (loop for kana in (query-dao 'kana-text
                                 (:select 'kt.* :from (:as 'kana-text 'kt) 'entry
                                          :where (:and (:= 'kt.seq 'entry.seq)
                                                       (:is-null 'kt.best-kanji)
                                                       'root-p)))
       do (set-reading kana))))

;;;

(defun find-word (word &key root-only)
  (let ((table (if (test-word word :kana) 'kana-text 'kanji-text)))
    (if root-only
        (query-dao table (:select 'wt.* :from (:as table 'wt) :inner-join 'entry :on (:= 'wt.seq 'entry.seq)
                                  :where (:and (:= 'text word)
                                               'root-p)))
        (select-dao table (:= 'text word)))))

(defun word-readings (word)
  (let* ((kana-seq (query (:select 'seq :from 'kana-text :where (:= 'text word)) :column))
         (readings
          (if kana-seq (list word)
              (let* ((kanji-seq (query (:select 'seq :from 'kanji-text
                                                :where (:= 'text word)) :column)))
                (query (:order-by 
                        (:select 'text :from 'kana-text :where
                                 (:in 'seq (:set kanji-seq)))
                        'id) :column)))))
    (values readings (mapcar #'ichiran:romanize-word readings))))

;; Proxy text (kanji-text or kana-text with changed spelling)

(defclass proxy-text (simple-text)
  ((text :reader text :initarg :text)
   (kana :reader get-kana :initarg :kana)
   (source :reader source :initarg :source)))

(defgeneric true-text (obj)
  (:documentation "Returns original text for reading")
  (:method (obj) (text obj))
  (:method ((obj proxy-text)) (true-text (source obj))))

(defmethod word-conjugations ((obj proxy-text))
  (word-conjugations (source obj)))

(defmethod (setf word-conjugations) (value (obj proxy-text))
  (setf (word-conjugations (source obj)) value))

(defmethod seq ((obj proxy-text))
  (seq (source obj)))

(defmethod common ((obj proxy-text))
  (common (source obj)))

(defmethod ord ((obj proxy-text))
  (ord (source obj)))

(defmethod nokanji ((obj proxy-text))
  (nokanji (source obj)))

(defmethod word-type ((obj proxy-text))
  (word-type (source obj)))

;; Compound words (2 or more words squished together)

(defclass compound-text ()
  ((text :reader text :initarg :text)
   (kana :reader get-kana :initarg :kana)
   (primary :reader primary :initarg :primary)
   (words :reader words :initarg :words)
   (score-mod :reader score-mod :initarg :score-mod)
   ))

(defmethod seq ((obj compound-text))
  (mapcar #'seq (words obj)))

(defmethod print-object ((obj compound-text) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a ~a" (seq obj) (text obj))))

(defmethod common ((obj compound-text))
  (common (primary obj)))

(defmethod ord ((obj compound-text))
  (ord (primary obj)))

(defmethod word-type ((obj compound-text)) (word-type (primary obj)))

(defgeneric adjoin-word (word1 word2 &key text kana score-mod)
  (:documentation "make compound word from 2 words"))

(defmethod adjoin-word :around (word1 word2 &key text kana score-mod)
  (call-next-method word1 word2
                    :text (or text (concatenate 'string (get-text word1) (get-text word2)))
                    :kana (or kana (concatenate 'string (get-kana word1) (get-kana word2)))
                    :score-mod (or score-mod 0)))

(defmethod adjoin-word ((word1 simple-text) (word2 simple-text) &key text kana score-mod)
  (make-instance 'compound-text
                 :text text :kana kana :primary word1 :words (list word1 word2) :score-mod score-mod))

(defmethod adjoin-word ((word1 compound-text) (word2 simple-text) &key text kana score-mod)
  (with-slots ((s-text text) (s-kana kana) (s-words words) (s-score-mod score-mod)) word1
    (setf s-text text s-kana kana
          s-words (append s-words (list word2))
          s-score-mod (+ s-score-mod score-mod)))
  word1)

(defgeneric word-conj-data (word)
  (:documentation "conjugation data for word"))

(defmethod word-conj-data ((word simple-text))
  (get-conj-data (seq word) (word-conjugations word)))

(defmethod word-conj-data ((word compound-text))
  (word-conj-data (car (last (words word)))))

(defmethod word-conjugations ((word compound-text))
  (word-conjugations (car (last (words word)))))

(defmethod (setf word-conjugations) (value (word compound-text))
  (setf (word-conjugations (car (last (words word)))) value))

(defstruct segment
  start end word (score nil) (info nil) (top nil)) ;; (accum 0) (path nil)

(defun length-multiplier (length power len-lim)
  "len^power until len-lim, goes linear after"
  (cond ((<= length len-lim) (expt length power))
        (t (* length (expt len-lim (1- power))))))

(defparameter *length-coeff-sequences*
  '((:strong 1 8 24 40 60)
    (:weak 1 4 9 16 25 36)
    (:tail 4 9 16 24)))

(defun length-multiplier-coeff (length class)
  (let ((coeffs (assoc class *length-coeff-sequences*)))
    (if (< length (length coeffs))
        (elt coeffs length)
        (* length (/ (car (last coeffs)) (1- (length coeffs)))))))

(defun kanji-break-penalty (score)
  (ceiling score 2))

;; *skip-words* *final-prt* *weak-conj-types* *skip-conj-forms* are defined in dict-errata.lisp

(defun calc-score (reading &key final use-length (score-mod 0) kanji-break)
  (when (typep reading 'compound-text)
    (multiple-value-bind (score info) (calc-score (primary reading)
                                                  :use-length (mora-length (text reading))
                                                  :score-mod (score-mod reading))
      (setf (getf info :conj) (word-conj-data reading))
      (when kanji-break (setf score (kanji-break-penalty score)))
      (return-from calc-score
        (values score info))))

  (let* ((score 1) prop-score
         (kanji-p (eql (word-type reading) :kanji))
         (katakana-p (and (not kanji-p) (> (count-char-class (text reading) :katakana-uniq) 0)))
         (text (text reading))
         (n-kanji (count-char-class text :kanji))
         ;(kanji-prefix (kanji-prefix text))
         (len (max 1 (mora-length text)))
         (seq (seq reading))
         (ord (ord reading))
         (entry (get-dao 'entry seq))
         (conj-only (let ((wc (word-conjugations reading))) (and wc (not (eql wc :root)))))
         (root-p (and (not conj-only) (root-p entry)))
         (conj-data (word-conj-data reading))
         (conj-of (mapcar #'conj-data-from conj-data))
         (secondary-conj-p (and conj-data (every #'conj-data-via conj-data)))
         (conj-types (unless root-p (mapcar (lambda (cd) (conj-type (conj-data-prop cd))) conj-data)))
         (conj-types-p (or root-p (set-difference conj-types *weak-conj-types*)))
         (seq-set (cons seq conj-of)) ;;(if root-p (list seq) (cons seq conj-of)))
         (prefer-kana
          (select-dao 'sense-prop (:and (:in 'seq (:set (if (and root-p (not use-length)) (list seq) seq-set)))
                                        (:= 'tag "misc") (:= 'text "uk"))))
         (posi (query (:select 'text :distinct :from 'sense-prop
                               :where (:and (:in 'seq (:set seq-set)) (:= 'tag "pos"))) :column))
         (common (if conj-only :null (common reading)))
         (common-of common)
         (common-p (not (eql common :null)))
         (particle-p (member "prt" posi :test 'equal))
         (semi-final-particle-p (member seq *semi-final-prt*))
         (pronoun-p (member "pn" posi :test 'equal))
         (cop-da-p (member "cop-da" posi :test 'equal))
         (long-p (> len
                    (if (or (and kanji-p (not prefer-kana)
                                 (or root-p (and use-length (member 13 conj-types))))
                            (and common-p (< 0 common 10)))
                        2 3)))
         (no-common-bonus (or particle-p (and (not long-p) (equal posi '("int")))))
         (primary-p nil))
    (when (or (intersection seq-set *skip-words*)
              (and (not final) (member seq *final-prt*))
              (and (not root-p) (skip-by-conj-data conj-data)))
      (return-from calc-score 0))
    (when (and conj-data (not (and (= ord 0) common-p)))
      (let* ((table (if kanji-p 'kanji-text 'kana-text))
             (src-text (get-original-text conj-data (true-text reading)))
             (conj-of-data (query (:select 'common 'ord :from table
                                           :where (:and (:in 'seq (:set conj-of))
                                                        (:in 'text (:set src-text)))))))
        (when conj-of-data
          (unless common-p
            (let ((conj-of-common (mapcan (lambda (row) (unless (eql (car row) :null) (list (car row)))) conj-of-data)))
              (when conj-of-common
                (setf common 0 common-p t common-of (car (sort conj-of-common #'compare-common))))))
          (let ((conj-of-ord (reduce 'min conj-of-data :key 'second)))
            (when (< conj-of-ord ord) (setf ord conj-of-ord))))))

    (setf primary-p 
          (or (and prefer-kana conj-types-p
                   (not kanji-p)
                   (or (not (primary-nokanji entry))
                       (nokanji reading)))
              (and (or (= ord 0) cop-da-p)
                   (or kanji-p conj-types-p)
                   (or (and kanji-p (not prefer-kana))
                       (and common-p pronoun-p)
                       (= (n-kanji entry) 0)))
              (and prefer-kana kanji-p (= ord 0)
                   (not (query (:select 'id :from 'sense
                                        :where (:and (:in 'id (:set (mapcar 'sense-id prefer-kana)))
                                                     (:= 'ord 0))))))
              ))

    (when primary-p
      (incf score (cond (long-p 10)
                        (common-p 5)
                        ((or prefer-kana (= (n-kanji entry) 0)) 3)
                        (t 2))))
    (when (and particle-p (or final (not semi-final-particle-p)))
      (incf score 2)
      (when common-p
        (incf score (+ 2 len)))
      (when final
        (cond (primary-p (incf score 5))
              (semi-final-particle-p (incf score 2)))))
    (when (and common-p (not no-common-bonus))
      (let ((common-bonus
             (cond
               (secondary-conj-p (if primary-p 4 2))
               ((or long-p cop-da-p (and primary-p root-p (or kanji-p (> len 2))))
                (cond ((= common 0) 10)
                      ((not primary-p) (max (- 15 common) 10))
                      (t (max (- 20 common) 10))))
               (kanji-p 8)
               (primary-p 4)
               ((or (> len 2) (< 0 common 10)) 3)
               (t 2))))
        (incf score common-bonus)))
    (when long-p
      (setf score (max len score)))
    (when kanji-p
      (setf score (max 5 score))
      (when (and long-p kanji-p (or (> n-kanji 1) (> len 4)))
        (incf score 2)))
    (setf prop-score score)
    (setf score (* score (+ (length-multiplier-coeff len (if (or kanji-p katakana-p) :strong :weak))
                            (if (> n-kanji 1) (* (1- n-kanji) 5) 0)
                            (if use-length
                                (+ (length-multiplier-coeff (- use-length len) :tail)
                                   (* score-mod (- use-length len)))
                                0))))

    (multiple-value-bind (split score-mod-split) (get-split reading conj-of)
      (when split
        (setf score
              (+ score-mod-split
                 (loop with nparts = (length split)
                    for part in split
                    for cnt from 1
                    for last = (= cnt nparts)
                    summing (calc-score part
                                        :final (and final last)
                                        :use-length (when (and last use-length)
                                                      (+ (mora-length (text part))
                                                         (- use-length len)))
                                        :score-mod (if last score-mod 0)
                                        ))))))

    (when kanji-break (setf score (kanji-break-penalty score)))
    (values score (list :posi posi :seq-set (cons seq conj-of)
                        :conj conj-data
                        :common (and common-p common-of)
                        :score-info (list prop-score kanji-break)
                        :kpcl (list kanji-p primary-p common-p long-p)))))

(defun gen-score (segment &key final kanji-break)
  (setf (values (segment-score segment) (segment-info segment))
        (calc-score (segment-word segment) :final final :kanji-break kanji-break))
  segment)

(defun find-sticky-positions (str)
  "words cannot start or end after sokuon and before yoon characters"
  (loop with modifiers = (append *modifier-characters* *iteration-characters*)
       and str-len = (length str)
     for pos from 0 below str-len
     for char = (char str pos)
     for char-class = (gethash char *char-class-hash* char)
     if (and (eql char-class :sokuon) 
             (not (= pos (1- str-len)))
             (let ((char (char str (1+ pos))))
               (member (gethash char *char-class-hash* char) *kana-characters*))) collect (1+ pos)
     else if (and (member char-class modifiers)
                  (not (and (= pos (1- str-len)) (eql char-class :long-vowel))))
                  collect pos))

(defun make-slice ()
  (make-array 0 :element-type 'character
              :displaced-to ""))

(defun subseq-slice (slice str start &optional (end (length str)))
  (assert (>= end start))
  (unless slice (setf slice (make-slice)))
  (adjust-array slice (- end start)
                :displaced-to str
                :displaced-index-offset start))

(defun find-substring-words (str)
  (loop with sticky = (find-sticky-positions str)
       and slice = (make-slice)
       for start from 0 below (length str)
       unless (member start sticky)
       nconcing 
       (loop for end from (1+ start) upto (length str)
            unless (member end sticky)
            nconcing (mapcar 
                      (lambda (word)
                        (gen-score (make-segment :start start :end end :word word)
                                   :final (= end (length str))))
                      (find-word (subseq-slice slice str start end))))))

(defparameter *identical-word-score-cutoff* 1/2)

(defun compare-common (c1 c2)
  (cond ((not c2) c1)
        ((= c2 0) (and c1 (> c1 0)))
        ((and c1 (> c1 0)) (< c1 c2))))

(defun cull-segments (segments)
  (when segments
    (let* ((segments (stable-sort segments #'compare-common
                                  :key (lambda (s) (getf (segment-info s) :common))))
           (segments (stable-sort segments #'> :key #'segment-score))
           (max-score (segment-score (car segments)))
           (cutoff (* max-score *identical-word-score-cutoff*)))
      (loop for seg in segments
           while (>= (segment-score seg) cutoff)
           collect seg))))

(defstruct segment-list segments start end (top nil))

(defgeneric get-segment-score (seg)
  (:documentation "Like segment-score but also works for segment-list and synergies")
  (:method ((seg segment))
    (segment-score seg))
  (:method ((seg-list segment-list))
    (let ((seg (car (segment-list-segments seg-list))))
      (if seg (segment-score seg) 0))))

;;; Those are only bound during join-substring-words calls
(defvar *suffix-map-temp* nil)
(defvar *suffix-next-end* nil)

(defun find-word-full (word)
  (let ((simple-words (find-word word)))
    (nconc simple-words
           (find-word-suffix word :unique (not simple-words)))))

(defparameter *score-cutoff* 5) ;; this must filter out ONLY bad kana spellings, and NOT filter out any kanji spellings

(defun join-substring-words* (str)
  (loop with sticky = (find-sticky-positions str)
        and kanji-break
        and slice = (make-slice)
       with suffix-map = (get-suffix-map str)
       for start from 0 below (length str)
       unless (member start sticky)
       nconcing 
       (loop for end from (1+ start) upto (length str)
            unless (member end sticky)
            nconcing
            (let* ((part (subseq-slice slice str start end))
                   (segments (mapcar
                              (lambda (word)
                                (make-segment :start start :end end :word word))
                              (let ((*suffix-map-temp* suffix-map)
                                    (*suffix-next-end* end))
                                (find-word-full part)))))
              (when segments
                (setf kanji-break (nconc (sequential-kanji-positions part start) kanji-break))
                (list (list start end segments)))))
       into result
     finally (return (values result kanji-break))))

(defun join-substring-words (str)
  (multiple-value-bind (result kanji-break) (join-substring-words* str)
    (loop for (start end segments) in result
       for kb = (intersection (list start end) kanji-break)
       for sl = (loop for segment in segments
                   do (gen-score segment :final (= (segment-end segment) (length str)) :kanji-break kb)
                   if (>= (segment-score segment) *score-cutoff*)
                   collect segment)
       when sl
         collect (make-segment-list :segments (cull-segments sl) :start start :end end))))

(defstruct (top-array-item (:conc-name tai-)) score payload)

(defclass top-array ()
  ((array :reader top-array)
   (count :reader item-count :initform 0)
   ))

(defmethod initialize-instance :after ((obj top-array) &key (limit 5))
  (setf (slot-value obj 'array) (make-array limit :initial-element nil)))

(defgeneric register-item (collection score payload)
  (:method ((obj top-array) score payload)
    (with-slots (array count) obj
      (let ((item (make-top-array-item :score score :payload payload))
            (len (length array)))
        (loop for idx from (min count len) downto 0
           for prev-item = (when (> idx 0) (aref array (1- idx)))
           for done = (or (not prev-item) (>= (tai-score prev-item) score))
           when (< idx len) do (setf (aref array idx) (if done item prev-item))
           until done)
        (incf count)))))

(defgeneric get-array (collection)
  (:method ((obj top-array))
    (with-slots (array count) obj
      (if (>= count (length array)) array (subseq array 0 count)))))

(defparameter *gap-penalty* -500)

(declaim (inline gap-penalty))
(defun gap-penalty (start end)
  (* (- end start) *gap-penalty*))

(defun find-best-path (segment-lists str-length &key (limit 5))
  "generalized version of old find-best-path that operates on segment-lists and uses synergies"
  (let ((top (make-instance 'top-array :limit limit)))
    (register-item top (gap-penalty 0 str-length) nil)

    (dolist (segment-list segment-lists)
      (setf (segment-list-top segment-list) (make-instance 'top-array :limit limit)))

    ;;assume segments are sorted by (start, end) (as is the result of find-substring-words)
    (loop for (seg1 . rest) on segment-lists
         for score1 = (get-segment-score seg1)
       do
         (let ((gap-left (gap-penalty 0 (segment-list-start seg1)))
               (gap-right (gap-penalty (segment-list-end seg1) str-length)))
           (register-item (segment-list-top seg1) (+ gap-left score1) (list seg1))
           (register-item top (+ gap-left score1 gap-right) (list seg1)))
         (loop for seg2 in rest
            for score2 = (get-segment-score seg2)
            when (>= (segment-list-start seg2) (segment-list-end seg1)) do
              (loop with gap-left = (gap-penalty (segment-list-end seg1) (segment-list-start seg2))
                   and gap-right = (gap-penalty (segment-list-end seg2) str-length)
                   for tai across (get-array (segment-list-top seg1))
                   for (seg-left . tail) = (tai-payload tai)
                   for score3 = (get-segment-score seg-left)
                   for score-tail = (- (tai-score tai) score3)
                   do (loop for split in (cons (get-penalties seg-left seg2) (get-synergies seg-left seg2))
                           for accum = (+ gap-left
                                          (max (reduce #'+ split :key #'get-segment-score)
                                               (1+ score3)
                                               (1+ score2))
                                          score-tail)
                           for path = (nconc split tail)
                           do (register-item (segment-list-top seg2) accum path)
                              (register-item top (+ accum gap-right) path)))))

    (dolist (segment segment-lists)
      (setf (segment-list-top segment) nil))

    (loop for tai across (get-array top)
         collect (cons (reverse (tai-payload tai)) (tai-score tai)))))

;; (defun find-best-path* (segment-lists &key (limit 5))
;;   "convert find-best-path results to single word format"
;;   (let ((result (find-best-path segment-lists :limit limit)))
;;     (dolist (item result result)
;;       (setf (car item)
;;             (mapcan (lambda (obj)
;;                       (typecase obj
;;                         (segment-list (list (car (segment-list-segments obj))))))
;;                     (car item))))))

(defclass word-info ()
  ((type :initarg :type :accessor word-info-type)
   (text :initarg :text :accessor word-info-text)
   (kana :initarg :kana :accessor word-info-kana)
   (seq :initarg :seq :initform nil :accessor word-info-seq)
   (conjugations :initarg :conjugations :initform nil :accessor word-info-conjugations)
   (score :initarg :score :initform 0 :accessor word-info-score)
   (components :initarg :components :initform nil :accessor word-info-components)
   (alternative :initarg :alternative :initform nil :accessor word-info-alternative)
   (primary :initarg :primary :initform t :accessor word-info-primary)
   (start :initarg :start :initform nil :accessor word-info-start)
   (end :initarg :end :initform nil :accessor word-info-end)
   ))

(defun word-info-json (word-info)
  (with-slots (type text kana seq conjugations score components alternative primary start end)
      word-info
    (jsown:new-js
      ("type" (symbol-name type))
      ("text" text)
      ("kana" kana)
      ("seq" seq)
      ("conjugations" (if (eql conjugations :root) "ROOT" conjugations))
      ("score" score)
      ("components" (mapcar #'word-info-json components))
      ("alternative" alternative)
      ("primary" primary)
      ("start" start)
      ("end" end))))

(defun simple-word-info (seq text reading type &key (as :object))
  (let ((obj (make-instance 'word-info :type type :text text :seq seq :kana reading)))
    (cond ((eql as :object)
           obj)
          ((eql as :json)
           (word-info-json obj)))))

;; define appropriate defmethods so that word-info-str and
;; word-info-gloss-json work both on CLOS objects and jsown objects

(defmacro def-reader-for-json (name slot)
  (alexandria:with-gensyms (obj)
    `(defmethod ,name ((,obj cons))
       (jsown:val ,obj ,slot))))

;;(def-reader-for-json word-info-type "type")
(defmethod word-info-type ((obj cons))
  (let ((val (jsown:val obj "type")))
    (cond ((equal val "KANJI") :kanji)
          ((equal val "KANA") :kana)
          (t :gap))))

(defmethod word-info-conjugations ((obj cons))
  (let ((val (jsown:val obj "conjugations")))
    (cond ((equal val "ROOT") :root)
          (t val))))

(def-reader-for-json word-info-text "text")
(def-reader-for-json word-info-kana "kana")
(def-reader-for-json word-info-seq "seq")
(def-reader-for-json word-info-score "score")
(def-reader-for-json word-info-components "components")
(def-reader-for-json word-info-alternative "alternative")
(def-reader-for-json word-info-primary "primary")
(def-reader-for-json word-info-start "start")
(def-reader-for-json word-info-end "end")

(defmethod print-object ((obj word-info) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a ~a[~a] score=~a"
            (word-info-seq obj) (word-info-text obj) (word-info-kana obj) (word-info-score obj))))

(defun word-info-from-segment (segment &aux (word (segment-word segment)))
  (make-instance 'word-info
                 :type (word-type word)
                 :text (get-text word)
                 :kana (get-kana word)
                 :seq (seq word)
                 :conjugations (when (typep word 'simple-text) (word-conjugations word))
                 :components (when (typep word 'compound-text)
                               (loop with primary-id = (id (primary word)) 
                                  for wrd in (words word)
                                  collect (make-instance 'word-info 
                                                         :type (word-type wrd)
                                                         :text (get-text wrd)
                                                         :kana (get-kana wrd)
                                                         :seq (seq wrd)
                                                         :conjugations (word-conjugations wrd)
                                                         :primary (= (id wrd) primary-id))))
                 :score (segment-score segment)
                 :start (segment-start segment)
                 :end (segment-end segment)))

(defparameter *segment-score-cutoff* 2/3)

(defun word-info-from-segment-list (segment-list)
  (let* ((segments (segment-list-segments segment-list))
         (wi-list* (mapcar #'word-info-from-segment segments))
         (wi1 (car wi-list*))
         (max-score (word-info-score wi1))
         (wi-list (remove-if (lambda (wi)
                               (< (word-info-score wi)
                                  (* *segment-score-cutoff* max-score)))
                             wi-list*)))
    (if (= (length wi-list) 1)
        wi1
        (loop for wi in wi-list
           collect (word-info-kana wi) into kana-list
           collect (word-info-seq wi) into seq-list
           finally (return (make-instance 'word-info
                                          :type (word-info-type wi1)
                                          :text (word-info-text wi1)
                                          :kana (remove-duplicates kana-list :test 'equal)
                                          :seq seq-list
                                          :components wi-list
                                          :alternative t
                                          :score (word-info-score wi1)
                                          :start (segment-list-start segment-list)
                                          :end (segment-list-end segment-list)
                                          ))))))

(defun fill-segment-path (str path)
  (flet ((make-substr-gap (start end)
           (let ((substr (subseq str start end)))
             (make-instance 'word-info
                            :type :gap :text substr :kana substr
                            :start start :end end))))
    (loop with idx = 0 and result
       for segment-list in path
       when (typep segment-list 'segment-list)
       if (> (segment-list-start segment-list) idx)
         do (push (make-substr-gap idx (segment-list-start segment-list)) result)
         end
       and do (push (word-info-from-segment-list segment-list) result)
              (setf idx (segment-list-end segment-list))
       finally
         (when (< idx (length str))
           (push (make-substr-gap idx (length str)) result))
         (return (process-word-info (nreverse result))))))

(defun word-info-rec-find (wi-list test-fn)
  "Find a word satisfying test-fn and the one after it"
  (loop for (wi wi-next) on wi-list
     for components = (word-info-components wi)
     if (funcall test-fn wi) nconc (list (cons wi wi-next))
     nconc (loop for (wf . wf-next) in (word-info-rec-find components test-fn)
              collect (cons wf (or wf-next wi-next)))))
       
(defun process-word-info (wi-list)
  "Process readings such as nani/nan (hardcoded so far)"
  (loop for (wi wi-next) on wi-list
       when (and wi-next (equal (word-info-text wi) "何")) do
         (let ((kn (word-info-kana wi-next)))
            (unless (listp kn) (setf kn (list kn)))
            (loop with nani = nil and nan = nil
               for kana in kn
               for first-char = (when (> (length kana) 0) (char kana 0))
               for fc-class = (gethash first-char *char-class-hash* first-char)
               when first-char
               if (member fc-class '(:ba :bi :bu :be :bo
                                     :pa :pi :pu :pe :po
                                     :da :dji :dzu :de :do
                                     :za :ji :zu :ze :zo
                                     :ta :chi :tsu :te :to
                                     :na :nu :ne :no
                                     :ra :ri :ru :re :ro))
                 do (setf nan t)
               else
               do (setf nani t)
               finally (let ((nani-kana (cond ((and nan nani) "なに")
                                              (nan "なん")
                                              (nani "なに"))))
                         (when nani-kana (setf (word-info-kana wi) nani-kana))))))
  wi-list)

(defun dict-segment (str &key (limit 5))
  (with-connection *connection*
    (loop for (path . score) in (find-best-path (join-substring-words str) (length str) :limit limit)
         collect (cons (fill-segment-path str path) score))))

(defun simple-segment (str &key (limit 5))
  (caar (dict-segment str :limit limit)))

(defun get-senses (seq)
  (query (:order-by
          (:select (:select (:concat "[" (:raw "string_agg(pos.text, ',' ORDER BY pos.ord)") "]")
                            :from (:as 'sense-prop 'pos) :where (:and (:= 'pos.sense-id 'sense.id) (:= 'pos.tag "pos")))
                   (:select (:raw "string_agg(gloss.text, '; ' ORDER BY gloss.ord)")
                            :from 'gloss :where (:= 'gloss.sense-id 'sense.id))
                   :from 'sense
                   :where (:= 'sense.seq seq)
                   :group-by 'sense.id)
          'sense.ord)))

(defun get-senses-str (seq)
  (with-output-to-string (s)
    (loop for (pos gloss) in (get-senses seq)
          for i from 1
          for rpos = pos then (if (equal pos "[]") rpos pos)
          when (> i 1) do (terpri s)
          do (format s "~a. ~a ~a" i rpos gloss))))

(defun split-pos (pos-str)
  (split-sequence #\, pos-str :start 1 :end (1- (length pos-str))))

(defun get-senses-json (seq &key pos-list)
  (loop for (pos gloss) in (get-senses seq)
     for emptypos = (equal pos "[]")
     for rpos = pos then (if emptypos rpos pos)
     for lpos = (split-pos pos) then (if emptypos lpos (split-pos pos))
     when (or (not pos-list) (intersection lpos pos-list :test 'equal))
     collect (list rpos gloss)))

(defun short-sense-str (seq &key with-pos)
  (query 
   (sql-compile
    `(:limit 
      (:order-by
       (:select (:select (:raw "string_agg(gloss.text, '; ' ORDER BY gloss.ord)")
                         :from gloss :where (:= gloss.sense-id sense.id))
                :from sense 
                ,@(if with-pos
                      `(:inner-join (:as sense-prop pos) :on (:and (:= pos.sense-id sense.id)
                                                                   (:= pos.tag "pos")
                                                                   (:= pos.text ,with-pos))))
                :where (:= 'sense.seq ,seq)
                :group-by 'sense.id)
       'sense.ord)
      1)) :single))

(defun reading-str (kanji kana)
  (if kanji
      (format nil "~a 【~a】" kanji kana)
      kana))

(defun reading-str-seq (seq)
  (let* ((kanji-text (car (query (:select 'text :from 'kanji-text :where (:and (:= 'seq seq) (:= 'ord 0))) :column)))
         (kana-text (car (query (:select 'text :from 'kana-text :where (:and (:= 'seq seq) (:= 'ord 0))) :column))))
    (reading-str kanji-text kana-text)))

(defun entry-info-short (seq &key with-pos)
  (let ((sense-str (short-sense-str seq :with-pos with-pos)))
    (with-output-to-string (s)
      (format s "~a : " (reading-str-seq seq))
      (when sense-str (princ sense-str s)))))

(defun select-conjs (seq &optional conj-ids)
  (if conj-ids
      (unless (eql conj-ids :root)
        (select-dao 'conjugation (:and (:= 'seq seq) (:in 'id (:set conj-ids)))))
      (or
       (select-dao 'conjugation (:and (:= 'seq seq) (:is-null 'via)))
       (select-dao 'conjugation (:= 'seq seq)))))

(defun print-conj-info (seq &key conjugations (out *standard-output*))
  (loop with via-used = nil
     for conj in (select-conjs seq conjugations)
     for via = (seq-via conj)
     unless (member via via-used)
     do (loop for conj-prop in (select-dao 'conj-prop (:= 'conj-id (id conj)))
             for first = t then nil
           do (format out "~%~:[ ~;[~] Conjugation: ~a" first (conj-info-short conj-prop)))
       (if (eql via :null)
           (format out "~%  ~a" (entry-info-short (seq-from conj)))
           (progn
             (format out "~% --(via)--")
             (print-conj-info via :out out)
             (push via via-used)))
       (princ " ]" out)))

(defun conj-info-json (seq &key conjugations)
  (loop with via-used = nil
     for conj in (select-conjs seq conjugations)
     for via = (seq-via conj)
     unless (member via via-used)
     collect (let* ((conj-pos nil)
                    (js (jsown:new-js 
                          ("prop" (loop for conj-prop in (select-dao 'conj-prop (:= 'conj-id (id conj)))
                                     do (push (pos conj-prop) conj-pos)
                                     collect (conj-prop-json conj-prop))))))
               (if (eql via :null)
                   (jsown:extend-js js
                     ("reading" (reading-str-seq (seq-from conj)))
                     ("gloss" (get-senses-json (seq-from conj) :pos-list conj-pos)))
                   (progn
                     (jsown:extend-js js
                       ("via" (conj-info-json via)))
                     (push via via-used)))
               js)))

(defun map-word-info-kana (fn word-info &key (separator "/")
                           &aux (wkana (word-info-kana word-info)))
  (if (listp wkana)
      (with-output-to-string (s)
        (loop for wk in wkana
             for first = t then nil
             unless first do (princ separator s)
             do (princ (funcall fn wk) s)))
      (funcall fn wkana)))

(defun word-info-reading-str (word-info)
  (reading-str (case (word-info-type word-info)
                 (:kanji (word-info-text word-info))
                 (t nil))
               (word-info-kana word-info)))

(defun word-info-str (word-info)
  (with-connection *connection*
    (with-output-to-string (s)
      (labels ((inner (word-info &optional suffix marker)
                 (when marker (princ " * " s))
                 (princ (word-info-reading-str word-info) s)
                 (if (word-info-components word-info)
                     (progn
                       (format s " Compound word: ~{~a~^ + ~}" (mapcar #'word-info-text (word-info-components word-info)))
                       (dolist (comp (word-info-components word-info))
                         (terpri s)
                         (inner comp (not (word-info-primary comp)) t)))
                     (let ((seq (word-info-seq word-info)) 
                           (conjs (word-info-conjugations word-info))
                           desc)
                       (cond ((and suffix (setf desc (get-suffix-description seq)))
                              (format s "  [suffix]: ~a " desc))
                             ((or (not conjs) (eql conjs :root))
                              (terpri s) (princ (if seq (get-senses-str seq) "???") s)))
                       (when seq
                         (print-conj-info seq :out s
                                          :conjugations conjs))))))
        (if (word-info-alternative word-info)
            (loop for wi in (word-info-components word-info)
                 for i from 1
                 when (> i 1) do (terpri s)
                 do (format s "<~a>. " i) (inner wi nil nil))
            (inner word-info))))))

(defun word-info-gloss-json (word-info &key root-only)
  (with-connection *connection*
    (labels ((inner (word-info &optional suffix)
               (let ((js (jsown:new-js ("reading" (word-info-reading-str word-info))
                                       ("text" (word-info-text word-info))
                                       ("kana" (word-info-kana word-info))
                                       )))
                 (when (word-info-score word-info)
                   (jsown:extend-js js ("score" (word-info-score word-info))))
                 (if (word-info-components word-info)
                     (jsown:extend-js js
                       ("compound" (mapcar #'word-info-text (word-info-components word-info)))
                       ("components" (loop for wi in (word-info-components word-info)
                                        collect (inner wi (not (word-info-primary wi))))))
                     (let ((seq (word-info-seq word-info))
                           (conjs (word-info-conjugations word-info))
                           desc)
                       (cond (root-only
                              (return-from inner
                                (jsown:extend-js js ("gloss" (get-senses-json seq)))))
                             ((and suffix (setf desc (get-suffix-description seq)))
                              (jsown:extend-js js ("suffix" desc)))
                             ((and seq (or (not conjs) (eql conjs :root)))
                              (jsown:extend-js js ("gloss" (get-senses-json seq)))))
                       (when seq
                         (jsown:extend-js js 
                           ("conj" (conj-info-json seq :conjugations (word-info-conjugations word-info)))))))
                 js)))
      (if (word-info-alternative word-info)
          (jsown:new-js ("alternative" (mapcar #'inner (word-info-components word-info))))
          (inner word-info)))))

(defun get-kanji-words (char)
  (with-connection *connection*
    (let* ((str (if (typep char 'character) (make-string 1 :initial-element char) char)))
      (query (:select 'e.seq 'k.text 'r.text 'k.common
                      :from (:as 'entry 'e) (:as 'kanji-text 'k) (:as 'kana-text 'r)
                      :where (:and (:= 'e.seq 'k.seq)
                                   (:= 'e.seq 'r.seq)
                                   (:= 'r.text 'k.best-kana)
                                   (:not-null 'k.common)
                                   'e.root-p
                                   (:like 'k.text (:|| "%" str "%"))))))))

(defun exists-reading (seq reading)
  (query (:select 'seq :from 'kana-text :where (:and (:= 'seq seq) (:= 'text reading)))))
         
(defun find-word-info (text &key reading root-only &aux (end (length text)))
  (with-connection *connection*
    (let* ((*suffix-map-temp* (get-suffix-map text))
           (*suffix-next-end* end)
           (all-words (if root-only
                          (find-word text :root-only t)
                          (find-word-full text)))
           (segments (loop for word in all-words
                        collect (gen-score (make-segment :start 0 :end end :word word))))
           (segments (sort segments #'> :key #'segment-score))
           (wis (mapcar #'word-info-from-segment segments)))
      (when reading
        (setf wis
              (loop for wi in wis
                 for seq = (word-info-seq wi)
                 if (equal (word-info-kana wi) reading)
                 collect wi
                 else if (and seq (exists-reading seq reading))
                 do (setf (word-info-kana wi) reading)
                 and collect wi)))
      wis)))

(defun find-word-info-json (text &key reading root-only)
  (mapcar (lambda (wi) (word-info-gloss-json wi :root-only root-only))
          (find-word-info text :reading reading :root-only root-only)))

