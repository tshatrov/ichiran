;; JMdict crawler

(in-package #:ichiran/dict)

(defparameter *jmdict-path* #p"foobar")

(defparameter *jmdict-data* #p"foobar")

(defparameter *connection* '("jmdict" "postgres" "" "localhost"))

(eval-when (:load-toplevel)
  (load (asdf:system-relative-pathname :ichiran "settings.lisp") :if-does-not-exist nil))

(defgeneric get-kana (obj)
  (:documentation "most popular kana representation"))

(defgeneric get-text (obj)
  (:documentation "most popular text representation (kanji or kana)")
  (:method (obj) (text obj)))

(defclass entry ()
  ((seq :reader seq :col-type integer :initarg :seq)
   (content :reader content :col-type string :initarg :content)
   (n-kanji :accessor n-kanji :col-type integer :initform 0)
   (n-kana :accessor n-kana :col-type integer :initform 0) 
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

(defclass kanji-text ()
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (text :reader text :col-type string :initarg :text)
   (ord :reader ord :col-type integer :initarg :ord)
   (common :reader common :col-type (or db-null integer) :initarg :common)
   )
  (:metaclass dao-class)
  (:keys id))

(deftable kanji-text
  (!dao-def)
  (!index 'seq)
  (!index 'ord)
  (!index 'text)
  (!index 'common)
  (!foreign 'entry 'seq))

(defmethod print-object ((obj kanji-text) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a ~a" (seq obj) (text obj))))

(defmethod get-kana ((obj kanji-text))
  (text (car (select-dao 'kana-text (:and (:= 'seq (seq obj)) (:= 'ord 0))))))

(defclass kana-text ()
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (text :reader text :col-type string :initarg :text)
   (ord :reader ord :col-type integer :initarg :ord)
   (common :reader common :col-type (or db-null integer) :initarg :common)
   )
  (:metaclass dao-class)
  (:keys id))

(deftable kana-text
  (!dao-def)
  (!index 'seq)
  (!index 'ord)
  (!index 'text)
  (!index 'common)
  (!foreign 'entry 'seq))

(defmethod print-object ((obj kana-text) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a ~a" (seq obj) (text obj))))

(defmethod get-kana ((obj kana-text))
  (text obj))

(defclass sense ()
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (ord :reader ord :col-type integer :initarg :ord))
  (:metaclass dao-class)
  (:keys id))

(deftable sense
  (!dao-def)
  (!index 'seq)
  (!index 'ord)
  (!foreign 'entry 'seq))

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
  (!index 'ord)
  (!foreign 'sense 'sense-id 'id))

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
  (!index 'sense-id)
  (!index 'tag)
  (!index 'text)
  (!index 'ord)
  (!index 'seq)
  (!foreign 'entry 'seq)
  (!foreign 'sense 'sense-id 'id))

(defclass conjugation ()
  ((seq :reader seq :col-type integer :initarg :seq)
   (from :reader seq-from :col-type integer :initarg :from)
   (pos :reader pos :col-type string :initarg :pos)
   )
  (:documentation "conjugation link")
  (:metaclass dao-class)
  (:keys seq))

(deftable conjugation
  (!dao-def)
  (!index 'from)
  (!index 'pos)
  (!foreign 'entry 'seq)
  (!foreign 'entry 'from 'seq))

(defclass conj-prop ()
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (neg :reader conj-neg :col-type boolean :initarg :neg)
   (fml :reader conj-fml :col-type boolean :initarg :fml))
  (:metaclass dao-class)
  (:keys id))

(deftable conj-prop
  (!dao-def)
  (!index 'seq)
  (!foreign 'entry 'seq))


(defmacro csv-hash (hash-name (filename &key skip-first) loader-opts &rest accessor-opts-list)
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
             :do (setf (gethash ,row-key-form ,hash-name) ,value-form)))
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

(csv-hash *conj-description* ("conj.csv" :skip-first t)
          ((conj-id description) (parse-integer conj-id) description)
          (val val))

(defstruct (conjugation-rule
             (:conc-name cr-)
             (:constructor make-conjugation-rule (pos conj neg fml onum stem okuri euphr euphk)))
  pos conj neg fml onum stem okuri euphr euphk)

(csv-hash *conj-rules* ("conjo.csv" :skip-first t)
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
          (val val))


(defun construct-conjugation (word rule)
  (assert (> (length word) 1))
  (let* ((iskana (test-word (subseq word (- (length word) 2)) :kana))
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

(defun init-tables ()
  (with-connection *connection*
    (let ((tables '(entry kanji-text kana-text sense gloss sense-prop conjugation conj-prop)))
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
  (do-node-list-ord (ord node node-list)
    (let* ((reading-node (dom:item (dom:get-elements-by-tag-name node tag) 0))
           (reading-text (node-text reading-node))
           (common :null))
      (dom:do-node-list (node (dom:get-elements-by-tag-name node pri))
        (let ((pri-tag (node-text node)))
          (if (eql common :null) (setf common 0))
          (when (alexandria:starts-with-subseq "nf" pri-tag)
            (setf common (parse-integer pri-tag :start 2)))))
      (make-dao table :seq seq :text reading-text :ord ord :common common))
    (incf ord)))

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
         (seq (parse-integer (node-text entseq-node)))
         (entry-obj (make-dao 'entry :seq seq :content content)))
    (let* ((kanji-nodes (dom:get-elements-by-tag-name parsed "k_ele"))
           (kana-nodes (dom:get-elements-by-tag-name parsed "r_ele"))
           (n-kanji (dom:length kanji-nodes))
           (n-kana (dom:length kana-nodes))
           (sense-nodes (dom:get-elements-by-tag-name parsed "sense")))
      (insert-readings kanji-nodes "keb" 'kanji-text seq "ke_pri")
      (insert-readings kana-nodes "reb" 'kana-text seq "re_pri")
      (insert-senses sense-nodes seq)
      (setf (n-kanji entry-obj) n-kanji
            (n-kana entry-obj) n-kana)
      (update-dao entry-obj))))

(defun fix-entities (source)
  "replaces entity definitions with abbreviations"
  (let ((entity-hash (cxml::dtd-gentities (cxml::dtd (slot-value source 'cxml::context)))))
    (maphash 
     (lambda (name entdef)
       (unless (member name '("lt" "gt" "amp" "apos" "quot") :test #'equal)
         (setf (cxml::entdef-value (cdr entdef)) name)))
     entity-hash)))
    
(defun load-jmdict (&optional (path *jmdict-path*))
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
         finally (query "ANALYZE") (return cnt)))))

(defun find-word (word)
  (select-dao 
   (if (test-word word :kana) 'kana-text 'kanji-text)
   (:= 'text word)))

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

(defstruct segment
  start end word (score nil) (top nil)) ;; (accum 0) (path nil)

(defun calc-score (reading)
  (let* ((score 1)
         (kanji-p (typep reading 'kanji-text))
         (len (length (text reading))))
    (with-slots (common seq ord) reading
      (let* ((entry (get-dao 'entry seq))
             (prefer-kana (select-dao 'sense-prop (:and (:= 'seq seq) (:= 'tag "misc") (:= 'text "uk"))))
             (particle-p (select-dao 'sense-prop (:and (:= 'seq seq) (:= 'tag "pos") (:= 'text "prt"))))
             (long-p (> len (if kanji-p 2 3)))
             (primary-p (and (= ord 0)
                             (or (and prefer-kana (not kanji-p))
                                 (and (not prefer-kana) kanji-p)
                                 (= (n-kanji entry) 0)))))
        (when primary-p
          (incf score 10)
          (when particle-p
            (incf score 10)))
        (unless (eql common :null)
          (if (or primary-p long-p)
              (incf score (if (= common 0) 10 (max (- 20 common) 10)))
              (incf score 5)))
        (setf score (* score (expt len (if kanji-p 3 2))))
        ))
    score))

(defun gen-score (segment)
  (setf (segment-score segment) (calc-score (segment-word segment)))
  segment)

(defun find-sticky-positions (str)
  "words cannot start or end after sokuon and before yoon characters"
  (loop with modifiers = (append *modifier-characters* *iteration-characters*)
     for pos from 0 below (length str)
     for char = (char str pos)
     for char-class = (gethash char *char-class-hash* char)
     if (eql char-class :sokuon) collect (1+ pos)
     else if (member char-class modifiers) collect pos))

(defun find-substring-words (str)
  (loop with sticky = (find-sticky-positions str)
       for start from 0 below (length str)
       unless (member start sticky)
       nconcing 
       (loop for end from (1+ start) upto (length str)
            unless (member end sticky)
            nconcing (mapcar 
                      (lambda (word)
                        (gen-score (make-segment :start start :end end :word word)))
                      (find-word (subseq str start end))))))


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


(defun find-best-path (segments &key (limit 5))
  ;;assume segments are sorted by (start, end) (as is the result of find-substring-words)
  (let ((top (make-instance 'top-array :limit limit)))
    (register-item top 0 nil)

    (dolist (segment segments)
      (setf (segment-top segment) (make-instance 'top-array :limit limit)))

    (loop for (seg1 . rest) on segments
       when (> (segment-score seg1) 0) do 
         (register-item (segment-top seg1) (segment-score seg1) (list seg1))
         (register-item top (segment-score seg1) (list seg1))
         (loop for seg2 in rest
            for seg2-score = (segment-score seg2)
            when (and (> seg2-score 0) 
                      (>= (segment-start seg2) (segment-end seg1))) do
              (loop for tai across (get-array (segment-top seg1))
                 for accum = (+ (tai-score tai) seg2-score)
                 for path = (cons seg2 (tai-payload tai))
                 do (register-item (segment-top seg2) accum path)
                    (register-item top accum path))))
    (dolist (segment segments)
      (setf (segment-top segment) nil))

    (loop for tai across (get-array top)
         collect (cons (reverse (tai-payload tai)) (tai-score tai)))))

(defstruct word-info type text kana (score 0) (seq nil))

(defun word-info-from-segment (segment &aux (word (segment-word segment)))
  (make-word-info :type (if (typep word 'kanji-text) :kanji :kana)
                  :text (get-text word)
                  :kana (get-kana word)
                  :seq (seq word)
                  :score (segment-score segment)))

(defun fill-segment-path (str path)
  (flet ((make-substr-gap (start end)
           (let ((substr (subseq str start end)))
             (make-word-info :type :gap :text substr :kana substr))))
    (loop with idx = 0 and result
       for segment in path
       if (> (segment-start segment) idx)
         do (push (make-substr-gap idx (segment-start segment)) result)
       do (push (word-info-from-segment segment) result)
          (setf idx (segment-end segment))
       finally
         (when (< idx (length str))
           (push (make-substr-gap idx (length str)) result))
         (return (nreverse result)))))
  
                      
(defun dict-segment (str &key (limit 5))
  (with-connection *connection*
    (loop for (path . score) in (find-best-path (find-substring-words str) :limit limit)
         collect (cons (fill-segment-path str path) score))))

(defun simple-segment (str)
  (caar (dict-segment str :limit 1)))

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

(defun word-info-str (word-info)
  (with-connection *connection*
    (with-output-to-string (s)
      (case (word-info-type word-info)
        (:kanji (format s "~a 【~a】" (word-info-text word-info) (word-info-kana word-info)))
        (t (princ (word-info-text word-info) s)))
      (terpri s)
      (let ((seq (word-info-seq word-info)))
        (princ (if seq (get-senses-str seq) "???") s)))))
          
