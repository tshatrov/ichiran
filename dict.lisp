;; ichiran dictionary module
;; based on JMDict

(in-package #:ichiran/dict)

(defvar *jmdict-path* #p"foobar")

(defvar *jmdict-data* #p"foobar")

(load-settings :keep-connection t)

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

(defun recalc-entry-stats (&rest entries)
  (query (:update 'entry :set
                  'n-kanji (:select (:count 'id) :from 'kanji-text :where (:= 'kanji-text.seq 'entry.seq))
                  'n-kana (:select (:count 'id) :from 'kana-text :where (:= 'kana-text.seq 'entry.seq))
                  :where (:in 'entry.seq (:set entries)))))

(defun recalc-entry-stats-all ()
  (query (:update 'entry :set
                  'n-kanji (:select (:count 'id) :from 'kanji-text :where (:= 'kanji-text.seq 'entry.seq))
                  'n-kana (:select (:count 'id) :from 'kana-text :where (:= 'kana-text.seq 'entry.seq)))))

(defun entry-digest (entry)
  (list (seq entry) (get-text entry) (get-kana entry)))

(defclass simple-text ()
  ((conjugations :accessor word-conjugations :initform nil)
   (hintedp :accessor hintedp :initarg :hintedp :initform nil)
   ))

(defmethod print-object ((obj simple-text) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a ~a" (seq obj) (text obj))))

(defparameter *disable-hints* nil)

(defmethod get-kana :around ((obj simple-text))
  (or (unless (or *disable-hints* (hintedp obj))
        (let ((*disable-hints* t))
          (get-hint obj)))
      (call-next-method)))

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

(defmethod get-kana ((obj kanji-text))
  (let ((bk (best-kana-conj obj)))
    (if (eql bk :null)
        (get-kanji-kana-old obj)
        bk)))

(defun get-kanji-kana-old (obj)
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


(defmethod common ((obj entry) &aux (seq (seq obj)))
  (query (:select (:max 'common) :from (:as (:union
          (:select 'common :from 'kanji-text :where (:= 'seq seq))
          (:select 'common :from 'kana-text :where (:= 'seq seq))) 'tmp))
         :single))

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
  (format nil "[~a] ~a~@[~[ Affirmative~; Negative~]~]~@[~[ Plain~; Formal~]~]"
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

(defcache :no-conj-data *no-conj-data*
  ;; seq's that DON'T have conj data are calculated here
  ;; both because there's less of them, and it's generally safer when new conjs are added
  (let ((no-conj-data (make-hash-table :size 200000)))
    (dolist (seq (query (:select 'entry.seq :from 'entry
                                 :left-join (:as 'conjugation 'c) :on (:= 'entry.seq 'c.seq)
                                 :where (:is-null 'c.seq)) :column))
      (setf (gethash seq no-conj-data) t))
    no-conj-data))

(defun no-conj-data (seq)
  (nth-value 1 (gethash seq (ensure :no-conj-data))))

(defun get-conj-data (seq &optional from/conj-ids texts)
  "from/conj-ids can be either from which word to find conjugations or a list of conj-ids
   texts is a string or list of strings, if supplied, only the conjs that have src-map with this text will be collected
"
  (when (or (eql from/conj-ids :root) (no-conj-data seq))
    (return-from get-conj-data nil))
  (unless (listp texts)
    (setf texts (list texts)))
  (loop for conj in (cond
                      ((null from/conj-ids)
                       (select-dao 'conjugation (:= 'seq seq)))
                      ((listp from/conj-ids)
                       (select-dao 'conjugation (:and (:= 'seq seq) (:in 'id (:set from/conj-ids)))))
                      (t (select-dao 'conjugation (:and (:= 'seq seq) (:= 'from from/conj-ids)))))
     for src-map = (if texts
                       (query (:select 'text 'source-text :from 'conj-source-reading
                                       :where (:and (:= 'conj-id (id conj)) (:in 'text (:set texts)))))
                       (query (:select 'text 'source-text :from 'conj-source-reading
                                       :where (:= 'conj-id (id conj)))))
       when (or (not texts) src-map)
       nconcing (loop for prop in (select-dao 'conj-prop (:= 'conj-id (id conj)))
                     collect (make-conj-data :seq (seq conj) :from (seq-from conj)
                                             :via (let ((via (seq-via conj)))
                                                    (if (eql via :null) nil via))
                                             :prop prop
                                             :src-map src-map
                                             ))))

(defun get-original-text-once (conj-datas texts)
  (unless (listp texts)
    (setf texts (list texts)))
  (unless (listp conj-datas)
    (setf conj-datas (list conj-datas)))
  (loop for conj-data in conj-datas
       nconc (loop for (txt src-txt) in (conj-data-src-map conj-data)
                if (find txt texts :test 'equal) collect src-txt)))

(defun get-original-text* (conj-datas texts)
  (unless (listp texts)
    (setf texts (list texts)))
  (unless (listp conj-datas)
    (setf conj-datas (list conj-datas)))
  (loop for conj-data in conj-datas
       nconc
       (let ((src-text (loop for (txt src-txt) in (conj-data-src-map conj-data)
                          if (find txt texts :test 'equal) collect src-txt)))
         (if (not (conj-data-via conj-data))
             (mapcar (lambda (txt) (list txt (conj-data-from conj-data))) src-text)
             (let ((new-cd (get-conj-data (conj-data-via conj-data) (conj-data-from conj-data))))
                     (get-original-text* new-cd src-text))))))

(defgeneric get-original-text (reading &key conj-data)
  (:documentation "Returns unconjugated text(s) for reading")
  (:method ((reading simple-text) &key conj-data)
    (let ((orig-texts (get-original-text* (or conj-data (word-conj-data reading)) (text reading)))
          (table (case (word-type reading) (:kanji 'kanji-text) (:kana 'kana-text))))
      (loop for (txt seq) in orig-texts
           nconc (select-dao table (:and (:= 'seq seq) (:= 'text txt)))))))

;;;;

(defprepared query-parents-kanji
    (:select 'kt.id 'conj.id
             :from (:as 'kanji-text 'kt)
             (:as 'conj-source-reading 'csr)
             (:as 'conjugation 'conj)
             :where (:and
                     (:= 'conj.seq '$1)
                     (:= 'conj.id 'csr.conj-id)
                     (:= 'csr.text '$2)
                     (:= 'kt.seq (:case ((:not-null 'conj.via) 'conj.via)
                                   (:else 'conj.from)))
                     (:= 'kt.text 'csr.source-text))))

(defprepared query-parents-kana
    (:select 'kt.id 'conj.id
             :from (:as 'kana-text 'kt)
             (:as 'conj-source-reading 'csr)
             (:as 'conjugation 'conj)
             :where (:and
                     (:= 'conj.seq '$1)
                     (:= 'conj.id 'csr.conj-id)
                     (:= 'csr.text '$2)
                     (:= 'kt.seq (:case ((:not-null 'conj.via) 'conj.via)
                                   (:else 'conj.from)))
                     (:= 'kt.text 'csr.source-text))))

(defun best-kana-conj (obj &aux (wc (word-conjugations obj)))
  (cond ((and (or (not wc) (eql wc :root))
              (not (eql (best-kana obj) :null)))
         (best-kana obj))
        (t (let* ((parents (query-parents-kanji (seq obj) (text obj))))
             (loop for (pid cid) in parents
                  for parent-kt = (get-dao 'kanji-text pid)
                  for parent-bk = (best-kana-conj parent-kt)
                  unless (or (eql parent-bk :null) (and wc (or (eql wc :root) (not (find cid wc)))))
                  do (let ((readings (query (:select 'text :from 'conj-source-reading
                                                     :where (:and (:= 'conj-id cid)
                                                                  (:= 'source-text parent-bk)))
                                            :column)))
                       (when readings
                         (return
                           (if (= (length readings) 1)
                               (car readings)
                               (let ((km (kanji-cross-match (text parent-kt) parent-bk (text obj))))
                                 (or (car (member km readings :test 'equal))
                                     (loop with regex = (kanji-regex (text obj))
                                        for rd in readings
                                        if (ppcre:scan regex rd) do (return rd)
                                        finally (return (car readings)))))))))
                  finally (return :null))))))

(defun best-kanji-conj (obj &aux (wc (word-conjugations obj)))
  (cond ((and (or (not wc) (eql wc :root))
              (not (eql (best-kanji obj) :null)))
         (best-kanji obj))
        ((or (nokanji obj) (= (n-kanji (get-dao 'entry (seq obj))) 0))
         :null)
        (t (let* ((parents (query-parents-kana (seq obj) (text obj))))
             (loop for (pid cid) in parents
                  for parent-bk = (best-kanji-conj (get-dao 'kana-text pid))
                  unless (or (eql parent-bk :null) (and wc (or (eql wc :root) (not (find cid wc)))))
                  do (let* ((readings (query (:select 'text :from 'conj-source-reading
                                                     :where (:and (:= 'conj-id cid)
                                                                  (:= 'source-text parent-bk)))
                                            :column))
                            (matching-readings
                             (some (lambda (reading) (and (kanji-match reading (text obj)) reading))
                                   readings)))
                       (when matching-readings
                         (return matching-readings)))
                  finally (return :null))))))


;;;


;; > (query (:select (:max (:length 'text)) :from 'kana-text) :single)
;; 37
;; > (query (:select (:max (:length 'text)) :from 'kanji-text) :single)
;; 27
(defparameter *max-word-length* 50)
(defparameter *substring-hash* nil)

(defun find-word (word &key root-only)
  (when (<= (length word) *max-word-length*)
    (multiple-value-bind (inits present-p) (and *substring-hash* (gethash word *substring-hash*))
      (if (and present-p (not root-only))
          (loop for init in inits collect (apply 'make-instance init))
          (let ((table (if (test-word word :kana) 'kana-text 'kanji-text)))
            (if root-only
                (query-dao table (:select 'wt.* :from (:as table 'wt) :inner-join 'entry :on (:= 'wt.seq 'entry.seq)
                                          :where (:and (:= 'text word)
                                                       'root-p)))
                (select-dao table (:= 'text word))))))))

(defun find-substring-words (str &key sticky)
  (let ((substring-hash (make-hash-table :test 'equal))
        kana-keys kanji-keys)
    (loop
       for start from 0 below (length str)
       unless (member start sticky)
       do (loop for end from (1+ start) upto (min (length str) (+ start *max-word-length*))
             unless (member end sticky)
             do (let ((part (subseq str start end)))
                  (setf (gethash part substring-hash) nil)
                  (if (test-word part :kana) (push part kana-keys) (push part kanji-keys)))))
    (loop
       for table in '(kana-text kanji-text)
       for keys in (mapcar 'remove-duplicates (list kana-keys kanji-keys))
       when keys
       do (loop for kt in (query (:select '* :from table :where (:in 'text (:set keys))) :plists)
             do (push (cons table kt) (gethash (getf kt :text) substring-hash))))
    substring-hash))

(defun find-words-seqs (words seqs)
  "generalized version of find-word-seq from dict-grammar"
  (unless (listp words)
    (setf words (list words)))
  (unless (listp seqs)
    (setf seqs (list seqs)))
  (loop for word in words
     if (test-word word :kana)
     collect word into kana-words
     else
     collect word into kanji-words
     finally
       (let ((kw (when kanji-words (select-dao 'kanji-text (:and (:in 'text (:set kanji-words)) (:in 'seq (:set seqs))))))
             (rw (when kana-words (select-dao 'kana-text (:and (:in 'text (:set kana-words)) (:in 'seq (:set seqs)))))))
         (return (nconc kw rw)))))

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
  (:documentation "Returns true text for reading")
  (:method (obj) (text obj))
  (:method ((obj proxy-text)) (true-text (source obj))))

(defgeneric true-kana (obj)
  (:method (obj) (get-kana obj))
  (:method ((obj proxy-text)) (true-kana (source obj))))

(defgeneric true-kanji (obj)
  (:method (obj) (get-kanji obj))
  (:method ((obj proxy-text)) (true-kanji (source obj))))

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

(defmethod get-original-text ((reading proxy-text) &key conj-data)
  (get-original-text (source reading) :conj-data conj-data))

(defun find-word-as-hiragana (str &key exclude finder)
  (let* ((as-hiragana (as-hiragana str))
         (words (and (string/= str as-hiragana) (if finder
                                                    (funcall finder as-hiragana)
                                                    (find-word as-hiragana :root-only t)))))
    (when words
      (let ((str (copy-seq str)))
        (loop for w in words
           unless (find (seq w) exclude)
           collect (make-instance 'proxy-text
                                  :source w
                                  :text str
                                  :kana str))))))

;; Compound words (2 or more words squished together)

(defclass compound-text ()
  ((text :reader text :initarg :text)
   (kana :reader get-kana :initarg :kana)
   (primary :reader primary :initarg :primary)
   (words :reader words :initarg :words)
   (score-base :initform nil :initarg :score-base)
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

(defgeneric adjoin-word (word1 word2 &key text kana score-mod score-base)
  (:documentation "make compound word from 2 words"))

(defmethod adjoin-word :around (word1 word2 &key text kana score-mod score-base)
  (call-next-method word1 word2
                    :text (or text (concatenate 'string (get-text word1) (get-text word2)))
                    :kana (or kana (concatenate 'string (get-kana word1) (get-kana word2)))
                    :score-mod (or score-mod 0)
                    :score-base score-base))

(defmethod adjoin-word ((word1 simple-text) (word2 simple-text) &key text kana score-mod score-base)
  (make-instance 'compound-text
                 :text text :kana kana :primary word1 :words (list word1 word2)
                 :score-mod score-mod :score-base score-base))

(defmethod adjoin-word ((word1 compound-text) (word2 simple-text) &key text kana score-mod &allow-other-keys)
  (with-slots ((s-text text) (s-kana kana) (s-words words) (s-score-mod score-mod)) word1
    (setf s-text text s-kana kana
          s-words (append s-words (list word2))
          s-score-mod (funcall (if (listp s-score-mod) 'cons 'list) score-mod s-score-mod)))
  word1)

(defgeneric word-conj-data (word)
  (:documentation "conjugation data for word"))

(defmethod word-conj-data ((word simple-text))
  (get-conj-data (seq word) (word-conjugations word) (true-text word)))

(defmethod word-conj-data ((word compound-text))
  (word-conj-data (car (last (words word)))))

(defmethod word-conjugations ((word compound-text))
  (word-conjugations (car (last (words word)))))

(defmethod (setf word-conjugations) (value (word compound-text))
  (setf (word-conjugations (car (last (words word)))) value))

(defgeneric score-base (word)
  (:method ((word compound-text))
    (with-slots (score-base primary) word
      (or score-base primary))))

(defstruct segment
  start end word (score nil) (info nil) (top nil) (text nil))

(defmethod get-text ((segment segment))
  (or (segment-text segment)
      (setf (segment-text segment) (text (segment-word segment)))))

(defun length-multiplier (length power len-lim)
  "len^power until len-lim, goes linear after"
  (cond ((<= length len-lim) (expt length power))
        (t (* length (expt len-lim (1- power))))))

(defparameter *length-coeff-sequences*
  '((:strong 1 8 24 40 60)
    (:weak 1 4 9 16 25 36)
    (:tail 4 9 16 24)
    (:ltail 4 12 18 24)))

(declaim (inline length-multiplier-coeff))
(declaim (ftype (function ((integer 0 10000) (member :strong :weak :tail :ltail)) (integer 0 120000)) length-multiplier-coeff))
(defun length-multiplier-coeff (length class)
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (let ((coeffs (assoc class *length-coeff-sequences*)))
    (declare (type list coeffs))
    (if (< 0 length (length coeffs))
        (elt coeffs length)
        (* length (the (integer 0 1000) (/ (the integer (car (last coeffs))) (1- (length coeffs))))))))

(defun kanji-break-penalty (kanji-break score &key info text use-length score-mod)
  (let ((end (cond ((cdr kanji-break) :both)
                   ((eql (car kanji-break) 0) :beg)
                   (t :end)))
        (bonus 0) (ratio 2)
        (posi (and info (getf info :posi))))
    (when info
      (cond ((or (intersection (getf info :seq-set) *no-kanji-break-penalty*)
                 ;; do not penalize second word of で/す break
                 (and (eql end :beg) (alexandria:starts-with #\す text)))
             (return-from kanji-break-penalty score))
            ((intersection '("vs-s" "v5s") posi :test 'equal)
             ;; these words have する endings which might lead to bad splits
             (let ((suru-suffix (find :suru (get-suffixes text) :key 'second)))
               (when suru-suffix
                 (let* ((offset (- (mora-length text) (mora-length (car suru-suffix))))
                        (suffix-score (calc-score (third suru-suffix)
                                                  :use-length (and use-length (- use-length offset))
                                                  :score-mod score-mod)))
                   (return-from kanji-break-penalty (min score (+ suffix-score 50)))))))
            ((and (eql end :beg) (member "num" posi :test 'equal))
             (incf bonus 5))
            ((and (eql end :beg) (intersection
                                  '("suf" "n-suf") posi :test 'equal))
             (incf bonus 10))
            ((and (eql end :end) (member "pref" posi :test 'equal))
             (incf bonus 12))
            ))
    (if (>= score *score-cutoff*)
        (max *score-cutoff* (+ (ceiling score ratio) bonus))
        score)))


(defgeneric apply-score-mod (score-mod score len)
  (:documentation "apply score-mod")
  (:method ((score-mod integer) score len)
    (* score score-mod len))
  (:method ((score-mod function) score len)
    (funcall score-mod score))
  (:method ((score-mod list) score len)
    (reduce '+ score-mod :key (lambda (sm) (apply-score-mod sm score len)))))


(defcache :is-arch *is-arch-cache*
  (let* ((is-arch-cache (make-hash-table))
         (a1 (query
                  (:select 'sense.seq :from 'sense
                           :left-join (:as 'sense-prop 'sp) :on (:and (:= 'sp.sense-id 'sense.id)
                                                                      (:= 'sp.tag "misc")
                                                                      (:or (:= 'sp.text "arch")
                                                                           (:= 'sp.text "obsc")))
                           :group-by 'sense.seq :having (:every (:not-null 'sp.id)))
                  :column))
         (a2 (query (:select (:distinct 'seq) :from 'conjugation :where (:in 'from (:set a1))) :column)))
    (dolist (seq (union a1 a2))
      (setf (gethash seq is-arch-cache) t))
    is-arch-cache))

(defun is-arch (seq)
  (nth-value 1 (gethash seq (ensure :is-arch))))

(defun get-non-arch-posi (seq-set)
  (query
   (:select 'sp1.text :distinct
            :from (:as 'sense-prop 'sp1)
            :left-join (:as 'sense-prop 'sp2)
            :on (:and (:= 'sp1.sense-id 'sp2.sense-id)
                      (:= 'sp2.tag "misc")
                      (:or (:= 'sp2.text "arch")
                           (:= 'sp2.text "obsc")))
            :where (:and (:in 'sp1.seq (:set seq-set))
                         (:= 'sp1.tag "pos")
                         (:is-null 'sp2.id)))
   :column))

;; *skip-words* *(semi-/non-)final-prt* *weak-conj-forms* *skip-conj-forms* are defined in dict-errata.lisp

(defun calc-score (reading &key final use-length (score-mod 0) kanji-break &aux ctr-mode)
  (declare (optimize (speed 3) (debug 3) (safety 1)))
  (declare (type (or null (integer 0 10000)) use-length))
  (typecase reading
    (compound-text
     (let ((args (list (score-base reading) :use-length (mora-length (text reading)) :score-mod (score-mod reading))))
       (multiple-value-bind (score info) (apply 'calc-score args)
         (setf (getf info :conj) (word-conj-data reading))
         (when kanji-break
           (setf score (apply 'kanji-break-penalty kanji-break score :info info :text (text (car args)) (cdr args))))
         (return-from calc-score
           (values score info)))))
    (counter-text
     (setf ctr-mode t)))

  (let* ((score 1) (prop-score 0)
         (kanji-p (eql (word-type reading) :kanji))
         (katakana-p (and (not kanji-p) (> (the fixnum (count-char-class (true-text reading) :katakana-uniq)) 0)))
         (text (text reading))
         (n-kanji (count-char-class text :kanji))
         ;(kanji-prefix (kanji-prefix text))
         (len (max 1 (the fixnum (mora-length text))))
         (seq (the (or null fixnum) (seq reading)))
         (ord (ord reading))
         (entry (and seq (get-dao 'entry seq)))
         (conj-only (let ((wc (word-conjugations reading))) (and wc (not (eql wc :root)))))
         (root-p (or ctr-mode (and (not conj-only) (root-p entry))))
         (conj-data (word-conj-data reading))

         (secondary-conj-p (and conj-data
                                (or (every 'conj-data-via conj-data)
                                    ;; if this is nil, delete all secondary conjugations from conj data
                                    (and (setf conj-data (delete-if 'conj-data-via (the cons conj-data))) nil))))

         (conj-of (mapcar #'conj-data-from conj-data))
         (conj-props (mapcar 'conj-data-prop conj-data))
         (conj-types (mapcar 'conj-type conj-props))
         (conj-types-p (or root-p use-length
                           (notevery (lambda (prop)
                                       (test-conj-prop prop *weak-conj-forms*))
                                     conj-props)))
         (seq-set (and seq (cons seq conj-of))) ;;(if root-p (list seq) (cons seq conj-of)))
         (sp-seq-set (if (and seq root-p (not use-length)) (list seq) seq-set))
         (prefer-kana
          (select-dao 'sense-prop (:and (:in 'seq (:set sp-seq-set))
                                        (:= 'tag "misc") (:= 'text "uk"))))
         (is-arch (every 'is-arch sp-seq-set))
         (posi (if ctr-mode (list "ctr")
                   (get-non-arch-posi seq-set)))
         (common (if conj-only :null (common reading)))
         (common-of common)
         (common-p (not (eql common :null)))
         (particle-p (member "prt" posi :test 'equal))
         (semi-final-particle-p (member seq *semi-final-prt*))
         (non-final-particle-p (member seq *non-final-prt*))
         (pronoun-p (member "pn" posi :test 'equal))
         (cop-da-p (intersection seq-set *copulae*))
         (long-p (> len
                    (cond
                      ((and kanji-p (not prefer-kana)
                            (or (and root-p (not conj-data))
                                (and use-length (member 13 conj-types))))
                       2)
                      ((and common-p (< 0 (the fixnum common) 10)) 2)
                      ((and (intersection '(3 9) conj-types) (not use-length)) 4)
                      (t 3))))
         (no-common-bonus (or particle-p
                              (not conj-types-p)
                              (and (not long-p) (equal posi '("int")))))
         (primary-p nil)
         (use-length-bonus 0)
         split-info)
    (declare (type (integer 0 1000000) score prop-score use-length-bonus)
             (type (integer 0 10000) len ord n-kanji)
             (type (or fixnum (eql :null)) common)
             (type string text))
    (when (or (intersection seq-set *skip-words*)
              (and (not final) (member seq *final-prt*))
              (and (not root-p) (skip-by-conj-data conj-data)))
      (return-from calc-score 0))
    (when (and conj-data (not (and (= ord 0) common-p)))
      (let ((conj-of-data (loop for ot in (get-original-text reading :conj-data conj-data)
                             collect (list (common ot) (ord ot)))))
        (when conj-of-data
          (unless common-p
            (let ((conj-of-common (mapcan (lambda (row) (unless (eql (car row) :null) (list (car row)))) conj-of-data)))
              (declare (type list conj-of-common))
              (when conj-of-common
                (setf common 0 common-p t common-of (car (sort conj-of-common #'compare-common))))))
          (let ((conj-of-ord (reduce 'min conj-of-data :key 'second)))
            (declare (type fixnum conj-of-ord))
            (when (< conj-of-ord ord) (setf ord conj-of-ord))))))

    (unless is-arch
      (setf primary-p
            (or (not entry)
                (and prefer-kana conj-types-p
                     (not kanji-p)
                     (or (not (primary-nokanji entry))
                         (nokanji reading)))
                (and (or (= ord 0) cop-da-p)
                     (or kanji-p conj-types-p)
                     (or (and kanji-p (not prefer-kana))
                         (and common-p pronoun-p)
                         (= (the fixnum (n-kanji entry)) 0)))
                (and prefer-kana kanji-p (= ord 0)
                     (not (query (:select 'id :from 'sense
                                          :where (:and (:in 'id (:set (mapcar 'sense-id prefer-kana)))
                                                       (:= 'ord 0))))))
                )))

    (when primary-p
      (incf score (cond (long-p 10)
                        ((and secondary-conj-p (not kanji-p)) 2)
                        ((and common-p conj-types-p) 5)
                        ((or prefer-kana (not entry) (= (the fixnum (n-kanji entry)) 0)) 3)
                        (t 2))))
    (when (and particle-p (or final (not semi-final-particle-p)))
      (incf score 2)
      (when common-p
        (incf score (+ 2 len)))
      (when (and final (not non-final-particle-p))
        (cond (primary-p (incf score 5))
              (semi-final-particle-p (incf score 2)))))
    (when (and common-p (not no-common-bonus))
      (let ((common-bonus
             (cond
               ((and secondary-conj-p (not use-length)) (if (and kanji-p primary-p) 4 2))
               ((or long-p cop-da-p (and root-p (or kanji-p (and primary-p (> len 2)))))
                (cond ((= common 0) 10)
                      ((not primary-p) (max (- 15 common) 10))
                      (t (max (- 20 common) 10))))
               (kanji-p 8)
               (primary-p 4)
               ((or (> len 2) (< 0 common 10)) 3)
               (t 2))))
        (declare (type fixnum common-bonus))
        (when (and (>= common-bonus 10) (find 10 conj-types))
          (decf common-bonus 4))
        (incf score common-bonus)))
    (when long-p
      (setf score (max len score)))
    (when kanji-p
      (setf score (max (if is-arch 3 5) score))
      (when (and long-p (or (> n-kanji 1) (> len 4)))
        (incf score 2)))
    (when ctr-mode
      (setf score (max 5 score)))
    (setf prop-score score)
    (setf score (* prop-score (+ (length-multiplier-coeff len (if (or kanji-p katakana-p) :strong :weak))
                                 (if (> n-kanji 1) (* (1- n-kanji) 5) 0))))

    (when use-length
      (incf use-length-bonus
            (* prop-score (length-multiplier-coeff (- use-length len)
                                                   (if (and (> len 3) (or kanji-p katakana-p)) :ltail :tail))))
      (incf use-length-bonus
            (the (integer 0 10000) (apply-score-mod score-mod prop-score (- use-length len))))
      (incf score use-length-bonus))

    (unless ctr-mode
      (multiple-value-bind (split score-mod-split) (get-split reading conj-of)
        (declare (type (or null (integer -10000 10000)) score-mod-split))
        (cond
          ((member :score split)
           (incf score score-mod-split)
           (setf split-info score-mod-split))
          ((member :pscore split)
           (let ((new-prop-score (max 1 (+ prop-score score-mod-split))))
             (setf score (ceiling (* score new-prop-score) prop-score)
                   prop-score new-prop-score)))
          (split
           (setf score
                 (+ score-mod-split
                    (loop with nparts = (length split)
                       for part in split
                       for cnt of-type fixnum from 1
                       for last = (= cnt nparts)
                       for ptext of-type vector = (text part)
                       for plen of-type fixnum = (length ptext)
                       for slen of-type fixnum = plen then (+ slen plen)
                       for pmlen of-type (integer 0 10000) = (mora-length (text part))
                       for smlen of-type (integer 0 10000) = pmlen then (+ smlen pmlen)
                       for tpart = (if (and last (> slen (length text)))
                                       (let ((new-len (max 1 (+ plen (- (length text) slen)))))
                                         (make-instance 'proxy-text :source part :text (subseq ptext 0 new-len) :kana ""))
                                       part)
                       for part-score = (calc-score tpart
                                                    :final (and final last)
                                                    :use-length (when (and last use-length)
                                                                  (+ pmlen (- use-length smlen)))
                                                    :score-mod (if last score-mod 0))
                       collect part-score into part-scores
                       finally
                         (setf split-info (cons score-mod-split part-scores))
                         (return (the fixnum (reduce '+ part-scores))))))))))

    (let ((info (list :posi posi :seq-set (if ctr-mode seq-set (cons seq conj-of))
                      :conj conj-data
                      :common (and common-p common-of)
                      :score-info (list prop-score kanji-break use-length-bonus split-info)
                      :kpcl (list (or kanji-p katakana-p) primary-p common-p long-p))))
      (when kanji-break (setf score (kanji-break-penalty kanji-break score
                                                         :info info :text text :use-length use-length :score-mod score-mod)))
      (values score info))))

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

(defstruct segment-list segments start end (top nil) (matches 0))

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

(defun find-word-full (word &key as-hiragana counter)
  (let ((simple-words (find-word word)))
    (nconc simple-words
           (find-word-suffix word :matches simple-words)
           (when as-hiragana
             (find-word-as-hiragana word :exclude (mapcar 'seq simple-words)))
           (when counter
             (case counter
               (:auto
                (let ((groups (consecutive-char-groups :number word)))
                  (when groups
                    (find-counter (subseq word (caar groups) (cdar groups))
                                  (subseq word (cdar groups) (length word))))))
               (t (let ((number (subseq word 0 counter))
                        (counter (subseq word counter (length word))))
                    (find-counter number counter :unique (not simple-words)))))))))

(defparameter *score-cutoff* 5) ;; this must filter out ONLY bad kana spellings, and NOT filter out any kanji spellings

(defun join-substring-words* (str)
  (loop with sticky = (find-sticky-positions str)
        with substring-hash = (find-substring-words str :sticky sticky)
        with katakana-groups = (consecutive-char-groups :katakana str)
        with number-groups = (consecutive-char-groups :number str)
        and kanji-break and ends
       with suffix-map = (get-suffix-map str)
       for start from 0 below (length str)
       for katakana-group-end = (cdr (assoc start katakana-groups))
       for number-group-end = (cdr (assoc start number-groups))
       unless (member start sticky)
       nconcing
       (loop for end from (1+ start) upto (min (length str) (+ start *max-word-length*))
            unless (member end sticky)
            nconcing
            (let* ((part (subseq str start end))
                   (segments (mapcar
                              (lambda (word)
                                (make-segment :start start :end end :word word))
                              (let ((*suffix-map-temp* suffix-map)
                                    (*suffix-next-end* end)
                                    (*substring-hash* substring-hash))
                                (find-word-full part
                                                :as-hiragana (and katakana-group-end (= end katakana-group-end))
                                                :counter (and number-group-end
                                                              (<= number-group-end end)
                                                              (let ((d (- number-group-end start)))
                                                                (and (<= d 20) d))))))))
              (when segments
                (when (or (= start 0) (find start ends))
                  (setf kanji-break
                        (nconc (if (find part *force-kanji-break* :test 'equal)
                                   (alexandria:iota (1- (length part)) :start (1+ start))
                                   (sequential-kanji-positions part start))
                               kanji-break)))
                (pushnew end ends)
                (list (list start end segments)))))
       into result
     finally (return (values result kanji-break))))

(defun join-substring-words (str)
  (multiple-value-bind (result kanji-break) (join-substring-words* str)
    (loop
       with ends-with-lw = (alexandria:ends-with #\ー str)
       for (start end segments) in result
       for kb = (mapcar (lambda (n) (- n start)) (intersection (list start end) kanji-break))
       for sl = (loop for segment in segments
                   do (gen-score segment
                                 :final (or (= (segment-end segment) (length str))
                                            (and ends-with-lw
                                                 (= (segment-end segment) (1- (length str)))))
                                 :kanji-break kb)
                   if (>= (segment-score segment) *score-cutoff*)
                   collect segment)
       when sl
       collect (make-segment-list :segments (cull-segments sl) :start start :end end
                                  :matches (length segments)))))

(defun substring-index(str)
  (let ((sls (join-substring-words str))
        (index (make-hash-table :test 'equal)))
    (loop for sl in sls
       do (setf (gethash (list (segment-list-start sl) (segment-list-end sl)) index) sl))
    index))

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

(defun get-seg-initial (seg)
  (loop for split in (apply-segfilters nil seg)
     collect (cadr split)))

(defun get-seg-splits (seg-left seg-right)
  (let ((splits (apply-segfilters seg-left seg-right)))
    (loop for (seg-left seg-right) in splits
         nconcing (cons (get-penalties seg-left seg-right) (get-synergies seg-left seg-right)))))

(defun expand-segment-list (segment-list)
  (setf (segment-list-segments segment-list)
        (stable-sort
         (loop for segment in (segment-list-segments segment-list)
            for segsplit = (get-segsplit segment)
            collect segment
            when segsplit
            collect segsplit and do (incf (segment-list-matches segment-list)))
         #'> :key #'segment-score)))

(defun find-best-path (segment-lists str-length &key (limit 5))
  "generalized version of old find-best-path that operates on segment-lists and uses synergies"
  (let ((top (make-instance 'top-array :limit limit)))
    (register-item top (gap-penalty 0 str-length) nil)

    (dolist (segment-list segment-lists)
      (expand-segment-list segment-list)
      (setf (segment-list-top segment-list) (make-instance 'top-array :limit limit)))

    ;;assume segments are sorted by (start, end) (as is the result of join-substring-words)
    (loop for (seg1 . rest) on segment-lists
       do
         (let ((gap-left (gap-penalty 0 (segment-list-start seg1)))
               (gap-right (gap-penalty (segment-list-end seg1) str-length)))
           (let ((initial-segs (get-seg-initial seg1)))
             (loop for seg in initial-segs
                for score1 = (get-segment-score seg)
                do
                  (register-item (segment-list-top seg1) (+ gap-left score1) (list seg))
                  (register-item top (+ gap-left score1 gap-right) (list seg)))))
         (loop for seg2 in rest
            for score2 = (get-segment-score seg2)
            when (>= (segment-list-start seg2) (segment-list-end seg1)) do
              (loop with gap-left = (gap-penalty (segment-list-end seg1) (segment-list-start seg2))
                   and gap-right = (gap-penalty (segment-list-end seg2) str-length)
                   for tai across (get-array (segment-list-top seg1))
                   for (seg-left . tail) = (tai-payload tai)
                   for score3 = (get-segment-score seg-left)
                   for score-tail = (- (tai-score tai) score3)
                   do (loop for split in (get-seg-splits seg-left seg2)
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
   (true-text :initarg :true-text :initform nil :accessor word-info-true-text)
   (kana :initarg :kana :accessor word-info-kana)
   (seq :initarg :seq :initform nil :accessor word-info-seq)
   (conjugations :initarg :conjugations :initform nil :accessor word-info-conjugations)
   (score :initarg :score :initform 0 :accessor word-info-score)
   (components :initarg :components :initform nil :accessor word-info-components)
   (alternative :initarg :alternative :initform nil :accessor word-info-alternative)
   (primary :initarg :primary :initform t :accessor word-info-primary)
   (start :initarg :start :initform nil :accessor word-info-start)
   (end :initarg :end :initform nil :accessor word-info-end)
   (counter :initarg :counter :initform nil :accessor word-info-counter)
   (skipped :initarg :skipped :initform 0 :accessor word-info-skipped)
   ))

(defun word-info-json (word-info)
  (with-slots (type text true-text kana seq conjugations score components
               alternative primary start end counter skipped)
      word-info
    (jsown:new-js
      ("type" (symbol-name type))
      ("text" text)
      ("truetext" true-text)
      ("kana" kana)
      ("seq" seq)
      ("conjugations" (if (eql conjugations :root) "ROOT" conjugations))
      ("score" score)
      ("components" (mapcar #'word-info-json components))
      ("alternative" alternative)
      ("primary" primary)
      ("start" start)
      ("end" end)
      ("counter" counter)
      ("skipped" skipped))))

(defun simple-word-info (seq text reading type &key (as :object))
  (let ((obj (make-instance 'word-info :type type :text text :true-text text :seq seq :kana reading)))
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
(def-reader-for-json word-info-true-text "truetext")
(def-reader-for-json word-info-kana "kana")
(def-reader-for-json word-info-seq "seq")
(def-reader-for-json word-info-score "score")
(def-reader-for-json word-info-components "components")
(def-reader-for-json word-info-alternative "alternative")
(def-reader-for-json word-info-primary "primary")
(def-reader-for-json word-info-start "start")
(def-reader-for-json word-info-end "end")
(def-reader-for-json word-info-counter "counter")
(def-reader-for-json word-info-skipped "skipped")

(defmethod print-object ((obj word-info) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a ~a[~a] score=~a"
            (word-info-seq obj) (word-info-text obj) (word-info-kana obj) (word-info-score obj))))

(defun word-info-from-segment (segment &aux (word (segment-word segment)))
  (make-instance 'word-info
                 :type (word-type word)
                 :text (get-text segment)
                 :kana (get-kana word)
                 :seq (seq word)
                 :conjugations (when (typep word 'simple-text) (word-conjugations word))
                 :true-text (when (typep word 'simple-text) (true-text word))
                 :components (when (typep word 'compound-text)
                               (loop with primary-seq = (seq (primary word))
                                  for wrd in (words word)
                                  collect (make-instance 'word-info
                                                         :type (word-type wrd)
                                                         :text (get-text wrd)
                                                         :true-text (true-text wrd)
                                                         :kana (get-kana wrd)
                                                         :seq (seq wrd)
                                                         :conjugations (word-conjugations wrd)
                                                         :primary (= (seq wrd) primary-seq))))
                 :counter (when (typep word 'counter-text) (list (value-string word) (ordinalp word)))
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
                             wi-list*))
         (matches (segment-list-matches segment-list)))
    (if (= (length wi-list) 1)
        (prog1 wi1
          (setf (word-info-skipped wi1) (- matches 1)))
        (loop for wi in wi-list
           collect (word-info-kana wi) into kana-list
           collect (word-info-seq wi) into seq-list
           finally (return (make-instance 'word-info
                                          :type (word-info-type wi1)
                                          :text (word-info-text wi1)
                                          :kana (remove-duplicates kana-list :test 'equal :from-end t)
                                          :seq seq-list
                                          :components wi-list
                                          :alternative t
                                          :score (word-info-score wi1)
                                          :start (segment-list-start segment-list)
                                          :end (segment-list-end segment-list)
                                          :skipped (- matches (length wi-list))
                                          ))))))

(defun word-info-from-text (text)
  (with-connection *connection*
    (let* ((readings (find-word-full text :counter :auto))
           (segments (loop for r in readings collect (gen-score (make-segment :start 0 :end (length text) :word r :text text))))
           (segment-list (make-segment-list :segments segments :start 0 :end (length text)
                                            :matches (length segments))))
      (word-info-from-segment-list segment-list))))

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

(defun word-info-reading (word-info)
  (let ((table (case (word-info-type word-info) (:kanji 'kanji-text) (:kana 'kana-text)))
        (true-text (word-info-true-text word-info)))
    (when (and table true-text)
      (car (select-dao table (:= 'text true-text))))))

(defun dict-segment (str &key (limit 5))
  (with-connection *connection*
    (loop for (path . score) in (find-best-path (join-substring-words str) (length str) :limit limit)
         collect (cons (fill-segment-path str path) score))))

(defun simple-segment (str &key (limit 5))
  (caar (dict-segment str :limit limit)))

(defun get-senses-raw (seq &aux (tags '("pos" "s_inf" "stagk" "stagr")))
  (let* ((glosses
          (query (:order-by
                  (:select 'sense.ord (:raw "string_agg(gloss.text, '; ' ORDER BY gloss.ord)")
                           :from 'sense :left-join 'gloss :on (:= 'gloss.sense-id 'sense.id)
                           :where (:= 'sense.seq seq)
                           :group-by 'sense.id)
                  'sense.ord)))
         (props
          (query (:order-by
                  (:select 'sense.ord 'sense-prop.tag 'sense-prop.text
                           :from 'sense 'sense-prop
                           :where (:and (:= 'sense.seq seq)
                                        (:= 'sense-prop.sense-id 'sense.id)
                                        (:in 'sense-prop.tag (:set tags))))
                  'sense.ord 'sense-prop.tag 'sense-prop.ord)))
         (sense-list (loop for (sord gloss) in glosses
                          collect (list :ord sord :gloss (if (eql gloss :null) "" gloss) :props nil))))
    (loop with cursord and curtag and curprop and bag
       for (sord tag text) in props
       if (or (not (eql sord cursord)) (not (equal tag curtag)))
       do (when curprop (push (cons curtag (reverse bag)) (getf curprop :props)))
         (setf cursord sord curtag tag bag nil
               curprop (find sord sense-list :key 'cadr))
       do (push text bag)
       finally (when curprop (push (cons curtag bag) (getf curprop :props))))
    sense-list))

(defun get-senses (seq)
  (loop for sense in (get-senses-raw seq)
       for props = (getf sense :props)
       for gloss = (getf sense :gloss)
       for pos = (cdr (assoc "pos" props :test 'equal))
       for pos-str = (format nil "[~{~a~^,~}]" pos)
       collect (list pos-str gloss props)))

(defun get-senses-str (seq)
  (with-output-to-string (s)
    (loop for (pos gloss props) in (get-senses seq)
          for i from 1
          for rpos = pos then (if (equal pos "[]") rpos pos)
          for inf = (cdr (assoc "s_inf" props :test 'equal))
          for rinf = (when inf (join "; " inf))
          when (> i 1) do (terpri s)
          do (format s "~a. ~a ~@[《~a》 ~]~a" i rpos rinf gloss))))


(defun match-kana-kanji (kana-reading kanji-reading restricted)
  (cond ((nokanji kana-reading) nil)
        (t (let* ((kana-text (text kana-reading))
                  (restr (loop for (rt kt) in restricted when (equal kana-text rt) collect kt)))
             (if restr
                 (find (text kanji-reading) restr :test 'equal)
                 t)))))

(defun match-sense-restrictions (seq props reading)
  (let ((stagk (cdr (assoc "stagk" props :test 'equal)))
        (stagr (cdr (assoc "stagr" props :test 'equal)))
        (wtype (word-type reading)))
    (cond ((and (not stagk) (not stagr)) t)
          ((or (member (text reading) stagk :test 'equal)
               (member (text reading) stagr :test 'equal)) t)
          ((and (not stagr) (eql wtype :kanji)) nil)
          ((and (not stagk) (eql wtype :kana)) nil)
          (t (let ((restricted (query (:select 'reading 'text :from 'restricted-readings :where (:= 'seq seq)))))
               (case wtype
                 (:kanji
                  (let ((rkana (select-dao 'kana-text (:and (:= 'seq seq) (:in 'text (:set stagr))))))
                    (some (lambda (rk) (match-kana-kanji rk reading restricted)) rkana)))
                 (:kana
                  (let ((rkanji (select-dao 'kanji-text (:and (:= 'seq seq) (:in 'text (:set stagk))))))
                    (some (lambda (rk) (match-kana-kanji reading rk restricted)) rkanji)))))))))


(defun split-pos (pos-str)
  (split-sequence #\, pos-str :start 1 :end (1- (length pos-str))))

(defun get-senses-json (seq &key pos-list reading reading-getter)
  (loop with readp
     for (pos gloss props) in (get-senses seq)
     for emptypos = (equal pos "[]")
     for rpos = pos then (if emptypos rpos pos)
     for lpos = (split-pos pos) then (if emptypos lpos (split-pos pos))
     for inf = (cdr (assoc "s_inf" props :test 'equal))
     for rinf = (when inf (join "; " inf))
     when (and (or (not pos-list) (intersection lpos pos-list :test 'equal))
               (or (not (or reading-getter reading))
                   (not (or (assoc "stagk" props :test 'equal)
                            (assoc "stagr" props :test 'equal)))
                   (let ((rr (or reading
                                 (and (not readp)
                                      (setf readp t
                                            reading (funcall reading-getter))))))
                     (if rr (match-sense-restrictions seq props rr) t))))
     collect (let ((js (jsown:new-js ("pos" rpos) ("gloss" gloss))))
               (if rinf (jsown:extend-js js ("info" rinf)))
               js)))

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

(defun reading-str* (kanji kana)
  (if kanji
      (format nil "~a 【~a】" kanji kana)
      kana))

(defun reading-str-seq (seq)
  (let* ((kanji-text (car (query (:select 'text :from 'kanji-text :where (:and (:= 'seq seq) (:= 'ord 0))) :column)))
         (kana-text (car (query (:select 'text :from 'kana-text :where (:and (:= 'seq seq) (:= 'ord 0))) :column))))
    (reading-str* kanji-text kana-text)))

(defgeneric reading-str (obj)
  (:method ((obj simple-text))
    (reading-str* (get-kanji obj) (get-kana obj)))
  (:method ((obj integer))
    (reading-str-seq obj)))

(defun entry-info-short (seq &key with-pos)
  (let ((sense-str (short-sense-str seq :with-pos with-pos)))
    (with-output-to-string (s)
      (format s "~a : " (reading-str-seq seq))
      (when sense-str (princ sense-str s)))))

(defun entry-info-long (seq)
  (format nil "~a~@[ ~a~%~]~a" seq (reading-str-seq seq) (get-senses-str seq)))

(defun select-conjs (seq &optional conj-ids)
  (if conj-ids
      (unless (eql conj-ids :root)
        (select-dao 'conjugation (:and (:= 'seq seq) (:in 'id (:set conj-ids)))))
      (or
       (select-dao 'conjugation (:and (:= 'seq seq) (:is-null 'via)))
       (select-dao 'conjugation (:= 'seq seq)))))

(defun conj-type-order (conj-type)
  ;; swaps Continuative and Imperative so that the former is shown first
  (case conj-type
    (10 13)
    (13 10)
    (t conj-type)))

(defun select-conjs-and-props (seq &optional conj-ids)
  (sort
   (loop for conj in (select-conjs seq conj-ids)
      for props = (select-dao 'conj-prop (:= 'conj-id (id conj)))
      for val = (loop for prop in props minimizing (conj-type-order (conj-type prop)))
      collect (list conj props (list (if (eql (seq-via conj) :null) 0 1) val)))
   (lex-compare '<)
   :key 'third))

(defun print-conj-info (seq &key conjugations (out *standard-output*))
  (loop with via-used = nil
     for (conj props) in (select-conjs-and-props seq conjugations)
     for via = (seq-via conj)
     unless (member via via-used)
     do (loop for conj-prop in props
           for first = t then nil
           do (format out "~%~:[ ~;[~] Conjugation: ~a" first (conj-info-short conj-prop)))
       (if (eql via :null)
           (format out "~%  ~a" (entry-info-short (seq-from conj)))
           (progn
             (format out "~% --(via)--")
             (print-conj-info via :out out)
             (push via via-used)))
       (princ " ]" out)))

(defun conj-info-json* (seq &key conjugations text has-gloss)
  (loop with via-used = nil
     for (conj props) in (select-conjs-and-props seq conjugations)
     for via = (seq-via conj)
     unless (member via via-used)
     nconc (block outer
             (let* ((conj-pos nil)
                    (orig-text (get-original-text-once (get-conj-data seq (list (id conj))) text))
                    (js (jsown:new-js
                          ("prop" (loop for conj-prop in props
                                     do (push (pos conj-prop) conj-pos)
                                     collect (conj-prop-json conj-prop))))))
               (if (eql via :null)
                   (let ((orig-reading (when orig-text
                                         ;; TODO: allow multiple readings
                                         (car (find-words-seqs orig-text (seq-from conj))))))
                     (when (and has-gloss (not orig-reading))
                       (return-from outer nil))
                     (jsown:extend-js js
                       ("reading" (reading-str (or orig-reading (seq-from conj))))
                       ("gloss" (get-senses-json (seq-from conj)
                                                 :pos-list conj-pos
                                                 :reading-getter (lambda () orig-reading)))
                       ("readok" (when orig-reading t))))
                   (progn
                     (let ((cij (conj-info-json via :text orig-text :has-gloss has-gloss)))
                       (when cij
                         (jsown:extend-js js
                           ("via" cij)
                           ("readok" (jsown:val (car cij) "readok")))))
                     (push via via-used)))
               (list js)))))

(defun conj-info-json (seq &rest rest &key conjugations text has-gloss)
  (declare (ignorable conjugations text has-gloss))
  (let* ((cij (apply 'conj-info-json* seq rest))
         (fcij (remove-if-not (lambda (c) (jsown:val c "readok")) cij)))
    (or fcij cij)))

(defun simplify-reading-list (reading-list)
  ;; I'm sure there's a simpler way to do this...
  (loop with assoc
     for reading in reading-list
     for (can spos) = (loop with off = 0
                         for char across reading
                         for i from 0
                         if (char= char #\Space) collect (- i off) into spos and do (incf off)
                         else collect char into can
                         finally (return (list (coerce can 'string) (remove-duplicates spos))))
     for a = (assoc can assoc :test 'equal)
     if a do (setf (cddr a) (nconc spos (cddr a))) (incf (cadr a))
     else do (push (list* can 1 spos) assoc)
     finally (return
               (loop for (can cnt . spos) in (nreverse assoc)
                  for sspos = (sort (remove-duplicates spos) '<)
                  collect (with-output-to-string (s)
                            (loop for i from 0
                               for char across can
                               if (eql (car sspos) i)
                               do (write-char (if (= (count (car sspos) spos) cnt) #\Space #\MIDDLE_DOT) s)
                                 (pop sspos)
                               do (write-char char s)))))))

(defun map-word-info-kana (fn word-info &key (separator "/")
                           &aux (wkana (word-info-kana word-info)))
  (if (listp wkana)
      (join separator (simplify-reading-list (mapcar fn wkana)))
      (funcall fn wkana)))

(defun word-info-reading-str (word-info)
  (cond ((or (eql (word-info-type word-info) :kanji)
             (and (word-info-counter word-info) (word-info-seq word-info)))
         (reading-str* (word-info-text word-info) (word-info-kana word-info)))
        (t (reading-str* nil (word-info-text word-info)))))

(defmethod reading-str ((word-info word-info))
  (word-info-reading-str word-info))

(defmethod reading-str ((word-info list))
  (word-info-reading-str word-info))

(defun word-info-str (word-info)
  (with-connection *connection*
    (with-output-to-string (s)
      (labels ((inner (word-info &optional suffix marker)
                 (when marker (princ " * " s))
                 (princ (reading-str word-info) s)
                 (cond
                   ((word-info-components word-info)
                    (progn
                      (format s " Compound word: ~{~a~^ + ~}" (mapcar #'word-info-text (word-info-components word-info)))
                      (dolist (comp (word-info-components word-info))
                        (terpri s)
                        (inner comp (not (word-info-primary comp)) t))))
                   ((word-info-counter word-info)
                    (destructuring-bind (value nil) (word-info-counter word-info)
                      (terpri s) (princ value s)
                      (let ((seq (word-info-seq word-info)))
                        (when seq (terpri s) (princ (get-senses-str seq) s)))))
                   (t
                    (let ((seq (word-info-seq word-info))
                          (conjs (word-info-conjugations word-info))
                          desc)
                      (cond ((and suffix (setf desc (get-suffix-description seq)))
                             (format s "  [suffix]: ~a " desc))
                            ((or (not conjs) (eql conjs :root))
                             (terpri s) (princ (if seq (get-senses-str seq) "???") s)))
                      (when seq
                        (print-conj-info seq :out s
                                         :conjugations conjs)))))))
        (if (word-info-alternative word-info)
            (loop for wi in (word-info-components word-info)
                 for i from 1
                 when (> i 1) do (terpri s)
                 do (format s "<~a>. " i) (inner wi nil nil))
            (inner word-info))))))

(defun word-info-gloss-json (word-info &key root-only)
  (with-connection *connection*
    (labels ((inner (word-info &optional suffix)
               (let ((js (jsown:new-js ("reading" (reading-str word-info))
                                       ("text" (word-info-text word-info))
                                       ("kana" (word-info-kana word-info))
                                       )))
                 (when (word-info-score word-info)
                   (jsown:extend-js js ("score" (word-info-score word-info))))
                 (cond
                   ((word-info-components word-info)
                    (jsown:extend-js js
                      ("compound" (mapcar #'word-info-text (word-info-components word-info)))
                      ("components" (loop for wi in (word-info-components word-info)
                                       collect (inner wi (not (word-info-primary wi)))))))
                   ((word-info-counter word-info)
                    (destructuring-bind (value ordinal) (word-info-counter word-info)
                      (jsown:extend-js js ("counter" (jsown:new-js ("value" value) ("ordinal" ordinal)))))
                    (let ((seq (word-info-seq word-info)))
                      (when seq
                        (jsown:extend-js js ("seq" seq))
                        (let* ((reading-getter (lambda () (word-info-reading word-info)))
                               (gloss (get-senses-json seq :pos-list '("ctr") :reading-getter reading-getter)))
                          (when gloss
                            (jsown:extend-js js ("gloss" gloss)))))))
                   (t
                     (let ((seq (word-info-seq word-info))
                           (conjs (word-info-conjugations word-info))
                           (reading-getter (lambda () (word-info-reading word-info)))
                           desc has-gloss)
                       (when seq
                         (jsown:extend-js js ("seq" seq)))
                       (cond (root-only
                              (return-from inner
                                (jsown:extend-js js ("gloss" (get-senses-json seq :reading-getter reading-getter)))))
                             ((and suffix (setf desc (get-suffix-description seq)))
                              (jsown:extend-js js ("suffix" desc)))
                             ((and seq (or (not conjs) (eql conjs :root)))
                              (let ((gloss (get-senses-json seq :reading-getter reading-getter)))
                                (when gloss
                                  (setf has-gloss t)
                                  (jsown:extend-js js ("gloss" gloss))))))
                       (when seq
                         (jsown:extend-js js
                           ("conj" (conj-info-json seq :conjugations (word-info-conjugations word-info)
                                                   :text (word-info-true-text word-info)
                                                   :has-gloss has-gloss)))))))
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
                          (find-word-full text :as-hiragana (test-word text :katakana) :counter :auto)))
           (segments (loop for word in all-words
                        collect (gen-score (make-segment :start 0 :end end :word word :text text))))
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


(defun find-word-kana-pattern (pattern)
  (stable-sort (select-dao 'kana-text (:~ 'text pattern)) #'compare-common
               :key (lambda (r) (and (not (eql (common r) :null)) (common r)))))

(defun find-kanji-for-pattern (pattern)
  (with-connection *connection*
    (loop for r in (find-word-kana-pattern pattern)
       for k = (get-kanji r)
       when k collect k into kanji
       collect (text r) into kana
       finally (return
                 (values (remove-duplicates kanji :test 'equal :from-end t)
                         (remove-duplicates kana :test 'equal :from-end t))))))

(defun get-glosses (seqs)
  (let ((glosses (query (:order-by
                         (:select 'sense.seq 'gloss.text :from 'gloss 'sense
                                  :where (:and (:in 'sense.seq (:set seqs))
                                               (:= 'gloss.sense-id 'sense.id)))
                         'sense.seq))))
    (loop with al
       for (seq text) in glosses
       if (eql (caar al) seq) do (push text (cdar al))
       else do (push (list seq text) al)
       finally (return (nreverse al)))))

(defun get-candidates (text reading)
  (let ((is-kana (test-word text :kana)))
    (if is-kana
        (query (:order-by
                (:select 'e.seq :from (:as 'entry 'e)
                         :left-join (:as 'kana-text 'r) :on (:= 'e.seq 'r.seq)
                         :left-join (:as 'kanji-text 'k) :on (:= 'e.seq 'k.seq)
                         :where (:and 'e.root-p (:is-null 'k.text) (:= 'r.text text) (:= 'r.ord 0)))
                'e.seq)
               :column)
        (query (:order-by
                (:select 'e.seq :from (:as 'entry 'e)
                         :left-join (:as 'kana-text 'r) :on (:= 'e.seq 'r.seq)
                         :left-join (:as 'kanji-text 'k) :on (:= 'e.seq 'k.seq)
                        :where (:and (:= 'k.text text) (:= 'k.ord 0) (:= 'r.text reading) (:= 'r.ord 0)))
                'e.seq)
               :column))))

(defun match-glosses (text reading words &key (normalize 'identity) update-gloss)
  (with-connection *connection*
    (let ((candidates (get-candidates text reading))
          (nwords (mapcar normalize words)))
      (when candidates
        (let ((matched (loop for (seq . glosses) in (get-glosses candidates)
                          for match = (loop for gloss in (nreverse glosses)
                                            for ngloss = (funcall normalize gloss)
                                         when (and update-gloss (ppcre:scan update-gloss ngloss))
                                           do (return gloss)
                                         thereis (loop for word in nwords always (search word ngloss)))
                          thereis (cond ((stringp match) (list seq match))
                                        (match seq)))))
          (if matched
              (values matched t)
              (values (car candidates) nil)))))))
