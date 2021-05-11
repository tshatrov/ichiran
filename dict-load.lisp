;; code for loading data from jmdict dump

(in-package :ichiran/dict)

;; TODO: refactor into a separate package

(defun init-tables ()
  (with-connection *connection*
    (let ((tables '(entry kanji-text kana-text sense gloss sense-prop conjugation conj-prop
                    conj-source-reading restricted-readings)))
      (loop for table in (reverse tables)
         do (drop-table table :if-exists t))
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
    (query (:update 'entry
                    :set 'primary-nokanji primary-nokanji
                    (ecase table (kana-text 'n-kana) (kanji-text 'n-kanji)) (length to-add)
                    :where (:= 'seq seq)))))


(defun insert-sense-traits (sense-node tag sense-id seq)
  (do-node-list-ord (ord node (dom:get-elements-by-tag-name sense-node tag))
    (make-dao 'sense-prop :sense-id sense-id :tag tag :text (node-text node) :ord ord :seq seq)))

(defun insert-senses (node-list seq)
  (do-node-list-ord (ord node node-list)
    (let ((sense-id (id (make-dao 'sense :seq seq :ord ord))))
      (do-node-list-ord (ord node (dom:get-elements-by-tag-name node "gloss"))
        (make-dao 'gloss :sense-id sense-id :text (node-text node) :ord ord))
      (loop for tag in '("pos" "misc" "dial"
                         "s_inf" "stagk" "stagr")
           do (insert-sense-traits node tag sense-id seq)))))

(defun sense-exists-p (senses positions glosses)
  (loop
     with glosses-str = (join "; " glosses)
     for sense in senses
     for props = (getf sense :props)
     for gloss = (getf sense :gloss)
     for pos = (cdr (assoc "pos" props :test 'equal))
     for rpos = pos then (or pos rpos)
     thereis (and (equal rpos positions)
                  (equal glosses-str gloss))))

(defun add-new-sense (seq positions glosses &aux (senses (get-senses-raw seq)))
  (when (sense-exists-p senses positions glosses)
    (return-from add-new-sense nil))
  (let* ((last-sense (car (last senses)))
         (ord (1+ (getf last-sense :ord)))
         (last-pos (loop for s in (reverse senses)
                      for props = (getf s :props)
                      for pos = (cdr (assoc "pos" props :test 'equal))
                      thereis pos))
         (sense-id (id (make-dao 'sense :seq seq :ord ord))))
    (loop for gord from 0
       for gloss in glosses
       do (make-dao 'gloss :sense-id sense-id :text gloss :ord gord))
    (unless (equal last-pos positions)
      (loop for sord from 0
         for pos in positions
         do (make-dao 'sense-prop :sense-id sense-id :tag "pos" :text pos :ord sord :seq seq)))
    (values sense-id ord)))

(defun next-seq ()
  (1+ (query (:select (:max 'seq) :from 'entry) :single)))

(defun load-entry (content &key if-exists upstream seq)
  (let* ((parsed (typecase content
                   (dom:node
                    (unless (typep content 'dom:document)
                      (setf content (rune-dom:create-document content)))
                    (prog1 content
                      (setf content (dom:map-document (cxml:make-string-sink) content))))
                   (t (cxml:parse content (cxml-dom:make-dom-builder)))))
         (seq (cond
                ((stringp seq)
                 ;; if reading exists use its seq, otherwise choose next available seq
                 (let ((word (find-word seq)))
                   (if word
                       (seq (car word))
                       (next-seq))))
                (seq seq)
                (t (let ((entseq-node (dom:item (dom:get-elements-by-tag-name parsed "ent_seq") 0)))
                     (parse-integer (node-text entseq-node)))))))
    (when upstream
      (let ((entry (get-dao 'entry (car upstream))))
        (when (and entry (equal (get-text entry) (cadr upstream)))
          (return-from load-entry))))
    (case if-exists
      (:skip (when (get-dao 'entry seq) (return-from load-entry)))
      (:overwrite (let ((entry (get-dao 'entry seq)))
                    (when entry (delete-dao entry)))))

    (make-dao 'entry :seq seq :content content :root-p t)
    (let* ((kanji-nodes (dom:get-elements-by-tag-name parsed "k_ele"))
           (kana-nodes (dom:get-elements-by-tag-name parsed "r_ele"))
           (sense-nodes (dom:get-elements-by-tag-name parsed "sense")))
      (insert-readings kanji-nodes "keb" 'kanji-text seq "ke_pri")
      (insert-readings kana-nodes "reb" 'kana-text seq "re_pri")
      (insert-senses sense-nodes seq))
    seq))

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
         finally (recalc-entry-stats-all) (query "ANALYZE") (format t "~a entries total~%" cnt)))
    (when load-extras (load-extras))))

(defun load-extras ()
  (format t "Loading conjugations...~%")
  (load-conjugations)
  (format t "Loading secondary conjugations...~%")
  (load-secondary-conjugations)
  (format t "Loading custom data...~%")
  (ichiran/custom:load-custom-data nil t)
  (add-errata)
  (recalc-entry-stats-all)
  (query "ANALYZE"))

(defun drop-extras ()
  (query (:delete-from 'conj-prop))
  (query (:delete-from 'conj-source-reading))
  (query (:delete-from 'conjugation))
  (query (:delete-from 'entry :where (:not 'root-p)))
  )


;;; conjugations generator (warning: terrible code ahead)

(defmacro csv-hash (hash-name (filename &key skip-first ((:errata errata-fn)) relative) loader-opts &rest accessor-opts-list)
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
                                   ,(if relative
                                      `(asdf:system-relative-pathname ,relative ,filename)
                                      `(merge-pathnames *jmdict-data* ,filename))
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

(csv-hash *pos-index* ("kwpos.csv" :skip-first t)
          ((pos-id pos description) pos (cons (parse-integer pos-id) description))
          (val (car val)))

(csv-hash *pos-by-index* ("kwpos.csv" :skip-first t)
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


(defparameter *do-not-conjugate* '("n" "vs" "adj-na"))

(defparameter *do-not-conjugate-seq* '(2765070 2835284))

(defparameter *pos-with-conj-rules*
 '("adj-i" "adj-ix" "cop-da" "v1" "v1-s" "v5aru"
   "v5b" "v5g" "v5k" "v5k-s" "v5m" "v5n" "v5r" "v5r-i" "v5s"
   "v5t" "v5u" "v5u-s" "vk" "vs-s" "vs-i"))

(defparameter *secondary-conjugation-types-from* `(5 6 7 8 ,+conj-causative-su+))

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
         (next-seq (next-seq)))
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
               (sort
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
                           :column))
                '<)))
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
           (unless (select-dao 'conj-prop
                               (:and (:= 'conj-id conj-id)
                                     (:= 'conj-type conj-type)
                                     (:= 'pos pos)
                                     (:=== 'neg neg)
                                     (:=== 'fml fml)))
             (make-dao 'conj-prop :conj-id conj-id :conj-type conj-type :pos pos :neg neg :fml fml))

           (let* ((old-csr (when old-conj (query (:select 'text 'source-text :from 'conj-source-reading :where (:= 'conj-id conj-id)))))
                  (source-readings (remove-duplicates (set-difference source-readings old-csr :test 'equal) :test 'equal)))
             (loop for (text source-text) in source-readings
                unless (select-dao 'conj-source-reading
                                   (:and (:= 'conj-id conj-id)
                                         (:= 'text text)
                                         (:= 'source-text source-text)))
                do (make-dao 'conj-source-reading :conj-id conj-id :text text :source-text source-text))))

         (return (not seq-candidates)))))

(defun load-conjugations ()
  (with-connection *connection*
    (let ((seqs (query (:select 'seq :distinct :from 'sense-prop
                                :where (:and (:not (:in 'seq (:set *do-not-conjugate-seq*)))
                                             (:= 'tag "pos")
                                             (:in 'text (:set *pos-with-conj-rules*))))
                       :column)))
      (loop for cnt from 1
           for seq in seqs
           do (conjugate-entry-outer seq)
           if (zerop (mod cnt 500)) do (format t "~a entries processed~%" cnt)))))


(defun load-secondary-conjugations ()
  (let ((to-conj (query (:select 'conj.from 'conj.seq 'conj-prop.conj-type
                         :distinct-on 'conj.from 'conj.seq
                         :from (:as 'conjugation 'conj)
                         :left-join 'conj-prop :on (:= 'conj.id 'conj-prop.conj-id)
                                                    :where (:and (:in 'conj-prop.conj-type (:set *secondary-conjugation-types-from*))
                                                                 (:not (:in 'conj-prop.pos (:set "vs-i" "vs-s")))
                                                                 (:or (:not 'neg) (:is-null 'neg))
                                                                 (:or (:not 'fml) (:is-null 'fml)))))))
    (loop for cnt from 1
          for (seq-from seq conj-type) in to-conj
          do (conjugate-entry-outer seq-from :via seq
                                             :as-posi (list (if (= conj-type +conj-causative-su+) "v5s" "v1"))
                                             :conj-types *secondary-conjugation-types*)
          if (zerop (mod cnt 1000)) do (format t "~a entries processed~%" cnt))))

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
            (return-from set-reading))
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
