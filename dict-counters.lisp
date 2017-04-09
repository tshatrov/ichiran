(in-package :ichiran/dict)

(def-conn-var *counter-cache* nil)

(defgeneric counter-join (counter n number-kana counter-kana)
  (:documentation "Construct counter kana text")
  (:method (counter n number-kana counter-kana)
    (declare (ignore counter n))
    (concatenate 'string number-kana counter-kana)))

(defclass counter-text ()
  ((text :reader counter-text :initarg :text)
   (kana :reader counter-kana :initarg :kana)
   (number-text :reader number-text :initarg :number-text)
   (number :reader number-value)
   (source :reader source :initform nil :initarg :source)
   (ordinalp :reader ordinalp :initform nil :initarg :ordinalp)
   (suffix :reader counter-suffix :initarg :suffix :initform nil
           :documentation "Kana suffix (i.e. め for 目)")
   (digit-opts :reader digit-opts :initform nil :initarg :digit-opts
               :documentation "alist of form (digit opt1 opt2 ...)
    where opt can be :g (geminate) :d (dakuten) :h (handakuten) (only one of :d and :h is meaningful)
    These options are used in counter-join, if digit is present then the usual rules are ignored.
    Empty options are equivalent to simple concatenation.")
   ))

(defgeneric verify (counter unique)
  (:documentation "Verify if counter is valid")
  (:method (counter unique)
    (declare (ignore counter))
    unique))

(defun ordinal-str (n)
  (let* ((digit (mod n 10))
         (teenp (< 10 (mod n 100) 20))
         (suffix (if teenp "th" (case digit (1 "st") (2 "nd") (3 "rd") (t "th")))))
    (format nil "~a~a" n suffix)))

(defgeneric value-string (counter)
  (:documentation "Value to be presented as string")
  (:method ((counter counter-text) &aux (value (number-value counter)))
    (format nil "Value: ~a" (if (ordinalp counter) (ordinal-str value) value))))

(defmethod initialize-instance :after ((obj counter-text) &key)
  (setf (slot-value obj 'number) (parse-number (number-text obj))))

(defmethod print-object ((obj counter-text) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a[~a] ~a" (number-text obj) (number-value obj) (counter-text obj))))

(defmethod text ((obj counter-text))
  (concatenate 'string (number-text obj) (counter-text obj)))

(defmethod get-kanji ((obj counter-text))
  (concatenate 'string (number-to-kanji (number-value obj)) (counter-text obj)))

(defmethod get-kana ((obj counter-text))
  (let ((n (number-value obj)))
    (counter-join obj n (number-to-kana n :separator *kana-hint-space*)
                  (copy-seq (counter-kana obj)))))

(defmethod get-kana :around ((obj counter-text))
  (let ((kana (call-next-method)))
    (if (counter-suffix obj) (concatenate 'string kana (counter-suffix obj)) kana)))

(defmethod word-type ((obj counter-text))
  (if (> (count-char-class (text obj) :kanji-char) 0) :kanji :kana))

(defmethod common ((obj counter-text))
  (if (source obj) (common (source obj)) 0))

(defmethod seq ((obj counter-text))
  (and (source obj) (seq (source obj))))

(defmethod ord ((obj counter-text))
  (if (source obj) (ord (source obj)) 0))

(defmethod word-conjugations ((obj counter-text)) nil)

(defmethod word-conj-data ((obj counter-text)) nil)

(defmethod root-p ((obj counter-text)) t)

(defun get-digit (n)
  (let ((digit (mod n 10)))
    (if (zerop digit)
        (loop for (p pn . rest) on '(10 100 1000 10000 100000000)
           when (and pn (not (zerop (mod n pn)))) do (return p))
        digit)))

(defmethod counter-join ((obj counter-text) n number-kana counter-kana
                         &aux (digit (get-digit n))
                           (head (gethash (char counter-kana 0) *char-class-hash*))
                           (digit-opts (assoc digit (digit-opts obj)))
                           (off (assoc :off (digit-opts obj))))
  (when (or off digit-opts)
    (loop for opt in (cdr digit-opts)
       if (stringp opt)
       do (let ((stem (length (getf ichiran/numbers::*digit-to-kana* digit))))
            (setf number-kana
                  (concatenate 'string (subseq number-kana 0 (- (length number-kana) stem)) opt)))
       else do
         (case opt
           (:g (geminate number-kana))
           (:r (rendaku counter-kana))
           (:h (rendaku counter-kana :handakuten t))))
    (return-from counter-join (call-next-method obj n number-kana counter-kana)))
  
  (case digit
    (1 (case head
         ((:ka :ki :ku :ke :ko
           :sa :shi :su :se :so
           :ta :chi :tsu :te :to)
          (geminate number-kana))
         ((:ha :hi :fu :he :ho)
          (geminate number-kana)
          (rendaku counter-kana :handakuten t))))
    (3 (case head
         ((:ha :hi :fu :he :ho)
          (rendaku counter-kana :handakuten t))))
    (4 (case head
         ((:ha :hi :fu :he :ho)
          (rendaku counter-kana :handakuten t))))
    (6 (case head
         ((:ka :ki :ku :ke :ko)
          (geminate number-kana))
         ((:ha :hi :fu :he :ho
           :pa :pi :pu :pe :po)
          (geminate number-kana)
          (rendaku counter-kana :handakuten t))))
    (8 (case head
         ((:ka :ki :ku :ke :ko
           :sa :shi :su :se :so
           :ta :chi :tsu :te :to
           :pa :pi :pu :pe :po)
          (geminate number-kana))
         ((:ha :hi :fu :he :ho)
          (geminate number-kana)
          (rendaku counter-kana :handakuten t))))
    (10 (case head
           ((:ka :ki :ku :ke :ko
             :sa :shi :su :se :so
             :ta :chi :tsu :te :to
             :pa :pi :pu :pe :po)
            (geminate number-kana))
           ((:ha :hi :fu :he :ho)
            (geminate number-kana)
            (rendaku counter-kana :handakuten t))))
    (100 (case head
         ((:ka :ki :ku :ke :ko)
          (geminate number-kana))
         ((:ha :hi :fu :he :ho
           :pa :pi :pu :pe :po)
          (geminate number-kana)
          (rendaku counter-kana :handakuten t))))
    (1000 (case head
            ((:ha :hi :fu :he :ho)
             (rendaku counter-kana :handakuten t))))
    (10000 (case head
             ((:ha :hi :fu :he :ho)
              (rendaku counter-kana :handakuten t))))
    )
  (call-next-method))

(defclass number-text (counter-text)
  ((text :initform "")
   (kana :initform "")
   (ordinalp :initform nil)))

(defmethod get-kana ((obj number-text))
  (number-to-kana (number-value obj) :separator *kana-hint-space*))

(defparameter *special-counters* (make-hash-table))

(defun init-counters (&optional reset)
  (when (or reset (not *counter-cache*))
    (setf *counter-cache* (make-hash-table :test 'equal))
    (labels ((add-args (text &rest args)
               (if (listp text)
                   (loop for txt in text
                      for new-args = (copy-list args)
                      do (setf (getf (cdr new-args) :text) txt
                               (getf (cdr new-args) :source) (funcall (getf (cdr new-args) :source) txt))
                         (push new-args (gethash txt *counter-cache* nil)))
                   (push args (gethash text *counter-cache* nil)))))
      (add-args "" 'number-text)
      (let ((readings-hash (get-counter-readings)))
        (loop for seq being each hash-key of readings-hash using (hash-value readings)
           for (kanji . kana) = readings
           for special = (gethash seq *special-counters*)
           if special do (mapcar (lambda (args) (apply #'add-args args)) (funcall special (append kanji kana)))
           else do (loop for kt in kanji
                      for text = (text kt)
                      do (add-args text 'counter-text
                                   :text text :kana (text (car kana)) :source kt
                                   :ordinalp (alexandria:ends-with #\目 text)))))
      (loop for counter in (alexandria:hash-table-keys *counter-cache*)
         for cord = (concatenate 'string counter "目")
         unless (or (alexandria:emptyp counter)
                    (alexandria:ends-with #\目 counter)
                    (gethash cord *counter-cache*))
         do (loop for old-args in (gethash counter *counter-cache*)
               for (cls . args) = (copy-list old-args)
               unless (getf args :ordinalp)
               do
                 (setf (getf args :text) cord
                       (getf args :suffix) (format nil "~@[~a~]め" (getf args :suffix))
                       (getf args :ordinalp) t)
                 (apply #'add-args cord cls args)))
      )))

(defun find-counter (number counter &key (unique t))
  (init-counters)
  (let ((counter-args (gethash counter *counter-cache*)))
    (when counter-args
      (loop for args in counter-args
         for counter-obj = (handler-case
                               (apply 'make-instance `(,@args :number-text ,number))
                             (not-a-number () nil))
         when (and counter-obj (verify counter-obj unique))
           collect counter-obj))))

(defun get-counter-ids ()
  (sort
   (query (:select 'seq :distinct
                   :from 'sense-prop
                   :where (:and (:= 'tag "pos") (:= 'text "ctr")))
          :column)
   '<))

(defun get-counter-stags (seqs)
  (let ((stagks (make-hash-table))
        (stagrs (make-hash-table)))
    (flet ((q (tag)
             (query (:select 'sp.seq 'sp.text
                             :from (:as 'sense-prop 'sp) (:as 'sense-prop 'sp1)
                             :where (:and (:= 'sp.seq 'sp1.seq)
                                          (:= 'sp.sense-id 'sp1.sense-id)
                                          (:= 'sp.tag tag)
                                          (:= 'sp1.tag "pos")
                                          (:= 'sp1.text "ctr")
                                          (:in 'sp.seq (:set seqs))
                                          )))))
      (loop for (seq text) in (q "stagk")
         do (push text (gethash seq stagks nil)))
      (loop for (seq text) in (q "stagr")
         do (push text (gethash seq stagrs nil)))
      (cons stagks stagrs))))

(defparameter *extra-counter-ids* '())

(defparameter *skip-counter-ids*
  '(2426510 ;; 一個当り
    1241750 ;; 筋 条
    ))

(defun get-counter-readings ()
  (with-connection *connection*
    (let* ((hash (make-hash-table))
           (counter-ids (set-difference
                         (nconc (get-counter-ids) *extra-counter-ids*)
                         *skip-counter-ids*))
           (stags (get-counter-stags counter-ids))
           (kanji-readings (select-dao 'kanji-text (:in 'seq (:set counter-ids))))
           (kana-readings (select-dao 'kana-text (:in 'seq (:set counter-ids)))))
      (loop for r in kanji-readings
         for val = (gethash (seq r) hash)
         for stagks = (gethash (seq r) (car stags))
         when (or (not stagks) (find (text r) stagks :test 'equal))
         do (unless val (setf val (cons nil nil) (gethash (seq r) hash) val))
           (push r (car val)))
      (loop for r in kana-readings
         for val = (gethash (seq r) hash)
         for stagrs = (gethash (seq r) (cdr stags))
         when (or (not stagrs) (find (text r) stagrs :test 'equal))
         do (unless val (setf val (cons nil nil) (gethash (seq r) hash) val))
           (push r (cdr val)))
      (maphash
       (lambda (key value)
         (setf (gethash key hash) (cons (sort (car value) '< :key 'ord)
                                        (sort (cdr value) '< :key 'ord))))
       hash)
      hash)))

(defmacro def-special-counter (seq (&optional readings-var) &body body)
  (alexandria:with-gensyms (class-var text-var kana-var keys-var)
    (unless readings-var (setf readings-var (gensym "RV")))
    `(setf (gethash ,seq *special-counters*)
           (lambda (,readings-var)
             (flet ((args (,class-var ,text-var ,kana-var &rest ,keys-var &key &allow-other-keys)
                      (apply 'list ,text-var ,class-var :text ,text-var :kana ,kana-var
                             :source (if (listp ,text-var)
                                         (lambda (,text-var)
                                           (find ,text-var ,readings-var :key 'text :test 'equal))
                                         (find ,text-var ,readings-var :key 'text :test 'equal))
                             ,keys-var)))
               (list ,@body))))))

(def-special-counter 1203020 ()
  (args 'counter-text "階" "かい" :digit-opts '((3 :r))))

(def-special-counter 1315920 ()
  (args 'counter-text "時間" "じかん" :digit-opts '((4 "よ") (9 "く"))))

(def-special-counter 1356740 ()
  (args 'counter-text "畳" "じょう" :digit-opts '((4 "よ"))))

(defclass counter-tsu (counter-text) ())

(defmethod verify ((counter counter-tsu) unique)
  (and (<= 1 (number-value counter) 9) unique))

(defmethod get-kana ((obj counter-tsu))
  (case (number-value obj)
    (1 "ひとつ")
    (2 "ふたつ")
    (3 "みっつ")
    (4 "よっつ")
    (5 "いつつ")
    (6 "むっつ")
    (7 "ななつ")
    (8 "やっつ")
    (9 "ここのつ")
    (t (call-next-method))))

(def-special-counter 2220330 ()
  (args 'counter-tsu "つ" "つ"))

(defclass counter-hifumi (counter-text)
  ((digit-set :reader digit-set :initarg :digit-set)))

(defmethod get-kana ((obj counter-hifumi) &aux (value (number-value obj)))
  (cond
    ((find value (digit-set obj))
     (concatenate 'string
                  (case value
                    (1 "ひと")
                    (2 "ふた")
                    (3 "み")
                    (4 "よ")
                    (5 "いつ")
                    (6 "む")
                    (7 "なな")
                    (8 "や")
                    (9 "ここの")
                    (10 "と")
                    )
                  (counter-kana obj)))
    (t (call-next-method))))

(def-special-counter 1208920 ()
  (args 'counter-hifumi "株" "かぶ" :digit-set '(1 2)))

(def-special-counter 1214060 ()
  (args 'counter-hifumi '("竿" "棹") "さお" :digit-set '(1 2 3 4 5)))

(def-special-counter 1260670 ()  ;; uncertain
  (args 'counter-hifumi "本" "もと" :digit-set '(1 2 3)))

(def-special-counter 1275640 ()
  (args 'counter-hifumi "口" "くち" :digit-set '(1 2 3)))

(def-special-counter 1299680 ()
  (args 'counter-hifumi '("皿" "盤") "さら" :digit-set '(1 2 3)))

(def-special-counter 1302680 () ;; uncertain
  (args 'counter-hifumi "山" "やま" :digit-set '(1 2 3)))

(def-special-counter 1335810 () ;; uncertain
  (args 'counter-hifumi '("重ね" "襲") "かさね" :digit-set '(1 2 3)))

(def-special-counter 1361130 () ;; uncertain
  (args 'counter-hifumi '("振り" "風") "ふり" :digit-set '(1 2) :digit-opts '((:off))))

(def-special-counter 1366210 ()
  (args 'counter-hifumi '("針" "鉤" "鈎") "はり" :digit-set '(1 2) :digit-opts '((:off))))

(defclass counter-e (counter-hifumi)
  ((digit-set :initform '(1 2 3 5 7 8 9 10))))

(defmethod verify ((counter counter-e) unique)
  (and (find (number-value counter) (digit-set counter)) unique))

(def-special-counter 1335730 ()
  (args 'counter-e "重" "え"))

(def-special-counter 2108240 ()
  (args 'counter-text "重" "じゅう" :digit-opts '((4 "し") (7 "しち") (9 "く"))))

