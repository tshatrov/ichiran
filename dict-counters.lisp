(in-package :ichiran/dict)

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
   (accepts-suffixes :reader counter-suffix-accepts :initarg :accepts :initform nil
                     :documentation "which suffixes this counter accepts")
   (suffix-descriptions :reader counter-suffix-descriptions :initarg :suffix-descriptions :initform nil)
   (digit-opts :reader digit-opts :initform nil :initarg :digit-opts
               :documentation "alist of form (digit opt1 opt2 ...)
    where opt can be :g (geminate) :d (dakuten) :h (handakuten) (only one of :d and :h is meaningful)
    These options are used in counter-join, if digit is present then the usual rules are ignored.
    Empty options are equivalent to simple concatenation.")
   (common :reader counter-common :initform nil :initarg :common)
   (allowed :reader counter-allowed :initform nil :initarg :allowed)
   (foreign :reader counter-foreign :initform nil :initarg :foreign)
   ))

(defgeneric verify (counter unique)
  (:documentation "Verify if counter is valid")
  (:method (counter unique)
    (and (or (not (counter-allowed counter))
             (find (number-value counter) (counter-allowed counter)))
         unique)))

(defun ordinal-str (n)
  (let* ((digit (mod n 10))
         (teenp (< 10 (mod n 100) 20))
         (suffix (if teenp "th" (case digit (1 "st") (2 "nd") (3 "rd") (t "th")))))
    (format nil "~a~a" n suffix)))

(defgeneric value-string (counter)
  (:documentation "Value to be presented as string")
  (:method ((counter counter-text) &aux (value (number-value counter)))
    (format nil "Value: ~a~{ ~a~}"
            (if (ordinalp counter) (ordinal-str value) value)
            (reverse (counter-suffix-descriptions counter)))))

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
  (or (counter-common obj) (if (source obj) (common (source obj)) 0)))

(defmethod seq ((obj counter-text))
  (and (source obj) (seq (source obj))))

(defmethod ord ((obj counter-text))
  (if (source obj) (ord (source obj)) 0))

(defmethod word-conjugations ((obj counter-text)) nil)

(defmethod word-conj-data ((obj counter-text)) nil)

(defmethod nokanji ((obj counter-text))
  (and (source obj) (nokanji (source obj))))

(defmethod root-p ((obj counter-text)) t)

(defun get-digit (n)
  (let ((digit (mod n 10)))
    (if (zerop digit)
        (loop for (p pn) on '(10 100 1000 10000 #10r100000000)
           when (and pn (not (zerop (mod n pn)))) do (return p))
        digit)))

(defmethod counter-join ((obj counter-text) n number-kana counter-kana
                         &aux (digit (get-digit n))
                           (head (gethash (char counter-kana 0) *char-class-hash*))
                           (digit-opts (assoc digit (digit-opts obj)))
                           (off (assoc :off (digit-opts obj))))
  (when (or off digit-opts)
    (loop with mod-counter
       for opt in (cdr digit-opts)
       if (stringp opt) do
         (if mod-counter
             (setf counter-kana opt)
             (let ((stem (length (if (< digit 10)
                                     (getf ichiran/numbers::*digit-to-kana* digit)
                                     (getf ichiran/numbers::*power-to-kana* (round (log digit 10)))))))
               (setf number-kana
                     (concatenate 'string (subseq number-kana 0 (- (length number-kana) stem)) opt))))
       else do
         (case opt
           (:g (geminate number-kana))
           (:r (rendaku counter-kana))
           (:h (rendaku counter-kana :handakuten t))
           (:c (setf mod-counter t))))
    (return-from counter-join (call-next-method obj n number-kana counter-kana)))

  (when (counter-foreign obj)
    (case digit
      (6 (case head
           ((:ka :ki :ku :ke :ko
             :pa :pi :pu :pe :po)
            (geminate number-kana))))
      (8 (case head
           ((:ka :ki :ku :ke :ko
             :sa :shi :su :se :so
             :ta :chi :tsu :te :to
             :pa :pi :pu :pe :po)
            (geminate number-kana))))
      (10 (case head
           ((:ka :ki :ku :ke :ko
             :sa :shi :su :se :so
             :ta :chi :tsu :te :to
             :pa :pi :pu :pe :po)
            (geminate number-kana))))
      (100 (case head
             ((:ka :ki :ku :ke :ko)
              (geminate number-kana)))))
    (return-from counter-join (call-next-method)))

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
    (4 #-(and)(case head
                ((:ha :hi :fu :he :ho)
                 (rendaku counter-kana :handakuten t))))
    (6 (case head
         ((:ka :ki :ku :ke :ko
           :pa :pi :pu :pe :po)
          (geminate number-kana))
         ((:ha :hi :fu :he :ho)
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
         ((:ha :hi :fu :he :ho)
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

(defparameter *counter-suffixes* '((:kan "間" "かん" "[duration]")
                                   (:chuu "中" "ちゅう" "[among/out of ...]")))

(defparameter *counter-accepts* '((1194480 :kan) (1490430 :kan) (1333450 :kan)))

(defparameter *counter-foreign* '(1120410))

(defcache :counters *counter-cache*
  (let ((counter-cache (make-hash-table :test 'equal)))
    (labels ((add-args* (text args)
               (push args (gethash text counter-cache nil))
               (loop for suf in (getf (cdr args) :accepts)
                  for (suf-text suf-kana suf-desc) = (cdr (assoc suf *counter-suffixes*))
                  for (cls . new-args) = (copy-list args)
                  for new-text = (concatenate 'string text suf-text)
                  do (setf (getf new-args :text) new-text
                           (getf new-args :suffix) (format nil "~@[~a~]~a" (getf new-args :suffix) suf-kana))
                    (push suf-desc (getf new-args :suffix-descriptions))
                    (push (cons cls new-args) (gethash new-text counter-cache nil))))
             (add-args (text &rest args)
               (if (listp text)
                   (loop for txt in text
                      for new-args = (copy-list args)
                      do (setf (getf (cdr new-args) :text) txt
                               (getf (cdr new-args) :source) (funcall (getf (cdr new-args) :source) txt))
                         (add-args* txt new-args))
                   (add-args* text args))))
      (add-args "" 'number-text)
      (let ((readings-hash (get-counter-readings)))
        (loop for seq being each hash-key of readings-hash using (hash-value readings)
           for (kanji . kana) = readings
           for special = (gethash seq *special-counters*)
           if special do (mapcar (lambda (args) (apply #'add-args args)) (funcall special (append kanji kana)))
           else do (loop with foreign = (or (not kanji) (find seq *counter-foreign*))
                      for kt in (if foreign
                                    (append kanji (remove-if-not (lambda (x) (test-word (text x) :katakana)) kana))
                                    kanji)
                      for text = (text kt)
                      do (add-args text 'counter-text
                                   :text text :kana (text (car kana)) :source kt
                                   :ordinalp (and (> (length text) 1) (alexandria:ends-with #\目 text))
                                   :accepts (cdr (assoc (seq kt) *counter-accepts*))
                                   :foreign foreign))))
      (loop for counter in (alexandria:hash-table-keys counter-cache)
         for cord = (concatenate 'string counter "目")
         unless (or (alexandria:emptyp counter)
                    (and (> (length counter) 1) (alexandria:ends-with #\目 counter))
                    (gethash cord counter-cache))
         do (loop for old-args in (gethash counter counter-cache)
               for (cls . args) = (copy-list old-args)
               unless (getf args :ordinalp)
               do
                 (setf (getf args :text) cord
                       (getf args :suffix) (format nil "~@[~a~]め" (getf args :suffix))
                       (getf args :ordinalp) t)
                 (apply #'add-args cord cls args)))
      )
    counter-cache))

(defun find-counter (number counter &key (unique t))
  (let ((counter-args (gethash counter (ensure :counters))))
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

(defparameter *extra-counter-ids*
  '(1255430 ;; 月
    1606800 ;; 割
    ))

(defparameter *skip-counter-ids*
  '(2426510 ;; 一個当り
    2220370 ;; 歳 （とせ）
    2248360 ;; 入 （しお）
    2423450 ;; 差し
    2671670 ;; 幅 （の）
    2735690 ;; 種 （くさ）
    2838543 ;; 杯 （はた）

    ;; mahjong stuff - need some research on how to say these
    2249290 ;; 荘
    2833260 ;; 翻
    2833465 ;; 萬
    2833466 ;; 索
    2833467 ;; 筒
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
                             ,keys-var))
                    (args-suffix (,class-var ,text-var ,kana-var &rest ,keys-var &key &allow-other-keys)
                      (apply 'list (apply 'concatenate 'string ,text-var) ,class-var
                             :text (apply 'concatenate 'string ,text-var)
                             :kana (car ,kana-var) :suffix (cadr ,kana-var)
                             :source (find (car ,text-var) ,readings-var :key 'text :test 'equal)
                             ,keys-var))
                    )
               (list ,@body))))))

(def-special-counter 1203020 ()
  (args 'counter-text "階" "かい" :digit-opts '((3 :r))))

(def-special-counter 2020680 ()
  (args 'counter-text "時" "じ" :digit-opts '((4 "よ") (7 "しち") (9 "く"))))

(def-special-counter 1315920 ()
  (args 'counter-text "時間" "じかん" :digit-opts '((4 "よ") (9 "く"))))

(defclass counter-halfhour (counter-text) ())

(defmethod value-string ((counter counter-halfhour))
  (format nil "~a:30" (number-value counter)))

(def-special-counter 1658480 ()
  (args 'counter-halfhour "時半" "じはん" :digit-opts '((4 "よ") (9 "く"))))

(def-special-counter 1356740 ()
  (args 'counter-text "畳" "じょう" :digit-opts '((4 "よ") (7 "しち"))))

(def-special-counter 2258110 ()
  (args 'counter-text "帖" "じょう" :digit-opts '((4 "よ") (7 "しち"))))

(def-special-counter 1396490 ()
  (args 'counter-text "膳" "ぜん" :digit-opts '((4 "よ") (7 "しち"))))

(def-special-counter 1427240 ()
  (args 'counter-text "丁" "ちょう")
  (args-suffix 'counter-text '("丁" "目") '("ちょう" "め") :ordinalp t))

(def-special-counter 1427420 ()
  (args 'counter-text "丁目" "ちょうめ" :ordinalp t))

(def-special-counter 1514050 ()
  (args 'counter-text "舗" "ほ" :digit-opts '((4 :h))))

(def-special-counter 1522150 ()
  (args 'counter-text "本" "ほん" :digit-opts '((3 :r))))

(def-special-counter 1583370 ()
  (args 'counter-text '("匹" "疋") "ひき" :digit-opts '((3 :r))))

(def-special-counter 1607310 ()
  (args 'counter-text "羽" "わ" :digit-opts '((3 :c "ば") (6 :g :c "ぱ") (10 :g :c "ぱ")
                                              (100 :g :c "ぱ") (1000 :c "ば") (10000 :c "ば"))))

(def-special-counter 1607320 ()
  (args 'counter-text "把" "わ" :digit-opts '((3 :c "ば") (7 "しち") (10 :g :c "ぱ"))))

(def-special-counter 1633690 ()
  (args 'counter-text "段" "だん" :digit-opts '((7 "しち"))))

(def-special-counter 1901390 ()
  (args 'counter-text "敗" "はい" :digit-opts '((4 :h))))

(def-special-counter 1919550 ()
  (args 'counter-text "泊" "はく" :digit-opts '((4 :h))))

(def-special-counter 1994890 ()
  (args 'counter-text "首" "しゅ" :digit-opts '((10))))

(def-special-counter 1351270 ()
  (args 'counter-text "章" "しょう" :digit-opts '((10))))

(def-special-counter 2019640 ()
  (args 'counter-text '("杯" "盃") "はい" :digit-opts '((3 :r))))

(def-special-counter 2078550 ()
  (args 'counter-text "条" "じょう" :digit-opts '((7 "しち"))))

(def-special-counter 2078590 ()
  (args 'counter-text "軒" "けん" :digit-opts '((3 :r))))

(def-special-counter 2081610 ()
  (args 'counter-text '("立て" "たて" "タテ") "たて" :digit-opts '((:off))))

(def-special-counter 2084840 ()
  (args 'counter-text "年" "ねん" :digit-opts '((4 "よ") (7 "しち") (9 "く")) :accepts '(:kan)))

(def-special-counter 1468900 ()
  (args 'counter-text "年生" "ねんせい" :digit-opts '((4 "よ") (7 "しち") (9 "く"))))

(def-special-counter 1502840 ()
  (args 'counter-text "分" "ふん" :digit-opts '((4 :h))))

(def-special-counter 2386360 ()
  (args 'counter-text "分間" "ふんかん" :digit-opts '((4 :h))))

(def-special-counter 1373990 ()
  (args 'counter-text "世紀" "せいき" :digit-opts '((10 "じっ"))))

(def-special-counter 2836694 ()
  (args 'counter-text "傑" "けつ" :digit-opts '((10 "じっ"))))

(def-special-counter 2208060 ()
  (args 'counter-text "遍" "へん" :digit-opts '((3 :r))))

(def-special-counter 1511870 ()
  (args 'counter-text '("編" "篇") "へん" :digit-opts '((3 :r))))

(def-special-counter 2271620 ()
  (args 'counter-text "口" "こう"))

(def-special-counter 2412230 ()
  (args 'counter-text "足" "そく" :digit-opts '((3 :r))))

(def-special-counter 1175570 ()
  (args 'counter-text "円" "えん" :digit-opts '((4 "よ"))))

(def-special-counter 1315130 ()
  (args 'counter-text "字" "じ" :digit-opts '((4 "よ"))))

(def-special-counter 1487770 ()
  (args 'counter-text "筆" "ひつ" :digit-opts '((4 :h))))

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
  (args 'counter-hifumi '("竿" "棹") "さお" :digit-set '(1 2 3 4 5) :digit-opts '((4 "よ") (10))))

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

(def-special-counter 1379650 ()
  (args 'counter-hifumi '("盛り" "盛") "もり" :digit-set '(1 2)))

(def-special-counter 1383800 ()
  (args 'counter-hifumi '("切り" "限り" "限") "きり" :digit-set '(1 2 3) :digit-opts '((4 "よ") (8))))

(def-special-counter 1384840 ()
  (args 'counter-hifumi "切れ" "きれ" :digit-set '(1 2 3) :digit-opts '((4 "よ") (8))))

(def-special-counter 1385780 ()
  (args 'counter-hifumi "折" "おり" :digit-set '(1 2)))

(def-special-counter 1404450 ()
  (args 'counter-hifumi "束" "たば" :digit-set '(1 2)))

(def-special-counter 1426480 ()
  (args 'counter-hifumi "柱" "はしら" :digit-set '(1 2) :digit-opts '((:off))))

(def-special-counter 1432920 ()
  (args 'counter-hifumi "通り" "とおり" :digit-set '(1 2) :digit-opts '((100 :g))))

(def-special-counter 1445150 ()
  (args 'counter-hifumi "度" "たび" :digit-set '(1 2) :digit-opts '((:off)) :common :null))

(def-special-counter 1448350 ()
  (args 'counter-hifumi "棟" "むね" :digit-set '(1 2)))

(def-special-counter 1335730 ()
  (let ((digit-set '(1 2 3 5 7 8 9 10)))
    (args 'counter-hifumi "重" "え" :digit-set digit-set :allowed digit-set)))

(def-special-counter 2108240 ()
  (args 'counter-text "重" "じゅう" :digit-opts '((4 "し") (7 "しち") (9 "く"))))

(def-special-counter 1482110 ()
  (args 'counter-hifumi "晩" "ばん" :digit-set '(1 2 3) :digit-opts '((4 "よ"))))

(def-special-counter 1501110 ()
  (args 'counter-hifumi '("腹" "肚") "はら" :digit-set '(1 2) :digit-opts '((:off))))

(def-special-counter 1397450 ()
  (args 'counter-hifumi "組" "くみ" :digit-set '(1 2 3) :allowed '(1 2 3)
        :suffix-descriptions '("(sets or pairs only)"))
  (args 'counter-text "組" "くみ" :digit-opts '((1))))

(def-special-counter 1519300 ()
  (args 'counter-hifumi '("房" "総") "ふさ" :digit-set '(1 2) :digit-opts '((:off))))

(def-special-counter 1552890 ()
  (args 'counter-hifumi "粒" "つぶ" :digit-set '(1 2 3) :digit-opts '((6 :g))))

(def-special-counter 1564410 ()
  (args 'counter-hifumi "一刎" "はね" :digit-set '(1 2 3) :digit-opts '((:off))))

(def-special-counter 1585650 ()
  (args 'counter-hifumi '("箱" "函" "匣" "筥" "筐" "凾") "はこ" :digit-set '(1 2)
        :digit-opts '((4 "よ") (1000) (10000))))

(def-special-counter 1602800 () ;; uncertain, 三舟 can be さんしゅう in some context
  (args 'counter-hifumi '("船" "舟") "ふね" :digit-set '(1 2 3) :digit-opts '((:off))))

(def-special-counter 1853450 () ;; uncertain
  (args 'counter-hifumi '("締め" "〆") "しめ" :digit-set '(1 2)))

(def-special-counter 1215240 ()
  (args 'counter-hifumi "間" "ま" :digit-set '(1 2 3 4 9) :digit-opts '((4 "よ"))))

(def-special-counter 2243700 ()
  (args 'counter-hifumi "咫" "あた" :digit-set '(1 2 3)))

(def-special-counter 2414730 ()
  (args 'counter-hifumi "梱" "こり" :digit-set '(1 2)))

(def-special-counter 1583470 ()
  (args 'counter-hifumi "品" "しな" :digit-set '(1 2 3) :digit-opts '((4 "よ"))))

(def-special-counter 1411070 ()
  (args 'counter-hifumi "袋" "ふくろ" :digit-set '(1 2 3) :digit-opts '((4 "よ") (10 "じっ" :h))))

(def-special-counter 2707020 ()
  (args 'counter-text "袋" "たい" :digit-opts '((10 "じっ"))))

(def-special-counter 2800530 ()
  (args 'counter-hifumi '("回り" "廻り") "まわり" :digit-set '(1 2)))

(def-special-counter 1047880 ()
  (args 'counter-hifumi "ケース" "ケース" :digit-set '(1 2) :foreign t))

(def-special-counter 1214540 ()
  (args 'counter-hifumi "缶" "かん" :digit-set '(1 2)))

(def-special-counter 1575510 ()
  (args 'counter-hifumi '("齣" "コマ") "こま" :digit-set '(1 2)))

(def-special-counter 1253800 ()
  (args 'counter-hifumi "桁" "けた" :digit-set '(1 2 3)))

(def-special-counter 1241750 ()
  (args 'counter-hifumi "筋" "すじ" :digit-set '(1 2 3)))

(def-special-counter 1515340 ()
  (args 'counter-hifumi "包み" "つつみ" :digit-set '(1 2 3)))

(def-special-counter 2452360 ()
  (args 'counter-hifumi "片" "ひら" :digit-set '(1 2 3)))

(def-special-counter 2844070 ()
  (args 'counter-hifumi "腰" "こし" :digit-set '(1 2 3)))

(def-special-counter 2844196 ()
  (args 'counter-hifumi "緡" "さし" :digit-set '(1 2 3)))

(def-special-counter 1175140 ()
  (args 'counter-hifumi "駅" "えき" :digit-set '(1 2)))

(defclass counter-days-kun (counter-text)
  ((allowed :initform '(1 2 3 4 5 6 7 8 9 10 14 20 24 30))))

(defmethod get-kana ((obj counter-days-kun))
  (case (number-value obj)
    (1 "ついたち")
    (2 "ふつか")
    (3 "みっか")
    (4 "よっか")
    (5 "いつか")
    (6 "むいか")
    (7 "なのか")
    (8 "ようか")
    (9 "ここのか")
    (10 "とうか")
    (14 "じゅうよっか")
    (20 "はつか")
    (24 "にじゅうよっか")
    (30 "みそか")))

(def-special-counter 2083110 ()
  (args 'counter-days-kun "日" "か" :common 0 :accepts '(:kan)))

(defclass counter-days-on (counter-text) ())

(defmethod verify ((counter counter-days-on) unique)
  (let ((n (number-value counter)))
    (and (or (> n 10)
             (= n 1))
         (/= n 20)
         (call-next-method))))

(def-special-counter 2083100 ()
  (args 'counter-days-on "日" "にち"))

(defclass counter-months (counter-text)
  ((allowed :initform '(1 2 3 4 5 6 7 8 9 10 11 12))
   (digit-opts :initform '((4 "し") (7 "しち") (9 "く")))))

(defmethod value-string ((counter counter-months))
  (aref #("January" "February" "March"
          "April" "May" "June"
          "July" "August" "September"
          "October" "November" "December")
        (1- (number-value counter))))

(def-special-counter 1255430 ()
  (args 'counter-months "月" "がつ"))

(defclass counter-people (counter-text) ())

(defmethod get-kana ((obj counter-people))
  (case (number-value obj)
    (1 "ひとり")
    (2 "ふたり")
    (t (call-next-method))))

(def-special-counter 2149890 ()
  (args 'counter-people "人" "にん" :digit-opts '((4 "よ") (7 "しち")) :accepts '(:chuu)))

(defclass counter-wari (counter-text) ())

(defmethod value-string ((counter counter-wari))
  (format nil "~a%" (* 10 (number-value counter))))

(def-special-counter 1606800 ()
  (args 'counter-wari "割" "わり"))

(def-special-counter 1606950 ()
  (args 'counter-wari "割引" "わりびき"))

(defclass counter-age (counter-text) ())

(defmethod get-kana ((obj counter-age))
  (case (number-value obj)
    (20 "はたち")
    (t (call-next-method))))

(def-special-counter 1294940 ()
  (args 'counter-age '("歳" "才") "さい"))
