(in-package :ichiran/dict)

;;; SUFFIXES (makes compound word primary + suffix)

(def-conn-var *suffix-cache* nil)
(def-conn-var *suffix-class* nil) ;; seq -> class

(defun init-suffix-hashtables ()
  (setf *suffix-cache* (make-hash-table :test 'equal)
        *suffix-class* (make-hash-table :test 'eql)))

(defun get-kana-forms-conj-data-filter (conj-data)
  (unless (skip-by-conj-data conj-data)
    (loop for cd in conj-data
       for prop = (conj-data-prop cd)
       unless (test-conj-prop prop *weak-conj-forms*)
       collect (conj-id prop))))

(defun get-kana-forms* (seq)
  (loop for kt in
       (query-dao 'kana-text
                  (:union
                   (:select 'kt.* :from (:as 'kana-text 'kt) :where (:= 'kt.seq seq))
                   (:select 'kt.* :from (:as 'kana-text 'kt)
                            :left-join (:as 'conjugation 'conj) :on (:= 'conj.seq 'kt.seq)
                            :where (:= 'conj.from seq))))
       if (= (seq kt) seq)
       do (setf (word-conjugations kt) :root) and collect kt
       else if (let ((conj-ids (get-kana-forms-conj-data-filter (get-conj-data (seq kt) seq))))
                 (when conj-ids
                   (setf (word-conjugations kt) conj-ids)))
            collect kt))

(defun get-kana-forms (seq)
  (or (get-kana-forms* seq)
      (warn "No kana forms found for: ~a" seq)))

(defun get-kana-form (seq text &key conj)
  (let ((res (car (select-dao 'kana-text (:and (:= 'text text) (:= 'seq seq))))))
    (when (and res conj)
      (setf (word-conjugations res) conj))
    res))

(defun find-word-with-conj-prop (wordstr filter-fn &key allow-root)
  (loop for word in (find-word-full wordstr)
       for conj-data = (word-conj-data word)
       for conj-data-filtered = (remove-if-not filter-fn conj-data)
       for conj-ids = (mapcar (lambda (cdata) (conj-id (conj-data-prop cdata))) conj-data-filtered)
       when (or conj-data-filtered (and (null conj-data) allow-root))
       do (setf (word-conjugations word) conj-ids)
       and collect word))

(defun find-word-with-conj-type (word &rest conj-types)
  (find-word-with-conj-prop word
                            (lambda (cdata)
                              (member (conj-type (conj-data-prop cdata)) conj-types))))

(defun pair-words-by-conj (&rest word-groups)
  (flet ((key (word)
           (sort (mapcar (lambda (conj-id)
                           (let ((conj (get-dao 'conjugation conj-id)))
                             (list (seq-from conj) (let ((via (seq-via conj))) (if (eql via :null) 0 via)))))
                         (word-conjugations word))
                 (lex-compare '<))))
    (loop with bag = (make-hash-table :test 'equal)
       for wg in word-groups
       for idx from 0
       do (loop for word in wg
             for key = (key word)
             for arr = (or (gethash key bag) (loop for i below (length word-groups) collect nil))
             do (setf (elt arr idx) word
                      (gethash key bag) arr))
       finally (return (alexandria:hash-table-values bag)))))

(defun find-word-seq (word &rest seqs)
  (let ((table (if (test-word word :kana) 'kana-text 'kanji-text)))
    (select-dao table (:and (:= 'text word) (:in 'seq (:set seqs))))))

(defun find-word-conj-of (word &rest seqs)
  (union
   (apply #'find-word-seq word seqs)
   (let ((table (if (test-word word :kana) 'kana-text 'kanji-text)))
     (query-dao table (:select 'kt.* :from (:as table 'kt) (:as 'conjugation 'conj)
                               :where (:and (:= 'kt.seq 'conj.seq)
                                            (:in 'conj.from (:set seqs))
                                            (:= 'kt.text word)))))
   :key #'id))

(defun find-word-with-pos (word &rest posi)
  (let ((table (if (test-word word :kana) 'kana-text 'kanji-text)))
    (query-dao table (:select 'kt.* :distinct :from (:as table 'kt)
                              :inner-join (:as 'sense-prop 'sp) :on (:and (:= 'sp.seq 'kt.seq)
                                                                          (:= 'sp.tag "pos"))
                              :where (:and (:= 'kt.text word)
                                           (:in 'sp.text (:set posi)))))))

(defun or-as-hiragana (fn word &rest args)
  (let ((result (apply fn word args)))
    (or result
        (find-word-as-hiragana word :finder (lambda (w) (apply fn w args))))))

(defun find-word-with-suffix (wordstr &rest suffix-classes)
  (loop for word in (find-word-full wordstr)
     for seq = (seq word)
     for suffix-class = (and (listp seq) (gethash (car (last seq)) *suffix-class*))
     when (and suffix-class (find suffix-class suffix-classes)) collect word))

(hash-from-list
 *suffix-description*
 '(:chau "indicates completion (to finish ...)"
   :ha "topic marker particle"
   :tai "want to... / would like to..."
   :iru "indicates continuing action (to be ...ing)"
   :oru "indicates continuing action (to be ...ing) (humble)"
   :aru "indicates completion / finished action"
   :kuru "indicates action that had been continuing up till now / came to be "
   :oku "to do in advance / to leave in the current state expecting a later change"
   :kureru "(asking) to do something for one"
   :morau "(asking) to get somebody to do something"
   :itadaku "(asking) to get somebody to do something (polite)"
   :iku "is becoming / action starting now and continuing"
   :suru "makes a verb from a noun"
   :itasu "makes a verb from a noun (humble)"
   :sareru "makes a verb from a noun (honorific or passive)"
   :saseru "let/make someone/something do ..."
   :rou "probably / it seems that... / I guess ..."
   :ii "it's ok if ... / is it ok if ...?"
   :mo "even if ..."
   :sugiru "to be too (much) ..."
   :nikui "difficult to..."
   :sa "-ness (degree or condition of adjective)"
   :tsutsu "while ... / in the process of ..."
   :tsutsuaru "to be doing ... / to be in the process of doing ..."
   :uru "can ... / to be able to ..."
   :sou "looking like ... / seeming ..."
   :nai "negative suffix"
   :ra "pluralizing suffix (not polite)"
   :kudasai "please do ..."
   :yagaru "indicates disdain or contempt"
   :naru "to become ..."
   :desu "formal copula"
   :desho "it seems/perhaps/don't you think?"
   :tosuru "to try to .../to be about to..."
   :garu "to feel .../have a ... impression of someone"
   :me "somewhat/-ish"
   ;; these are used for splitsegs
   2826528 "polite prefix" ;; お
   2028980 "at / in / by" ;; で
   2028970 "or / questioning particle" ;; か
   2028990 "to / at / in" ;; に
   2029010 "indicates direct object of action" ;; を
   1469800 "indicates possessive (...'s)"
   2086960 "quoting particle"
   1002980 "from / because"
   ))

(defun get-suffix-description (seq)
  (gethash (or (gethash seq *suffix-class*) seq) *suffix-description*))

(defvar *init-suffixes-lock* (sb-thread:make-mutex :name "init-suffixes-lock"))

(defun init-suffixes-running-p ()
  (or (not *suffix-cache*)
      (sb-thread:mutex-value *init-suffixes-lock*)))

(defun init-suffixes-thread ()
  (sb-thread:with-mutex (*init-suffixes-lock* :wait-p nil)
    (with-connection *connection*
      (labels ((update-suffix-cache (text new &key join)
                 (let ((old (gethash text *suffix-cache*)))
                   (setf (gethash text *suffix-cache*)
                         (cond
                           ((not old) new)
                           ((and join (consp (car old)))
                            (cons new old))
                           (join
                            (list new old))
                           (t
                            ;;(format t "Overwriting ~a (was ~a, now ~a)~%" text old new)
                            new)))))
               (load-kf (key kf &key class text join)
                 (update-suffix-cache (or text (text kf)) (list key kf) :join join)
                 (setf (gethash (seq kf) *suffix-class*) (or class key)))
               (load-conjs (key seq &optional class join)
                 (loop for kf in (get-kana-forms seq)
                    do (load-kf key kf :class class :join join)))
               (load-abbr (key text &key join)
                 (update-suffix-cache text (list key nil) :join join)))

        (load-conjs :chau 2013800) ;; ちゃう
        (load-conjs :chau 2210750) ;; ちまう
        (load-kf :chau (get-kana-form 2028920 "は") :class :ha :text "ちゃ")
        (load-kf :chau (get-kana-form 2028920 "は") :class :ha :text "じゃ")

        (load-conjs :tai 2017560)
        (load-conjs :ren- 2772730 :nikui)

        (load-conjs :te 1577985 :oru) ;; おる

        (load-conjs :te 1296400 :aru) ;; ある

        (loop for kf in (get-kana-forms 1577980) ;; いる (る)
           for tkf = (text kf)
           do (setf (gethash tkf *suffix-cache*) (list (if (> (length tkf) 1) :te++ :te+) kf)
                    (gethash (seq kf) *suffix-class*) :iru)
             (when (> (length tkf) 1)
               (setf (gethash (subseq tkf 1) *suffix-cache*) (list :te+ kf))))

        (load-conjs :te 1547720 :kuru) ;; くる

        (load-conjs :te 1421850 :oku) ;; おく
        (load-conjs :to 2108590 :oku) ;; とく

        (load-conjs :te 1305380 :chau) ;; しまう

        (load-conjs :te+space 1269130 :kureru) ;; くれる
        (load-conjs :te+space 1535910 :morau) ;; もらう
        (load-conjs :te+space 1587290 :itadaku) ;; いただく

        (loop for kf in (get-kana-forms 1578850) ;;  いく / く
           for tkf = (text kf)
           for tkf-short = (subseq tkf 1)
           for val = (list :te kf)
           when (char= (char tkf 0) #\HIRAGANA_LETTER_I)
           do (setf (gethash tkf *suffix-cache*) val
                    (gethash (seq kf) *suffix-class*) :iku)
             (unless (gethash tkf-short *suffix-cache*)
               (setf (gethash tkf-short *suffix-cache*) val)))

        (load-kf :teii (get-kana-form 2820690 "いい") :class :ii)
        (load-kf :teii (get-kana-form 2820690 "いい") :class :ii :text "もいい")
        (load-kf :te (get-kana-form 2028940 "も") :class :mo)

        (load-kf :kudasai (get-kana-form 1184270 "ください" :conj :root))

        (load-conjs :suru 1157170) ;; する
        (load-conjs :suru 1421900 :itasu) ;; いたす
        ;; because suru isn't conjugated twice, this is added separately
        (load-conjs :suru 2269820 :sareru) ;; される
        (load-conjs :suru 1005160 :saseru) ;; させる

        (load-conjs :sou 1006610) ;; そう
        (load-conjs :sou+ 2141080) ;; そうにない

        (load-kf :rou (get-kana-form 1928670 "だろう") :text "ろう")

        (load-conjs :sugiru 1195970) ;; すぎる

        (load-kf :sa (get-kana-form 2029120 "さ"))

        (load-kf :ren (get-kana-form 1008120 "つつ") :class :tsutsu)
        (load-conjs :ren 2027910 :tsutsuaru)

        (load-kf :ren (get-kana-form 1454500 "うる") :class :uru)
        (load-kf :neg (car (find-word-conj-of "なく" 1529520)) :class :nai)

        (load-conjs :adv 1375610 :naru) ;; なる

        (load-conjs :teren 1012740 :yagaru)

        (load-kf :ra (get-kana-form 2067770 "ら"))

        (load-conjs :rashii 1013240) ;; らしい

        (load-kf :desu (get-kana-form 1628500 "です"))

        (load-kf :desho (get-kana-form 1008420 "でしょう"))
        (load-kf :desho (get-kana-form 1008420 "でしょ"))

        (load-conjs :tosuru 2136890) ;; とする

        (load-kf :kurai (get-kana-form 1154340 "くらい"))
        (load-kf :kurai (get-kana-form 1154340 "ぐらい"))

        (load-conjs :garu 1631750) ;; がる

        (load-kf :ren (get-kana-form 2016470 "がち") :class :gachi)

        (load-kf :iadj (get-kana-form 2006580 "げ"))
        (load-kf :iadj (get-kana-form 1604890 "め") :class :me)

        (load-abbr :nai "ねえ")
        (load-abbr :nai "ねぇ")
        (load-abbr :nai "ねー")
        (load-abbr :nai-x "ず")
        (load-abbr :nai-x "ざる")
        (load-abbr :nai-x "ぬ")
        (load-abbr :nai-n "ん")

        (load-abbr :nakereba "なきゃ")
        (load-abbr :nakereba "なくちゃ")

        ;; TODO: this abbr conflicts with noun + や too often
        ;; (load-abbr :eba "や") ;; う
        (load-abbr :teba "ちゃ" :join t) ;; つ
        (load-abbr :reba "りゃ") ;; る
        (load-abbr :keba "きゃ") ;; く
        (load-abbr :geba "ぎゃ") ;; ぐ
        (load-abbr :neba "にゃ") ;; ぬ
        (load-abbr :beba "びゃ") ;; ぶ
        (load-abbr :meba "みゃ") ;; む
        (load-abbr :seba "しゃ") ;; す

        (load-abbr :shimashou "しましょ")
        (load-abbr :dewanai "じゃない")

        (load-abbr :ii "ええ")
        ))))

(defun init-suffixes (&optional blocking reset)
  (when (or reset (not *suffix-cache*))
    (init-suffix-hashtables)
    (if blocking
        (init-suffixes-thread)
        (sb-thread:make-thread #'init-suffixes-thread)))
  (init-suffixes-running-p))

(defparameter *suffix-list* nil)
(defparameter *suffix-unique-only* nil)

(defmacro defsuffix (name key (root-var suf-var suf-obj-var) &body body)
  `(progn
     (defun ,name (,root-var ,suf-var ,suf-obj-var)
       ,@body)
     (pushnew (cons ,key ',name) *suffix-list*)))

;;(defvar *suffix-map-temp* nil) defined in dict.lisp

(defmacro def-simple-suffix (name keyword
                             (&key (stem 0) (score 0) (connector ""))
                                (root-var &optional suf-var patch-var)
                             &body get-primary-words)
  (alexandria:with-gensyms (suf primary-words)
    (unless suf-var (setf suf-var (gensym "SV")))
    (unless patch-var (setf patch-var (gensym "PV")))
    `(defsuffix ,name ,keyword (,root-var ,suf-var ,suf)
       (let* ((*suffix-map-temp* ,(if (= stem 0) '*suffix-map-temp* nil))
              (,patch-var nil)
              (,primary-words (progn ,@get-primary-words)))
         (mapcar (lambda (pw &aux score-base)
                   (when (listp pw)
                     (setf score-base (second pw)
                           pw (first pw)))
                   (adjoin-word pw ,suf
                                :text (concatenate 'string ,root-var ,suf-var)
                                :kana (let ((k (get-kana pw)))
                                        (concatenate 'string
                                                     (if ,patch-var
                                                         (concatenate 'string
                                                                      (destem k (length (car ,patch-var)))
                                                                      (cdr ,patch-var))
                                                         (destem k ,stem))
                                                     ,connector
                                                     ,suf-var))
                                :score-mod ,score
                                :score-base score-base))
                 ,primary-words)))))

(def-simple-suffix suffix-tai :tai (:connector "" :score 5) (root)
  (find-word-with-conj-type root 13))

(def-simple-suffix suffix-ren :ren (:connector "" :score 5) (root)
  ;; generic ren'youkei suffix
  (find-word-with-conj-type root 13))

(def-simple-suffix suffix-ren- :ren- (:connector "" :score 0) (root)
  (find-word-with-conj-type root 13))

(def-simple-suffix suffix-neg :neg (:connector "" :score 5) (root)
  (find-word-with-conj-type root 13 +conj-negative-stem+))

(defun te-check (root)
  (and (not (equal root "で"))
       (find (char root (1- (length root))) "てで")
       (find-word-with-conj-type root 3)))

(def-simple-suffix suffix-te :te (:connector "" :score 0) (root)
  (te-check root))

(def-simple-suffix suffix-te+ :te+ (:connector "" :score 3) (root)
  (te-check root))

(def-simple-suffix suffix-te++ :te++ (:connector "" :score 6) (root)
  (te-check root))

(def-simple-suffix suffix-te+space :te+space (:connector " " :score 3) (root)
  (te-check root))

(def-simple-suffix suffix-kudasai :kudasai (:connector " " :score (constantly 360)) (root)
  (te-check root))

(def-simple-suffix suffix-te-ren :teren (:connector "" :score 4) (root)
  (and (not (equal root "で"))
       (cond ((find (char root (1- (length root))) "てで")
              (find-word-with-conj-type root 3))
             ((not (member root '("い") :test 'equal))
              (find-word-with-conj-type root 13)))))

(def-simple-suffix suffix-teii :teii (:connector " " :score 1) (root)
  (and (find (char root (1- (length root))) "てで")
       (find-word-with-conj-type root 3)))

(def-simple-suffix suffix-chau :chau (:stem 1 :score 5) (root suf)
  (let ((te (case (char suf 0)
              (#\HIRAGANA_LETTER_ZI "で")
              (#\HIRAGANA_LETTER_TI "て"))))
    (when te
      (find-word-with-conj-type (concatenate 'string root te) 3))))

(def-simple-suffix suffix-to :to (:stem 1 :score 0) (root suf)
  (let ((te (case (char suf 0)
              (#\HIRAGANA_LETTER_TO "て")
              (#\HIRAGANA_LETTER_DO "で"))))
    (when te
      (find-word-with-conj-type (concatenate 'string root te) 3))))

(def-simple-suffix suffix-suru :suru (:connector " " :score 5) (root)
  (find-word-with-pos root "vs"))

(defun apply-patch (root patch)
  (concatenate 'string (subseq root 0 (- (length root) (length (cdr patch)))) (car patch)))

(defmacro suffix-sou-base (root patch)
  `(cond ((alexandria:ends-with-subseq "なさ" ,root)
          (setf ,patch '("い" . "さ"))
          (let ((root (apply-patch ,root ,patch))
                (*suffix-map-temp* nil))
            (find-word-with-conj-prop root (lambda (cdata)
                                             (conj-neg (conj-data-prop cdata))))))
         ((not (member ,root '("な" "よ" "よさ" "に" "き") :test 'equal))
          (find-word-with-conj-type ,root 13 +conj-adjective-stem+ +conj-adverbial+))))

(def-simple-suffix suffix-sou :sou (:score (constantly (cond
                                                         ((equal root "から") 40)
                                                         ((equal root "い") 0)
                                                         ((equal root "出来") 100)
                                                         (t 60)))
                                    :connector "")
    (root suf patch)
  (suffix-sou-base root patch))

(def-simple-suffix suffix-sou+ :sou+ (:connector "" :score 1)
    (root suf patch)
  (suffix-sou-base root patch))

(def-simple-suffix suffix-rou :rou (:connector "" :score 1) (root)
  (find-word-with-conj-type root 2))

(def-simple-suffix suffix-adv :adv (:connector "" :score 1) (root)
  (find-word-with-conj-type root +conj-adverbial+))

(def-simple-suffix suffix-sugiru :sugiru (:stem 1 :connector "" :score 5) (root suf patch)
  (let ((root (cond ((equal root "い") nil)
                    ((or (alexandria:ends-with-subseq "なさ" root)
                         (alexandria:ends-with-subseq "無さ" root))
                     (setf patch '("い" . "さ"))
                     (apply-patch root patch))
                    (t (concatenate 'string root "い")))))
    (when root
      (cond
        ((and patch (> (length root) 2))
         (find-word-with-conj-prop root (lambda (cdata)
                                          (conj-neg (conj-data-prop cdata)))))
        (t (find-word-with-pos root "adj-i"))))))

(def-simple-suffix suffix-sa :sa (:connector "" :score 2) (root)
  (nconc
   (find-word-with-conj-type root +conj-adjective-stem+)
   (find-word-with-pos root "adj-na")))

(pushnew (cons :sa
               (lambda (matches)
                 (let ((seqs (loop for match in matches if (seq match) collect it)))
                   (and seqs (query (:select 'seq :from 'entry :where (:and (:in 'seq (:set seqs)) 'root-p)) :column)))))
         *suffix-unique-only*)

(def-simple-suffix suffix-iadj :iadj (:connector "" :score 1) (root)
  (find-word-with-conj-type root +conj-adjective-stem+))

(def-simple-suffix suffix-garu :garu (:connector "" :score 0) (root suf patch)
  (unless (member root '("な" "い" "よ") :test 'equal)
    (or (find-word-with-conj-type root +conj-adjective-stem+)
        (when (alexandria:ends-with-subseq "そ" root)
          (setf patch '("う" . ""))
          (let ((root (apply-patch root patch))
                (*suffix-map-temp* nil))
            (find-word-with-suffix root :sou))))))

(def-simple-suffix suffix-ra :ra (:connector "" :score 1) (root)
  (unless (alexandria:ends-with-subseq "ら" root)
    (or (or-as-hiragana 'find-word-with-pos root "pn")
        (find-word-seq root 1580640))))

(pushnew :ra *suffix-unique-only*)

(def-simple-suffix suffix-rashii :rashii (:connector "" :score 3) (root)
  (pair-words-by-conj
   (find-word-with-conj-type root 2)
   (find-word-with-conj-type (concatenate 'string root "ら") 11)))

(def-simple-suffix suffix-desu :desu (:connector " " :score (constantly 200)) (root)
  (and (or (alexandria:ends-with-subseq "ない" root)
           (alexandria:ends-with-subseq "なかった" root))
       (find-word-with-conj-prop root (lambda (cdata)
                                        (conj-neg (conj-data-prop cdata))))))

(pushnew (cons :desu
               (lambda (matches)
                 (let ((seqs (loop for match in matches if (seq match) collect it)))
                   (< (length (and seqs (query (:select 'seq :from 'conjugation
                                                        :where (:and (:in 'seq (:set seqs))
                                                                     (:= 'from 2755350))) ;; じゃない
                                               :column)))
                      (length matches)))))
         *suffix-unique-only*)

(def-simple-suffix suffix-desho :desho (:connector " " :score (constantly 300)) (root)
  (and (alexandria:ends-with-subseq "ない" root)
       (find-word-with-conj-prop root (lambda (cdata)
                                        (conj-neg (conj-data-prop cdata))))))

(def-simple-suffix suffix-tosuru :tosuru (:connector " " :score 3) (root)
  (find-word-with-conj-type root 9))

(def-simple-suffix suffix-kurai :kurai (:connector " " :score 3) (root)
  (find-word-with-conj-type root 2))

(pushnew :mo *suffix-unique-only*)
(pushnew :nikui *suffix-unique-only*)

(defmacro def-abbr-suffix (name keyword stem
                           (root-var &optional suf-var patch-var)
                           &body get-primary-words)
  (alexandria:with-gensyms (suf primary-words)
    (unless suf-var (setf suf-var (gensym "SV")))
    (unless patch-var (setf patch-var (gensym "PV")))
    `(defsuffix ,name ,keyword (,root-var ,suf-var ,suf)
       (declare (ignore ,suf))
       (let* ((*suffix-map-temp* nil)
              (,patch-var nil)
              (,primary-words (progn ,@get-primary-words)))
         (mapcar (lambda (pw)
                   (let ((text (concatenate 'string ,root-var ,suf-var))
                         (kana (let ((k (get-kana pw)))
                                 (concatenate 'string
                                              (if ,patch-var
                                                  (concatenate 'string
                                                               (destem k (length (car ,patch-var)))
                                                               (cdr ,patch-var))
                                                  (destem k ,stem))
                                              ,suf-var))))
                     (etypecase pw
                       (simple-text
                        (make-instance 'proxy-text
                                       :source pw
                                       :text text
                                       :kana kana
                                       :hintedp t))
                       (compound-text
                        (with-slots ((stext text) (skana kana)) pw
                          (setf stext text skana kana))
                        pw))))
                 ,primary-words)))))


(def-abbr-suffix abbr-nee :nai 2 (root)
  (find-word-with-conj-prop
   (concatenate 'string root "ない")
   (lambda (cdata)
     ;; 居ない 来ない create problems so they are blocked
     (and (not (find (conj-data-from cdata) '(1577980 1547720)))
          (conj-neg (conj-data-prop cdata))))
   :allow-root t))

(def-abbr-suffix abbr-nx :nai-x 2 (root suf patch)
  (cond ((equal root "せ")
         (setf patch '("しない" . "せ"))
         (find-word-conj-of "しない" 1157170))
        (t
         (find-word-with-conj-prop
          (concatenate 'string root "ない")
          (lambda (cdata)
            (and (/= (conj-data-from cdata) 1157170)
                 (conj-neg (conj-data-prop cdata))))))))

(def-abbr-suffix abbr-n :nai-n 2 (root)
  (find-word-with-conj-prop
   (concatenate 'string root "ない")
   (lambda (cdata)
     ;; 居ない 来ない create problems so they are blocked
     (and (not (find (conj-data-from cdata) '(1577980 1547720)))
          (conj-neg (conj-data-prop cdata))))))

(pushnew :nai-n *suffix-unique-only*)

(def-abbr-suffix abbr-nakereba :nakereba 4 (root)
  (find-word-full (concatenate 'string root "なければ")))

(def-abbr-suffix abbr-shimasho :shimashou 5 (root)
  (find-word-full (concatenate 'string root "しましょう")))

(def-abbr-suffix abbr-dewanai :dewanai 4 (root)
  (find-word-full (concatenate 'string root "ではない")))

(pushnew :dewanai *suffix-unique-only*)

;; (def-abbr-suffix abbr-eba :eba 2 (root)
;;   (find-word-full (concatenate 'string root "えば")))

(def-abbr-suffix abbr-teba :teba 2 (root)
  (find-word-full (concatenate 'string root "てば")))

(def-abbr-suffix abbr-reba :reba 2 (root)
  (find-word-full (concatenate 'string root "れば")))

(def-abbr-suffix abbr-keba :keba 2 (root)
  (find-word-full (concatenate 'string root "けば")))

(def-abbr-suffix abbr-geba :geba 2 (root)
  (find-word-full (concatenate 'string root "げば")))

(def-abbr-suffix abbr-neba :neba 2 (root)
  (find-word-full (concatenate 'string root "ねば")))

(def-abbr-suffix abbr-beba :beba 2 (root)
  (find-word-full (concatenate 'string root "べば")))

(def-abbr-suffix abbr-meba :meba 2 (root)
  (find-word-full (concatenate 'string root "めば")))

(def-abbr-suffix abbr-seba :seba 2 (root)
  (find-word-full (concatenate 'string root "せば")))

(pushnew :eba *suffix-unique-only*)
(pushnew :teba *suffix-unique-only*)
(pushnew :reba *suffix-unique-only*)
(pushnew :keba *suffix-unique-only*)
(pushnew :geba *suffix-unique-only*)
(pushnew :neba *suffix-unique-only*)
(pushnew :beba *suffix-unique-only*)
(pushnew :meba *suffix-unique-only*)
(pushnew :seba *suffix-unique-only*)

(def-abbr-suffix abbr-ii :ii 2 (root)
  (find-word-full (concatenate 'string root "いい")))

(pushnew :ii *suffix-unique-only*)

(defun parse-suffix-val (substr val)
  (when val
    (cond ((consp (car val))
           (loop for v in val collect (cons substr v)))
          (t (list (cons substr val))))))

(defun get-suffix-map (str)
  (init-suffixes)
  (let ((result (make-hash-table)))
    (loop for start from 0 below (length str)
         do (loop for end from (1+ start) upto (length str)
                 do (let* ((substr (subseq-slice nil str start end))
                           (val (gethash substr *suffix-cache*)))
                      (loop for item in (parse-suffix-val substr val)
                         do (push item (gethash end result nil))))))
    result))

(defun get-suffixes (word)
  (init-suffixes)
  (loop for start from (1- (length word)) downto 1
     for substr = (subseq-slice nil word start)
     for val = (gethash substr *suffix-cache*)
     nconc (parse-suffix-val substr val)))

(defun match-unique (suffix-class matches)
  (let ((uniq (find suffix-class *suffix-unique-only* :key (lambda (x) (if (consp x) (car x) x)))))
    (cond ((consp uniq)
           (funcall (cdr uniq) matches))
          (t uniq))))

(defun find-word-suffix (word &key matches)
  (loop with suffixes = (if *suffix-map-temp*
                            (gethash *suffix-next-end* *suffix-map-temp*)
                            (get-suffixes word))
       and slice = (make-slice)
     for (suffix keyword kf) in suffixes
     for suffix-fn = (cdr (assoc keyword *suffix-list*))
     for suffix-class = (if kf (gethash (seq kf) *suffix-class*) keyword)
     for offset = (- (length word) (length suffix))
     when (and suffix-fn (> offset 0)
               (not (and matches (match-unique suffix-class matches))))
     nconc (let ((*suffix-next-end* (and *suffix-next-end* (- *suffix-next-end* (length suffix)))))
             (funcall suffix-fn (subseq-slice slice word 0 offset) suffix kf))))


;;; SYNERGIES (gives a bonus to two consequent words in a path)
;;; some of those should be converted to suffixes/prefixes

(defstruct synergy description connector score start end)

(defmethod get-segment-score ((syn synergy))
  (synergy-score syn))

(defun make-segment-list-from (old-segment-list segments)
  (let ((new-segment-list (copy-segment-list old-segment-list)))
    (setf (segment-list-segments new-segment-list) segments)
    new-segment-list))

(defparameter *synergy-list* nil)

(defmacro defsynergy (name (left-var right-var) &body body)
  `(progn
     (defun ,name (,left-var ,right-var)
       ,@body)
     (pushnew ',name *synergy-list*)))

(defmacro def-generic-synergy (name (segment-list-left segment-list-right)
                               filter-left filter-right &key description connector score)
  (alexandria:with-gensyms (start end left right)
   `(defsynergy ,name (,segment-list-left ,segment-list-right)
      (let ((,start (segment-list-end ,segment-list-left))
            (,end (segment-list-start ,segment-list-right)))
        (when (= ,start ,end)
          (let ((,left (remove-if-not ,filter-left (segment-list-segments ,segment-list-left)))
                (,right (remove-if-not ,filter-right (segment-list-segments ,segment-list-right))))
            (when (and ,left ,right)
              (list (list (make-segment-list-from ,segment-list-right ,right)
                          (make-synergy :start ,start :end ,end
                                        :description ,description
                                        :connector ,connector
                                        :score ,score)
                          (make-segment-list-from ,segment-list-left ,left))))))))))

(defun filter-is-noun (segment)
  (or (destructuring-bind (k p c l) (getf (segment-info segment) :kpcl)
        (and (or l k (and p c))
             (intersection '("n" "n-adv" "n-t" "adj-na" "n-suf" "pn")
                           (getf (segment-info segment) :posi)
                           :test 'equal)))
      (and (typep (segment-word segment) 'counter-text)
           (getf (segment-info segment) :seq-set))))

(defmacro filter-is-pos (pos-list (segment &rest kpcl-vars) &body kpcl-test)
  `(lambda (,segment)
     (destructuring-bind ,kpcl-vars (getf (segment-info ,segment) :kpcl)
       (declare (ignorable ,@kpcl-vars))
       (and (progn ,@kpcl-test)
            (intersection ',pos-list
                          (getf (segment-info ,segment) :posi)
                          :test 'equal)))))

(declaim (inline filter-in-seq-set))
(defun filter-in-seq-set (&rest seqs)
  (lambda (segment)
    (intersection seqs (getf (segment-info segment) :seq-set))))

(declaim (inline filter-is-conjugation))
(defun filter-is-conjugation (conj-type)
  (lambda (segment)
    (member conj-type (getf (segment-info segment) :conj)
            :key (lambda (cdata) (conj-type (conj-data-prop cdata))))))

(declaim (inline filter-is-compound-end))
(defun filter-is-compound-end (&rest seqs)
  (lambda (segment)
    (let* ((word (segment-word segment))
           (seq (seq word)))
      (and seq (listp seq)
           (member (car (last seq)) seqs)))))

(declaim (inline filter-is-compound-end-text))
(defun filter-is-compound-end-text (&rest texts)
  (lambda (segment)
    (let* ((word (segment-word segment))
           (seq (seq word)))
      (and seq (listp (seq word))
           (find (get-text (car (last (words word)))) texts :test 'equal)))))

(defparameter *noun-particles*
  '(2028920 ;; は
    2028930 ;; が
    2028990 ;; に
    2028980 ;; で
    2029000 ;; へ
    1007340 ;; だけ
    1579080 ;; ごろ
    1525680 ;; まで
    2028940 ;; も
    1582300 ;; など
    2215430 ;; には
    1469800 ;; の
    1009990 ;; のみ
    2029010 ;; を
    1005120 ;; さえ
    2034520 ;; でさえ
    1005120 ;; すら
    1008490 ;; と
    1008530 ;; とか
    1008590 ;; として
    2028950 ;; とは
    2028960 ;; や
    1009600 ;; にとって
    ))

(def-generic-synergy synergy-noun-particle (l r)
  #'filter-is-noun
 (apply #'filter-in-seq-set *noun-particles*)
  :description "noun+prt"
  :score (+ 10 (* 4 (- (segment-list-end r) (segment-list-start r))))
  :connector " ")

;; (def-generic-synergy synergy-suru-verb (l r)
;;   (filter-is-pos ("vs") (segment k p c l) (or k l (and p c)))
;;   (filter-in-seq-set 1157170) ;; する
;;   :description "noun+suru"
;;   :score 10
;;   :connector "")

(def-generic-synergy synergy-noun-da (l r)
  #'filter-is-noun
  (filter-in-seq-set 2089020) ;; だ
  :description "noun+da"
  :score 10
  :connector " ")

(def-generic-synergy synergy-no-da (l r)
  (filter-in-seq-set 1469800 2139720)
  (filter-in-seq-set 2089020 1007370 1928670)
  :description "no da/desu"
  :score 15
  :connector " ")

;; TODO: remove this hack
(def-generic-synergy synergy-sou-nanda (l r)
  (filter-in-seq-set 2137720)
  (filter-in-seq-set 2140410)
  :description "sou na n da"
  :score 50
  :connector " ")

(def-generic-synergy synergy-no-adjectives (l r)
  (filter-is-pos ("adj-no") (segment k p c l) (or k l (and p c)))
  (filter-in-seq-set 1469800) ;; の
  :description "no-adjective"
  :score 15
  :connector " ")

(def-generic-synergy synergy-na-adjectives (l r)
  (filter-is-pos ("adj-na") (segment k p c l) (or k l (and p c)))
  (filter-in-seq-set 2029110 2028990) ;; な ; に
  :description "na-adjective"
  :score 15
  :connector " ")

(def-generic-synergy synergy-to-adverbs (l r)
  (filter-is-pos ("adv-to") (segment k p c l) (or k l p))
  (filter-in-seq-set 1008490)
  :description "to-adverb"
  :score (+ 10 (* 10 (- (segment-list-end l) (segment-list-start l))))
  :connector " ")

(def-generic-synergy synergy-suffix-chu (l r)
  #'filter-is-noun
  (filter-in-seq-set 1620400 2083570)
  :description "suffix-chu"
  :score 12
  :connector "-")

(def-generic-synergy synergy-suffix-tachi (l r)
  #'filter-is-noun
  (filter-in-seq-set 1416220)
  :description "suffix-tachi"
  :score 10
  :connector "-")

(def-generic-synergy synergy-suffix-buri (l r)
  #'filter-is-noun
  (filter-in-seq-set 1361140)
  :description "suffix-buri"
  :score 40
  :connector "")

(def-generic-synergy synergy-suffix-sei (l r)
  #'filter-is-noun
  (filter-in-seq-set 1375260)
  :description "suffix-sei"
  :score 12
  :connector ""
  )

(def-generic-synergy synergy-o-prefix (l r)
  (filter-in-seq-set 1270190)
  (filter-is-pos ("n") (segment k p c l) (or k l))
  :description "o+noun"
  :score 10
  :connector "")

(def-generic-synergy synergy-kanji-prefix (l r)
  (filter-in-seq-set 2242840 1922780 2423740) ;; 未 不
  (filter-is-pos ("n") (segment k p c l) k)
  :description "kanji prefix+noun"
  :score 15
  :connector "")

(def-generic-synergy synergy-shicha-ikenai (l r)
  (filter-is-compound-end 2028920) ;; は
  (filter-in-seq-set 1000730 1612750 1409110 2829697 1587610) ;; いけない いけません だめ いかん いや
  :description "shicha ikenai"
  :score 50
  :connector " ")

(def-generic-synergy synergy-shika-negative (l r)
  (filter-in-seq-set 1005460) ;; しか
  (lambda (segment)
    (some (lambda (cdata)
            (conj-neg (conj-data-prop cdata)))
          (getf (segment-info segment) :conj)))
  :description "shika+neg"
  :score 50
  :connector " ")

(def-generic-synergy synergy-no-toori (l r)
  (filter-in-seq-set 1469800)
  (filter-in-seq-set 1432920)
  :description "no toori"
  :score 50
  :connector " ")

(defun get-synergies (segment-list-left segment-list-right)
  (loop for fn in *synergy-list*
     nconc (funcall fn segment-list-left segment-list-right)))


;;; PENALTIES (similar to synergy but reduces the score of two consequent segments)

(defparameter *penalty-list* nil)

(defmacro defpenalty (name (left-var right-var) &body body)
  `(progn
     (defun ,name (,left-var ,right-var)
       ,@body)
     (pushnew ',name *penalty-list*)))

(defmacro def-generic-penalty (name (segment-list-left segment-list-right)
                               test-left test-right &key (serial t) description score (connector " "))
  (alexandria:with-gensyms (start end)
   `(defpenalty ,name (,segment-list-left ,segment-list-right)
      (let ((,start (segment-list-end ,segment-list-left))
            (,end (segment-list-start ,segment-list-right)))
        (when (and ,(if serial `(= ,start ,end) t)
                   (funcall ,test-left ,segment-list-left)
                   (funcall ,test-right ,segment-list-right))
          (make-synergy :start ,start :end ,end
                        :description ,description
                        :connector ,connector
                        :score ,score))))))

(declaim (inline filter-short-kana))
(defun filter-short-kana (len &key except)
  (lambda (segment-list)
    (let ((seg (car (segment-list-segments segment-list))))
      (and seg
           (<= (- (segment-list-end segment-list)
                  (segment-list-start segment-list)) len)
           (not (car (getf (segment-info seg) :kpcl)))
           (not (and except (member (get-text seg) except :test 'equal)))))))

(def-generic-penalty penalty-short (l r)
  (filter-short-kana 1)
  (filter-short-kana 1 :except '("と"))
  :description "short"
  :serial nil
  :score -9)

(def-generic-penalty penalty-semi-final (l r)
  (lambda (sl)
    (some (lambda (s) (funcall (apply 'filter-in-seq-set *semi-final-prt*) s))
          (segment-list-segments sl)))
  (constantly t)
  :description "semi-final not final"
  :score -15)

(defun get-penalties (seg-left seg-right)
  (loop for fn in *penalty-list*
     for penalty = (funcall fn seg-left seg-right)
     when penalty
       do (return (list seg-right penalty seg-left))
     finally (return (list seg-right seg-left))))


;;; SEGFILTERS (used to ban certain combinations of subsequent words)
;;; a function is called with arguments segment-list-left and segment-list-right and
;;; must return a list of possible segment-list-left/segment-list-right combinations
;;; segfilter must work even if segment-list-left is nil

(defparameter *segfilter-list* nil)

(defmacro defsegfilter (name (left-var right-var) &body body)
  `(progn
     (defun ,name (,left-var ,right-var)
       ,@body)
     (pushnew ',name *segfilter-list*)))

(defun classify (filter list)
  (loop for element in list
     if (funcall filter element)
     collect element into yep
     else collect element into nope
     finally (return (values yep nope))))

(defmacro def-segfilter-must-follow (name (segment-list-left segment-list-right)
                                     filter-left filter-right &key allow-first)
  "This segfilter is for when segments that satisfy filter-right MUST follow segments that
   satisfy filter-left"
  (alexandria:with-gensyms (satisfies-left contradicts-left satisfies-right contradicts-right result)
    `(defsegfilter ,name (,segment-list-left ,segment-list-right)
       (multiple-value-bind (,satisfies-right ,contradicts-right)
           (classify ,filter-right (segment-list-segments ,segment-list-right))
         (cond
           ((or (not ,satisfies-right) (and ,allow-first (not ,segment-list-left)))
            (list (list ,segment-list-left ,segment-list-right)))
           ((or (not ,segment-list-left)
                (/= (segment-list-end ,segment-list-left) (segment-list-start ,segment-list-right)))
            (when ,contradicts-right
              (list (list ,segment-list-left
                          (make-segment-list-from ,segment-list-right ,contradicts-right)))))
           (t
            (multiple-value-bind (,satisfies-left ,contradicts-left)
                (classify ,filter-left (segment-list-segments ,segment-list-left))
              (if ,contradicts-left
                  (let ((,result (when ,contradicts-right
                                   (list
                                    (list ,segment-list-left
                                          (make-segment-list-from ,segment-list-right ,contradicts-right))))))
                    (when ,satisfies-left
                      (push
                       (list (make-segment-list-from ,segment-list-left ,satisfies-left)
                             (make-segment-list-from ,segment-list-right ,satisfies-right))
                       ,result))
                    ,result)
                  (list (list ,segment-list-left ,segment-list-right))))))))))


(defparameter *aux-verbs*
  '(1342560 ;; 初める/そめる
    ;; 2141080 ;; そうにない
    ))

(def-segfilter-must-follow segfilter-aux-verb (l r)
  (filter-is-conjugation 13)
  (apply #'filter-in-seq-set *aux-verbs*))

(def-segfilter-must-follow segfilter-tsu-iru (l r) ;; TODO: remove this, or make more generic
  (complement (filter-in-seq-set 2221640))
  (filter-in-seq-set 1577980)
  :allow-first t)

(def-segfilter-must-follow segfilter-wokarasu (l r)
  (filter-in-seq-set 2029010)
  (filter-in-seq-set 2087020))

(def-segfilter-must-follow segfilter-badend (l r)
  (constantly nil)
  (filter-is-compound-end-text "ちゃい" "いか" "とか" "とき" "い"))

;; (def-segfilter-must-follow segfilter-itsu (l r)
;;   (complement (filter-is-compound-end-text "い"))
;;   (filter-in-seq-set 2221640 1013250)
;;   :allow-first t)

(def-segfilter-must-follow segfilter-roku (l r)
  (complement (filter-is-compound-end-text "いろ"))
  (lambda (segment) (alexandria:starts-with #\く (get-text segment)))
  :allow-first t)

(def-segfilter-must-follow segfilter-sae (l r)
  (complement (filter-is-compound-end 2029120))
  (lambda (segment) (alexandria:starts-with #\え (get-text segment)))
  :allow-first t)

(def-segfilter-must-follow segfilter-janai (l r)
  (complement (filter-is-compound-end 2028920))
  (filter-in-seq-set 1529520 1296400 2139720)
  :allow-first t)

(def-segfilter-must-follow segfilter-nohayamete (l r)
  (complement (filter-in-seq-set 1469800))
  (filter-in-seq-set 1601080)
  :allow-first t)

(def-segfilter-must-follow segfilter-toomou (l r)
  ;; split と before 思う 言う
  (complement (filter-in-seq-set 2837117)) ;; 何だと
  (filter-in-seq-set 1589350 1587040)
  :allow-first t)

(def-segfilter-must-follow segfilter-totte (l r)
  (complement (filter-in-seq-set 1008490))
  (filter-in-seq-set 2086960)
  :allow-first t)

(def-segfilter-must-follow segfilter-dashi (l r)
  (lambda (segment &aux (seq-set (getf (segment-info segment) :seq-set)))
    (or (not (find 2089020 seq-set)) ;; だ
        (find 2028980 seq-set))) ;; で
  (filter-in-seq-set 1157170 2424740 1305070) ;; する　して
  :allow-first t)

(defun apply-segfilters (seg-left seg-right)
  (loop with splits = (list (list seg-left seg-right))
     for segfilter in *segfilter-list*
     do (setf splits
              (loop for (seg-left seg-right) in splits
                 nconc (funcall segfilter seg-left seg-right)))
     finally (return splits)))

(defparameter *honorifics*
  '(1247260 ;; 君
    ))

(def-segfilter-must-follow segfilter-honorific (l r)
  (complement (filter-in-seq-set *noun-particles*))
  (filter-in-seq-set *honorifics*))
