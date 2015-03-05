(in-package :ichiran/dict)

;;; SUFFIXES (makes compound word primary + suffix)

(defparameter *suffix-cache* nil)
(defparameter *suffix-class* nil) ;; seq -> class

(defun init-suffix-hashtables ()
  (setf *suffix-cache* (make-hash-table :test 'equal)
        *suffix-class* (make-hash-table :test 'eql)))

(defun get-kana-forms-conj-data-filter (conj-data)
  (unless (skip-by-conj-data conj-data)
    (loop for cd in conj-data
       for prop = (conj-data-prop cd)
       for ctype = (conj-type prop)
       unless (member ctype *weak-conj-types*)
       collect (conj-id prop)))) 

(defun get-kana-forms (seq)
  (loop for kt in 
       (query-dao 'kana-text (:select 'kt.* :distinct :from (:as 'kana-text 'kt)
                                      :left-join (:as 'conjugation 'conj) :on (:= 'conj.seq 'kt.seq)
                                      :where (:or (:= 'kt.seq seq)
                                                  (:= 'conj.from seq))))
       if (= (seq kt) seq)
       do (setf (word-conjugations kt) :root) and collect kt
       else if (let ((conj-ids (get-kana-forms-conj-data-filter (get-conj-data (seq kt) seq))))
                 (when conj-ids
                   (setf (word-conjugations kt) conj-ids)))
            collect kt))

(defun get-kana-form (seq text)
  (car (select-dao 'kana-text (:and (:= 'text text) (:= 'seq seq)))))

(defun find-word-with-conj-prop (wordstr filter-fn)
  (loop for word in (find-word-full wordstr)
       for conj-data = (funcall filter-fn (word-conj-data word))
       for conj-ids = (mapcar (lambda (cdata) (conj-id (conj-data-prop cdata))) conj-data)
       when conj-data
       do (setf (word-conjugations word) conj-ids)
       and collect word))

(defun find-word-with-conj-type (word &rest conj-types)
  (find-word-with-conj-prop word
      (lambda (conj-data)
        (remove-if-not (lambda (cdata) (member (conj-type (conj-data-prop cdata)) conj-types))
                       conj-data))))

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

(defun get-suffix-class-description (class)
  (case class
    (:chau "indicates completion (to finish ...)")
    (:tai "want to... / would like to...")
    (:iru "indicates continuing action (to be ...ing)")
    (:aru "indicates completion / finished action")
    (:kuru "indicates action that had been continuing up till now / came to be ")
    (:oku "to do in advance / to leave in the current state expecting a later change")
    (:kureru "(asking) to let do something")
    (:iku "is becoming / action starting now and continuing")
    (:suru "makes a verb from a noun")
    (:itasu "makes a verb from a noun (humble)")
    (:sareru "makes a verb from a noun (honorific or passive)")
    (:rou "probably / it seems that... / I guess ...")
    (:ii "it's ok if ... / is it ok if ...?")
    (:mo "even if ...")
    (:sugiru "to be too (much) ...")
    (:nikui "difficult to...")
    (:kara "because/why")
    (:sa "-ness (degree or condition of adjective)")
    (:tsutsu "while ... / in the process of ...")
    (:tsutsuaru "to be doing ... / to be in the process of doing ...")
    (:uru "can ... / to be able to ...")
    (:sou "looking like ... / seeming ...")
    (:nai "negative suffix")
    (:ra "pluralizing suffix (not polite)")
    ))

(defun get-suffix-description (seq)
  (get-suffix-class-description (gethash seq *suffix-class*)))

(defvar *init-suffixes-lock* (sb-thread:make-mutex :name "init-suffixes-lock"))

(defun init-suffixes-running-p ()
  (or (not *suffix-cache*)
      (sb-thread:mutex-value *init-suffixes-lock*)))

(defun init-suffixes-thread ()
  (sb-thread:with-mutex (*init-suffixes-lock* :wait-p nil)
    (init-suffix-hashtables)
    (with-connection *connection*
      (labels ((load-kf (key kf &key class text)
                 (setf (gethash (or text (text kf)) *suffix-cache*) (list key kf)
                       (gethash (seq kf) *suffix-class*) (or class key)))
               (load-conjs (key seq &optional class)
                 (loop for kf in (get-kana-forms seq)
                    do (load-kf key kf :class class)))
               (load-abbr (key text)
                 (setf (gethash text *suffix-cache*) (list key nil))))

        (load-conjs :chau 2013800)
        (load-conjs :tai 2017560)
        (load-conjs :ren 2772730 :nikui)

        (loop for kf in (get-kana-forms 1577980) ;; いる (る)
           for tkf = (text kf)
           for val = (list :te+ kf)
           do (setf (gethash tkf *suffix-cache*) val
                    (gethash (seq kf) *suffix-class*) :iru)
             (when (> (length tkf) 1)
               (setf (gethash (subseq tkf 1) *suffix-cache*) val)))
        
        (load-conjs :te 1296400 :aru) ;; ある

        (load-conjs :te 1547720 :kuru) ;; くる

        (load-conjs :te 1421850 :oku) ;; おく ;; TODO: implement teo -> to

        (load-conjs :te 1305380 :chau) ;; しまう
        
        (load-conjs :te+ 1269130 :kureru) ;; くれる

        (loop for kf in (get-kana-forms 1578850) ;;  いく / く
           for tkf = (text kf)
           for tkf-short = (subseq tkf 1)
           for val = (list :te kf)
           when (char= (char tkf 0) #\HIRAGANA_LETTER_I)
           do (setf (gethash tkf *suffix-cache*) val
                    (gethash (seq kf) *suffix-class*) :iku)
             (unless (gethash tkf-short *suffix-cache*)
               (setf (gethash tkf-short *suffix-cache*) val)))

        (load-kf :te (get-kana-form 2820690 "いい") :class :ii)
        (load-kf :te (get-kana-form 2820690 "いい") :class :ii :text "もいい")
        (load-kf :te (get-kana-form 2028940 "も") :class :mo)

        (load-conjs :suru 1157170) ;; する
        (load-conjs :suru 1421900 :itasu) ;; いたす  
        ;; because suru isn't conjugated twice, this is added separately
        (load-conjs :suru  2269820 :sareru) ;;  される

        (load-conjs :kara 1002980) ;; から

        (load-conjs :sou 1006610) ;; そう

        (load-kf :rou (get-kana-form 1928670 "だろう") :text "ろう")

        (load-conjs :sugiru 1195970) ;; すぎる

        (load-kf :sa (get-kana-form 2029120 "さ"))

        (load-kf :ren (get-kana-form 1008120 "つつ") :class :tsutsu)
        (load-conjs :ren 2027910 :tsutsuaru)

        (load-kf :ren (get-kana-form 1454500 "うる") :class :uru)

        (load-kf :ren (car (find-word-conj-of "なく" 1529520)) :class :nai)

        (load-kf :ra (get-kana-form 2067770 "ら"))

        ;;(load-abbr :nee "ねぇ")
        (load-abbr :nai "ねえ")
        (load-abbr :nai "ず")
        (load-abbr :nai "ぬ")
        ))))

(defun init-suffixes (&optional blocking)
  (unless *suffix-cache*
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
                                (root-var &optional suf-var kana-var)
                             &body get-primary-words)
  (alexandria:with-gensyms (suf primary-words)
    (unless suf-var (setf suf-var (gensym "SV")))
    (unless kana-var (setf kana-var (gensym "KV")))
    `(defsuffix ,name ,keyword (,root-var ,suf-var ,suf)
       (let* ((*suffix-map-temp* ,(if (= stem 0) '*suffix-map-temp* nil))
              (,kana-var nil)
              (,primary-words (progn ,@get-primary-words)))
         (mapcar (lambda (pw)
                   (adjoin-word pw ,suf
                                :text (concatenate 'string ,root-var ,suf-var)
                                :kana (let ((k (get-kana pw)))
                                        (concatenate 'string
                                                     (or ,kana-var
                                                         (subseq k 0 (- (length k) ,stem)))
                                                     ,connector
                                                     ,suf-var))
                                :score-mod ,score))
                 ,primary-words)))))
  
(def-simple-suffix suffix-chau :chau (:stem 1 :score 5) (root suf)
  (let ((te (case (char suf 0)
              (#\HIRAGANA_LETTER_ZI "で")
              (#\HIRAGANA_LETTER_TI "て"))))
    (when te
      (find-word-with-conj-type (concatenate 'string root te) 3))))

(def-simple-suffix suffix-tai :tai (:connector "" :score 5) (root)
  (find-word-with-conj-type root 13))

(def-simple-suffix suffix-ren :ren (:connector "" :score 5) (root)
  ;; generic ren'youkei suffix
  (find-word-with-conj-type root 13))

(def-simple-suffix suffix-te :te (:connector "" :score 0) (root)
  (and (find (char root (1- (length root))) "てで")
       (find-word-with-conj-type root 3)))

(def-simple-suffix suffix-te+ :te+ (:connector "" :score 3) (root)
  (and (find (char root (1- (length root))) "てで")
       (find-word-with-conj-type root 3)))

(def-simple-suffix suffix-suru :suru (:connector " " :score 5) (root)
  (find-word-with-pos root "vs"))

(def-simple-suffix suffix-kara :kara (:connector " " :score 1) (root)
  (or (find-word-seq root 1577100 2089020)
      #-(and)
      (and (find (char root (1- (length root))) "てで")
           (find-word-with-conj-type root 3))))

(def-simple-suffix suffix-sou :sou (:connector "" :score 3) (root)
  (unless (member root '("な" "よ") :test 'equal)
    (find-word-with-conj-type root 13 +conj-adjective-stem+)))

(def-simple-suffix suffix-rou :rou (:connector "" :score 1) (root)
  (find-word-with-conj-type root 2))

(def-simple-suffix suffix-sugiru :sugiru (:stem 1 :connector "" :score 5) (root suf kana)
  (let ((root (cond ((equal root "い") nil)
                    ((equal root "なさ") (setf kana "なさ") "ない")
                    (t (concatenate 'string root "い")))))
    (when root
      (find-word-with-pos root "adj-i"))))

(def-simple-suffix suffix-sa :sa (:connector "" :score 2) (root)
  (nconc
   (find-word-with-conj-type root +conj-adjective-stem+)
   (find-word-with-pos root "adj-na")))

(def-simple-suffix suffix-ra :ra (:connector "" :score 1) (root)
  (find-word-seq root 1002290 1457730))

(pushnew :sa *suffix-unique-only*)
(pushnew :mo *suffix-unique-only*)
(pushnew :nikui *suffix-unique-only*)

(defmacro def-abbr-suffix (name keyword stem
                           (root-var &optional suf-var kana-var)
                           &body get-primary-words)
  (alexandria:with-gensyms (suf primary-words)
    (unless suf-var (setf suf-var (gensym "SV")))
    (unless kana-var (setf kana-var (gensym "KV")))
    `(defsuffix ,name ,keyword (,root-var ,suf-var ,suf)
       (declare (ignore ,suf))
       (let* ((*suffix-map-temp* nil)
              (,kana-var nil)
              (,primary-words (progn ,@get-primary-words)))
         (mapcar (lambda (pw)
                   (let ((text (concatenate 'string ,root-var ,suf-var))
                         (kana (let ((k (get-kana pw)))
                                 (concatenate 'string
                                              (or ,kana-var
                                                  (subseq k 0 (- (length k) ,stem)))
                                              ,suf-var))))
                     (etypecase pw
                       (simple-text
                        (make-instance 'proxy-text
                                       :source pw
                                       :text text
                                       :kana kana))
                       (compound-text
                        (with-slots ((stext text) (skana kana)) pw
                          (setf stext text skana kana))
                        pw))))
                 ,primary-words)))))

(def-abbr-suffix abbr-nee :nai 2 (root)
  (find-word-full (concatenate 'string root "ない")))

(defun get-suffix-map (str)
  (init-suffixes)
  (let ((result (make-hash-table)))
    (loop for start from 0 below (length str)
         do (loop for end from (1+ start) upto (length str)
                 do (let* ((substr (subseq-slice nil str start end))
                           (val (gethash substr *suffix-cache*)))
                      (when val
                        (push (cons substr val) (gethash end result nil))))))
    result))

(defun get-suffixes (word)
  (init-suffixes)
  (loop for start from (1- (length word)) downto 1
       for substr = (subseq-slice nil word start)
       for val = (gethash substr *suffix-cache*)
       when val
       collect (cons substr val)))

(defun find-word-suffix (word &key unique)
  (loop with suffixes = (if *suffix-map-temp* 
                            (gethash *suffix-next-end* *suffix-map-temp*)
                            (get-suffixes word))
       and slice = (make-slice)
     for (suffix keyword kf) in suffixes
     for suffix-fn = (cdr (assoc keyword *suffix-list*))
     for suffix-class = (when kf (gethash (seq kf) *suffix-class*))
     for offset = (- (length word) (length suffix))
     when (and suffix-fn (> offset 0)
               (or unique (not (member suffix-class *suffix-unique-only*))))
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

(defparameter *synergy-list* nil)

(defun filter-is-noun (segment)
  (destructuring-bind (k p c l) (getf (segment-info segment) :kpcl)
    (and (or k l (and p c))
         (intersection '("n" "n-adv" "n-t" "adj-na")
                       (getf (segment-info segment) :posi)
                       :test 'equal))))

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
    2039380 ;; にて
    2215430 ;; には
    1469800 ;; の
    1009990 ;; のみ
    2029010 ;; を
    1005120 ;; さえ
    2034520 ;; でさえ
    1005120 ;; すら
    1008490 ;; と
    1008530 ;; とか
    2028960 ;; や
    ))

(def-generic-synergy synergy-noun-particle (l r)
  #'filter-is-noun
 (apply #'filter-in-seq-set *noun-particles*)
  :description "noun+prt"
  :score 10
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
  (filter-in-seq-set 2089020 1007370)
  :description "no da/desu"
  :score 5
  :connector " ")

(def-generic-synergy synergy-no-adjectives (l r)
  (filter-is-pos ("adj-no") (segment k p c l) (or k l (and p c)))
  (filter-in-seq-set 1469800) ;; の
  :description "no-adjective"
  :score 15
  :connector "")

(def-generic-synergy synergy-na-adjectives (l r)
  (filter-is-pos ("adj-na") (segment k p c l) (or k l (and p c)))
  (filter-in-seq-set 2029110 2028990) ;; な ; に 
  :description "na-adjective"
  :score 15
  :connector "")

(def-generic-synergy synergy-suffix-chu (l r)
  #'filter-is-noun
  (filter-in-seq-set 1620400 2083570)
  :description "suffix-chu"
  :score 5
  :connector "-")

(def-generic-synergy synergy-o-prefix (l r)
  (filter-in-seq-set 1270190)
  (filter-is-pos ("n") (segment k p c l) (or k l))
  :description "o+noun"
  :score 10
  :connector "")

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

(declaim (inline filter-too-short))
(defun filter-short-kana (len)
  (lambda (segment-list)
    (and
     (<= (- (segment-list-end segment-list)
            (segment-list-start segment-list)) len)
     (let ((seg (car (segment-list-segments segment-list))))
       (not (car (getf (segment-info seg) :kpcl)))))))

(def-generic-penalty penalty-short (l r)
  (filter-short-kana 1)
  (filter-short-kana 1)
  :description "short"
  :serial nil
  :score -9)

(defun get-penalties (seg-left seg-right)
  (loop for fn in *penalty-list*
     for penalty = (funcall fn seg-left seg-right)
     when penalty
       do (return (list seg-right penalty seg-left))
     finally (return (list seg-right seg-left))))
