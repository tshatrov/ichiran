(in-package :ichiran/dict)

;; SPLITS (words that should be scored as two or more other words)

(defparameter *split-map* (make-hash-table)) ;; seq -> split function

(defmacro defsplit (name seq (reading-var) &body body)
  `(progn
     (defun ,name (,reading-var) ;; reading -> (values parts score-bonus)
       ,@body)
     (setf (gethash ,seq *split-map*) ',name)))

(defmacro def-simple-split (name seq score (&optional length-var text-var reading-var) &body parts-def)
  "each part is (seq length-form)"
  (alexandria:with-gensyms (offset parts pseq part-length)
    (unless reading-var (setf reading-var (gensym "RV")))
    (unless length-var (setf length-var (gensym "LV")))
    (unless text-var (setf text-var (gensym "TV")))
    `(defsplit ,name ,seq (,reading-var)
       (let* ((,text-var (true-text ,reading-var))
              (,length-var (length ,text-var))
              (,offset 0)
              (,parts nil))
         (declare (ignorable ,text-var ,length-var))
         ,@(loop for (part-seq part-length-form conj-p rendaku-p) in parts-def
              if (eql part-seq :test)
                collect
                `(unless ,part-length-form
                   (return-from ,name nil))
              else
              collect
                `(let ((,pseq ,(if (listp part-seq)
                                   (if (and part-seq (stringp (car part-seq)))
                                       `(list (seq (car (find-word-conj-of ,@part-seq))))
                                       `',part-seq)
                                   `',(list part-seq)))
                       (,part-length ,part-length-form))
                   (push (car (apply
                               ,(if conj-p
                                   ''find-word-conj-of
                                   ''find-word-seq)
                                (let ((part-txt (subseq ,text-var ,offset
                                                       (and ,part-length (+ ,offset ,part-length)))))
                                  ,(if rendaku-p
                                      '(unrendaku part-txt)
                                      'part-txt))
                                ,pseq))
                         ,parts)
                   (when ,part-length
                     (incf ,offset ,part-length))))
         (values (nreverse ,parts) ,score)))))

(defun get-split* (reading &optional conj-of)
  (let ((split-fn (gethash (seq reading) *split-map*)))
    (if split-fn
        (funcall split-fn reading)
        (loop for seq in conj-of
           for split-fn = (gethash seq *split-map*)
           when split-fn do (return (funcall split-fn reading))))))

(defun get-split (reading &optional conj-of)
  "Includes safety check if one of split words is missing"
  (multiple-value-bind (split score) (get-split* reading conj-of)
    (when (and split (every 'identity split))
      (values split score))))

;; split definitions

;; -de expressions (need to be split otherwise -desune parses as -de sune)

#|
(:select 'kt.seq 'kt.text
                :from (:as 'kanji-text 'kt) (:as 'sense-prop 'sp)
                :where (:and (:like 'kt.text "%で")
                             (:= 'sp.seq 'kt.seq) (:= 'sp.tag "pos")
                             (:= 'sp.text "exp")))
|#

(defmacro def-de-split (seq seq-a &key (score 20))
  (let ((name (intern (format nil "~a~a" :split-de- seq))))
    `(def-simple-split ,name ,seq ,score (len)
       (,seq-a (- len 1))
       (2028980 1))))

(def-de-split 1163700 1576150) ;; 一人で

(def-de-split 1611020 1577100) ;; 何で

(def-de-split 1004800 1628530) ;; これで

(def-de-split 2810720 1004820) ;; 此れまでで

(def-de-split 1006840 1006880) ;; その上で

(def-de-split 1530610 1530600) ;; 無断で

(def-de-split 1245390 1245290) ;; 空で

(def-de-split 2719270 1445430) ;; 土足で

(def-de-split 1189420 2416780) ;; 何用で

(def-de-split 1221800 1591410) ;; 気まぐれで

(def-de-split 1263640 1630950) ;; 現行犯で

(def-de-split 1272220 1592990) ;; 交代で

(def-de-split 1311360 1311350) ;; 私費で

(def-de-split 1368500 1368490) ;; 人前で

(def-de-split 1395670 1395660) ;; 全体で

(def-de-split 1417790 1417780) ;; 単独で

(def-de-split 1454270 1454260) ;; 道理で

(def-de-split 1479100 1679020) ;; 半眼で

(def-de-split 1510140 1680900) ;; 別封で

(def-de-split 1510170 1510160) ;; 別便で

(def-de-split 1518550 1529560) ;; 無しで

(def-de-split 1528270 1528260) ;; 密室で

(def-de-split 1531420 1531410) ;; 名義で

(def-de-split 1597400 1585205) ;; 力尽くで

(def-de-split 1679990 2582460) ;; 抜き足で

(def-de-split 1682060 2085340) ;; 金ずくで

(def-de-split 1736650 1611710) ;; 水入らずで

(def-de-split 1865020 1590150) ;; 陰で

(def-de-split 1878880 2423450) ;; 差しで

(def-de-split 2126220 1802920) ;; 捩じり鉢巻きで

(def-de-split 2136520 2005870) ;; もう少しで

(def-de-split 2276140 1188520) ;; 何やかやで

(def-de-split 2513590 2513650) ;; 詰め開きで

(def-de-split 2771850 2563780) ;; 気にしないで

(def-de-split 2810800 1587590) ;; 今までで

(def-de-split 1343110 1343100) ;; ところで


(defmacro def-toori-split (seq seq-a &key (score 50) (seq-b 1432930))
  (let ((name (intern (format nil "~a~a" :split-toori- seq))))
    `(def-simple-split ,name ,seq ,score (len txt r)
       (:test (eql (word-type r) :kanji))
       (,seq-a (- len 2))
       (,seq-b 2))))

(def-toori-split 1260990 1260670) ;; 元通り

(def-toori-split 1414570 2082450) ;; 大通り

(def-toori-split 1424950 1620400) ;; 中通り [ちゅう通り]
(def-toori-split 1424960 1423310) ;; 中通り [なか通り]

(def-toori-split 1820790 1250090) ;; 型通り

(def-toori-split 1489800 1489340) ;; 表通り

(def-toori-split 1523010 1522150) ;; 本通り

(def-toori-split 1808080 1604890) ;; 目通り

(def-toori-split 1368820 1580640) ;; 人通り

(def-toori-split 1550490 1550190) ;; 裏通り

(def-toori-split 1619440 2069220) ;; 素通り

(def-toori-split 1164910 2821500 :seq-b 1432920) ;; 一通り

(def-toori-split 1462720 1461140 :seq-b 1432920) ;; 二通り

(defmacro def-do-split (seq seq-b &key (score 30) (seq-a 2252690))
  (let ((name (intern (format nil "~a~a" :split-do- seq))))
    `(def-simple-split ,name ,seq ,score (len txt r)
       (,seq-a 1)
       (,seq-b))))

(def-do-split 2142710 1185200) ;; ど下手

(def-do-split 2803190 1595630) ;; どすけべ

(def-do-split 2142680 1290210) ;; ど根性

(def-do-split 2523480 1442750) ;; ど田舎

#|
(query (:select 'kt.seq 'kt.text
                :from (:as 'kanji-text 'kt) (:as 'sense-prop 'sp)
                :where (:and (:like 'kt.text "し%")
                             (:= 'sp.seq 'kt.seq) (:= 'sp.tag "pos")
                             (:in 'sp.text (:set *pos-with-conj-rules*)))))
|#

(defmacro def-shi-split (seq seq-b &key (score 30) (seq-a '("し" 1157170)))
  (let ((name (intern (format nil "~a~a" :split-shi- seq))))
    `(def-simple-split ,name ,seq ,score (len txt r)
       (,seq-a 1)
       (,seq-b nil t))))

(def-shi-split 1005700 1156990) ;; し易い
(def-shi-split 1005830 1370760) ;; し吹く
(def-shi-split 1157200 2772730) ;; し難い
(def-shi-split 1157220 1195970) ;; し過ぎる
(def-shi-split 1157230 1284430) ;; し合う
(def-shi-split 1157280 1370090) ;; し尽す
(def-shi-split 1157310 1405800) ;; し続ける
(def-shi-split 1304890 1256520) ;; し兼ねる
(def-shi-split 1304960 1307550) ;; し始める
(def-shi-split 1305110 1338180) ;; し出す
(def-shi-split 1305280 1599390) ;; し直す
(def-shi-split 1305290 1212670) ;; し慣れる
(def-shi-split 1594300 1596510) ;; し損なう
(def-shi-split 1594310 1406680) ;; し損じる
(def-shi-split 1594460 1372620) ;; し遂げる
(def-shi-split 1594580 1277100) ;; し向ける
(def-shi-split 2518250 1332760) ;; し終える

;; nakunaru split: because naku often attaches to previous word

(def-simple-split split-nakunaru 1529550 30 (len) ;; 無くなる
  (("無く" 1529520) 2)
  (1375610 nil t))

(def-simple-split split-nakunaru2 1518540 10 (len txt r) ;; 亡くなる
  (:test (eql (word-type r) :kana))
  (("亡く" 1518450) 2)
  (1375610 nil t))

;; tegakakaru split (kana form might conflict with other uses of kakaru verb)

(def-simple-split split-tegakakaru 2089710 10 (len) ;; 手が掛かる
  (1327190 1) ;; 手
  (2028930 1) ;; が
  (1207590 nil t))


(def-simple-split split-kawaribae 1411570 10 (len txt) ;; 代わり映え
  ((1590770 1510720) (1+ (position #\り txt)))
  (1600610 2 nil t))

(def-simple-split split-hayaimonode 2815260 100 (len txt) ;; 早いもので
  (1404975 (1+ (position #\い txt)))
  (1502390 (if (find #\物 txt) 1 2))
  (2028980 1))

(def-simple-split split-dogatsukeru 2800540 30 (len) ;; ドが付ける
  (2252690 1)
  (2028930 1)
  (1495740 nil t))

(def-simple-split split-janaika 2819990 20 (len) ;; じゃないか
  (("じゃない" 2089020) 4)
  (2028970 1))

(def-simple-split split-kaasan 1609470 50 (len txt r) ;; 母さん
  (:test (eql (word-type r) :kanji))
  (1514990 1)
  (1005340 2))

(def-simple-split split-souda 1006650 5 ()
  (2137720 2)
  ((2089020 1628500)))

(def-simple-split split-kinosei 1221750 100 ()
  (1221520 1)
  (1469800 1)
  (1610040 2))

(def-simple-split split-kigatsuku 1591050 100 ()
  (1221520 1)
  (2028930 1)
  (1495740 nil t))

(def-simple-split split-nanimokamo 1599590 20 (len) ;; なにもかも
  (1188490 (- len 2))
  (2143350 2))

(def-simple-split split-katawonaraberu 2102910 20 (len txt) ;; 肩を並べる
  (1258950 (position #\を txt))
  (2029010 1)
  (1508390 nil t))

(def-simple-split split-moushiwakenasasou 2057340 300 (len txt) ;; 申し訳なさそう
  (1363050 (position #\な txt))
  (2246510))

(def-simple-split split-kimatte 1951150 50 () ;; 決まって
  (("決まって" 1591420)))

(def-simple-split split-osoreiru 1236680 100 (len txt) ;; 恐れ入る
  (1236660 (1+ (position #\れ txt)))
  (1465580 nil t))

(def-simple-split split-nantokanaru 2104540 20 (len txt) ;; なんとかなる
  (1188420 (1+ (position #\か txt)))
  (1375610 nil t))

(def-simple-split split-hajiketobu 2610760 50 (len txt) ;; 弾け飛ぶ
  (("弾け" 1419380) (1+ (position #\け txt)))
  (1429700 nil t))

(def-simple-split split-motteiku 1315700 50 (len txt) ;; 持って行く
  (("持って" 1315720) (1+ (position #\て txt)))
  (1578850 nil t))

(def-simple-split split-hairikomeru 1465460 100 (len txt r) ;; 入り込める
  (:test (eql (word-type r) :kanji))
  (("入り" 1465590) (1+ (position #\り txt)))
  (1288790 nil t))

(def-simple-split split-shinikakaru 1881080 30 () ;;死に掛かる
  (1310720 1)
  (2028990 1)
  (1207590 nil t))

(def-simple-split split-hisshininatte 1903910 50 (len txt) ;;必死になって
  (1601890 (position #\に txt))
  (2028990 1)
  (("なって" 1375610) nil))

(def-simple-split split-hitogayoi 2827409 30 (len txt) ;;人が好い
  (1580640 (position #\が txt))
  (2028930 1)
  (1605820 nil t))

(def-simple-split split-nitotte 1009600 50 (len txt) ;; にとって
  (2028990 1)
  (("取って" 1326980)))

(def-simple-split split-kotonisuru 2215340 100 (len txt) ;; 事にする
  (1313580 (position #\に txt))
  (2028990 1)
  (1157170 nil t))

(def-simple-split split-hajikidasu 1419350 100 (len txt) ;; 弾き出す
  (1901710 (1+ (position #\き txt)))
  (1338180 nil t))

(def-simple-split split-hitotachi 1368740 100 (len txt) ;; 人たち
  (1580640 (if (position #\人 txt) 1 2))
  (1416220 (if (position #\達 txt) 1 2)))

(def-simple-split split-desura 2034520 30 (len txt) ;; でさえ ですら
  (2028980 1)
  ((2827091))) ;; 1005120

(def-simple-split split-gotoni 1524660 50 (len txt) ;; ごとに
  (1524640 (position #\に txt))
  (2028990 1))

(def-simple-split split-osagari 1693800 50 () ;; お下がり
  (2826528 1)
  (1609810))

(def-simple-split split-kaisasae 1752860 50 () ;; 買い支え
  (1636070 2)
  (("支え" 1310090)))

(def-simple-split split-toiukotoda 2612990 30 (len) ;; ということだ
  (1922760 3)
  (1313580 (- len 4))
  (2089020))

(def-simple-split split-tonattara 2100770 50 (len) ;; となったら
  (1008490 1)
  (("なったら" 1375610)))

(def-simple-split split-tonaru 2100900 10 (len) ;; となる
  (1008490 1)
  (1375610 nil t))

;; SEGMENT SPLITS (allows to expand one segment into several, e.g. "ところが" "ところ+が")

(defparameter *segsplit-map* (make-hash-table)) ;; seq -> split function

(let ((*split-map* *segsplit-map*))
  (def-simple-split split-tokoroga 1008570 '(-10) (len) ;; ところが
    (1343100 (- len 1))
    (2028930 1))

  (def-simple-split split-tokorode 1343110 '(-10) (len) ;; ところで
    (1343100 (- len 1))
    (2028980 1))

  (def-simple-split split-dokoroka 2009220 '(-10) (len) ;; 所か
    (1343100 (- len 1))
    (2028970 1))

  (def-simple-split split-tokoroe 2097010 '(-10) (len) ;; ところへ
    (1343100 (- len 1))
    (2029000 1))

  (def-simple-split split-omise 2409240 '(20 :primary 1 :connector "") (len) ;; お店
    (2826528 1)
    (1582120))

  )

(defun get-segsplit (segment &aux (word (segment-word segment)))
  (when (typep word 'simple-text)
    (let ((*split-map* *segsplit-map*))
      (multiple-value-bind (split attrs) (get-split word)
        (when split
          (destructuring-bind (score &key (primary 0) (connector " ")) attrs
            (let* ((word
                    (make-instance 'compound-text
                                   :text (str:join "" (mapcar 'get-text split))
                                   :kana (str:join connector (mapcar 'get-kana split))
                                   :primary (elt split primary)
                                   :words split
                                   :score-mod score))
                   (new-seg (copy-segment segment)))
              (setf (segment-word new-seg) word
                    (segment-text new-seg) (get-text word)
                    (segment-score new-seg) (+ (segment-score segment) score))
              new-seg)))))))


;; KANA HINTS (indicate when to romanize は as わ etc.)

(defparameter *kana-hint-mod* #\u200c)
(defparameter *kana-hint-space* #\u200b)

(defparameter *hint-char-map* `(:space ,*kana-hint-space* :mod ,*kana-hint-mod*))

(defparameter *hint-simplify-map*
  (list (string *kana-hint-space*) " "
        (coerce (list *kana-hint-mod* #\は) 'string) "わ"
        (coerce (list *kana-hint-mod* #\ハ) 'string) "ワ"
        (coerce (list *kana-hint-mod* #\へ) 'string) "え"
        (coerce (list *kana-hint-mod* #\ヘ) 'string) "エ"
        (string *kana-hint-mod*) ""))

(defun process-hints (word)
  (simplify-ngrams word *hint-simplify-map*))

(defun strip-hints (word)
  (remove-if (lambda (c) (find c *hint-char-map*)) word))

(defparameter *kana-hint-map* (make-hash-table)) ;; seq -> split function

(defun insert-hints (str hints &aux (len (length str)))
  ;; hints are ((character-kw position) ...)
  (unless hints
    (return-from insert-hints str))
  (let ((positions (make-array (1+ len) :initial-element nil)))
    (loop for (character-kw position) in hints
       for char = (getf *hint-char-map* character-kw)
       when (<= 0 position len)
       do (push char (aref positions position)))
    (with-output-to-string (s)
      (loop for i from 0 upto len
         do (loop for char in (reverse (aref positions i))
               do (write-char char s))
         when (< i len)
         do (write-char (char str i) s)))))

(defparameter *hint-map* (make-hash-table)) ;; seq -> hint function

(defmacro defhint (seqs (reading-var) &body body)
  (unless (listp seqs)
    (setf seqs (list seqs)))
  (alexandria:with-gensyms (fn)
    `(let ((,fn (lambda (,reading-var) ,@body)))
       ,@(loop for seq in seqs
            collect `(setf (gethash ,seq *hint-map*) ,fn)))))

(defmacro def-simple-hint (seqs (&optional length-var kana-var reading-var) &body hints-def
                           &aux test-var test-var-used)
  (unless reading-var (setf reading-var (gensym "RV")))
  (unless length-var (setf length-var (gensym "LV")))
  (unless kana-var (setf kana-var (gensym "KV")))
  (setf test-var (gensym "TV"))
  `(defhint ,seqs (,reading-var)
     (block hint
       (let* ((,kana-var (true-kana ,reading-var))
              (,length-var (length ,kana-var))
              ,@(loop for (var value) in hints-def
                     for tvar = (cond ((eql var :test) (setf test-var-used t) test-var)
                                      ((keywordp var) nil)
                                      (t var))
                     when tvar collect `(,tvar (or ,value (return-from hint nil)))))
         (declare (ignorable ,length-var ,@(when test-var-used (list test-var))))
         (insert-hints (get-kana ,reading-var)
                       (list
                        ,@(loop for pair in hints-def
                             when (and (keywordp (car pair)) (not (eql (car pair) :test)))
                             collect `(list ,@pair))))))))

(defun get-hint (reading)
  (let ((hint-fn (gethash (seq reading) *hint-map*))
        (conj-of (mapcar #'conj-data-from (word-conj-data reading))))
    (if hint-fn
        (funcall hint-fn reading)
        (loop for seq in conj-of
           for hint-fn = (gethash seq *hint-map*)
           when hint-fn do (return (funcall hint-fn reading))))))

;; expressions ending with は

#|
(query (:select 'kt.seq 'kt.text :from (:as 'kana-text 'kt) (:as 'sense-prop 'sp)
                              :where (:and (:= 'kt.seq 'sp.seq)
                                           (:= 'sp.tag "pos")
                                           (:= 'sp.text "exp")
                                           (:like 'kt.text "%は")
                                           (:not (:in 'kt.seq (:set (alexandria:hash-table-keys *hint-map*)))))))


(:select 'kt.seq 'kt.text :from (:as 'kanji-text 'kt) (:as 'sense-prop 'sp)
                              :where (:and (:= 'kt.seq 'sp.seq)
                                           (:= 'sp.tag "pos")
                                           (:= 'sp.text "exp")
                                           (:like 'kt.text "%は%")
                                           (:not (:in 'kt.seq (:set (alexandria:hash-table-keys *hint-map*))))))
|#

;; TODO pos=int, pos=adv

(def-simple-hint
    (2028920 ;; は
     2029000 ;; へ
     )
    (l)
  (:mod (- l 1)))

(def-simple-hint ;; no space
    (1289480 ;; こんばんは
     1289400 ;; こんにちは
     1004620 ;; こにちは
     1008450 ;; では
     2215430 ;; には
     2028950 ;; とは
     )
    (l k)
  (:test (alexandria:ends-with #\は k))
  (:mod (- l 1)))

(def-simple-hint ;; with space
    (1006660 ;; そうでないばあいは
     1008500 ;; というのは
     1307530 ;; はじめは
     1320830 ;; じつは
     1324320 ;; もしくは
     1524990 ;; または
     1586850 ;; あるいは
     1586850 ;; あるは
     1877880 ;; ごきぼうのむきは
     1897510 ;; ところでは
     1907300 ;; へいそは
     1912570 ;; もとは
     2034440 ;; にかけては
     2098160 ;; なくては
     2105820 ;; にしては
     2134680 ;; それは
     2136300 ;; ということは
     2173060 ;; それいがいのものは
     2176280 ;; これは
     2177410 ;; のぞむらくは
     2177420 ;; おそらくは
     2177430 ;; おしむらくは
     2177440 ;; うらむらくは
     2177450 ;; こいねがわくは
     2256430 ;; としては
     2428890 ;; さすがは
     2513540 ;; そのことじたいは
     2523450 ;; おかれましては
     2557290 ;; なんだこれは
     2629620 ;; だけは
     2673120 ;; かくなるうえは
     2691570 ;; あいなるべくは
     2702090 ;; ては
     2717440 ;; ずは
     2717510 ;; ってのは
     2828541 ;; あさのは
     2176280 ;; これは
     1217970 ;; 希わくは
     1260080 ;; 見方によっては
     1263700 ;; 現時点においては
     1331520 ;; 就きましては
     1907290 ;; 平生は
     1914670 ;; 要は
     1950430 ;; 果ては
     2136680 ;; 無くしては
     2181810 ;; 上は
     2181730 ;; 然上は
     1217970 ;; 願わくは
     2181730 ;; 然る上は
     2576840 ;; 何時かは
     2181730 ;; しかる上は
     1331510 ;; 就いては
     1010470 ;; 延いては
     2008290 ;; さては
     2136690 ;; にあっては
     2829815 ;; まずは
     2830216 ;; じっさいは
     )
    (l k)
  (:test (alexandria:ends-with #\は k))
  (:space (- l 1))
  (:mod (- l 1)))

(def-simple-hint
    (2097010 ;; ところへ
     )
    (l)
  (:mod (- l 1)))

(def-simple-hint
    (2261800 ;; それはそれは
     )
    (l)
  (:space 2)
  (:mod 2)
  (:space 3)
  (:space (- l 1))
  (:mod (- l 1)))

(def-simple-hint ;; では/には ending
    (1009480 ;; ならでは
     1315860 ;; ときには
     1406050 ;; それでは
     1550170 ;; りろんてきには
     1917520 ;; わたくしなどには
     1917530 ;; わたくしのほうでは
     2026610 ;; からには
     2061740 ;; みたかぎりでは
     2097310 ;; ただでは
     2101020 ;; あるいみでは
     2119920 ;; そのわりには
     2134700 ;; なしには
     2200100 ;; うえでは
     2407650 ;; このぶんでは
     2553140 ;; こんにちでは
     2762790 ;; ひとつには
     1288910 ;; いまでは
     1423320 ;; なかにわ
     2099850 ;; ないことには
     1006890 ;; そのばあいには
     1887540 ;; 成功の暁には
     )
    (l k)
  (:test (alexandria:ends-with #\は k))
  (:space (- l 2))
  (:mod (- l 1)))

;; では expressions

(def-simple-hint
    (2089020 ;; だ
     2823770 ;; ではない
     2098240 ;; ではある
     2027020 ;; ではないか
     2135480 ;; ではまた
     2397760 ;; ではありますまいか
     2724540 ;; ではなかろうか
     2757720 ;; ではなさそう
     )
    (l k)
  (deha (search "では" k :from-end t))
  (:mod (1+ deha)))

(def-simple-hint ;; ends with ではない
    (1922645 ;; ふつうではない
     2027080 ;; べきではない
     2118000 ;; ひとごとではない
     2126160 ;; どうじつのだんではない
     2126140 ;; でるまくではない
     2131120 ;; たいしたことではない
     2136640 ;; といってもかごんではない
     2214830 ;; すてたものではない
     2221680 ;; いわないことではない
     2416950 ;; ばかりがのうではない
     2419210 ;; どうじつのろんではない
     2664520 ;; たまったものではない
     2682500 ;; しょうきのさたではない
     2775790 ;; それどころではない
     1343120 ;; どころではない
     1012260 ;; まんざらゆめではない
     2112270 ;; もののかずではない
     2404260 ;; しったことではない
     2523700 ;; ほんいではない
     2758400 ;; みられたものではない
     2827556 ;; だけがのうではない
     2057560 ;; わけではない
     2088970 ;; しないのではないか
     2088970 ;; ないのではないか
     2833095 ;; 吝かではない
     )
    (l k)
  (deha (search "では" k :from-end t))
  (:space deha)
  (:mod (1+ deha)))

;; では in the middle
(def-simple-hint
    (2037860 ;; ただではおかないぞ
     2694350 ;; ころんでもただではおきない
     2111220 ;; ひとすじなわではいかない
     2694360 ;; ころんでもただではおきぬ
     2182700 ;; ないではいられない
     2142010 ;; 口では大阪の城も建つ
     )
    (l k)
  (deha (search "では" k :from-end t))
  (:space deha)
  (:mod (1+ deha))
  (:space (+ 2 deha)))

;; には in the middle
(def-simple-hint
    (2057580 ;; わけにはいかない
     2067990 ;; つうにはたまらない
     2103020 ;; かわいいこにはたびをさせよ
     2105980 ;; てきをあざむくにはまずみかたから
     2152700 ;; わらうかどにはふくきたる
     2416920 ;; なくことじとうにはかてぬ
     2418030 ;; うえにはうえがある
     2792210 ;; れいにはおよばない
     2792420 ;; おれいにはおよびません
     2417920 ;; おんなのかみのけにはたいぞうもつながる
     2598720 ;; せきぜんのいえにはかならずよけいあり
     2420170 ;; きれいなばらにはとげがある
     2597190 ;; わけにはいけない
     2597800 ;; せきふぜんのいえにはかならずよおうあり
     2057570 ;; わけにはいきません
     2419360 ;; ねんにはねんをいれよ
     2121480 ;; うらにはうらがある
     2646440 ;; のこりものにはふくがある
     2740880 ;; なくことじとうにはかてない
     2416860 ;; よるとしなみにはかてない
     2156910 ;; ひとのくちにはとがたてられない
     2182690 ;; ずにはいられない looks like a suffix?
     )
    (l k)
  (niha (search "には" k :from-end t))
  (:space niha)
  (:mod (1+ niha))
  (:space (+ 2 niha)))

;; starts with には/とは

(def-simple-hint
    (2181860 ;; にはおよばない
     2037320 ;; とはいえ
     2125460 ;; とはべつに
     2128060 ;; とはいうものの
     2070730 ;; とはかぎらない
     )
    (l k)
  (:mod 1)
  (:space 2))

(def-simple-hint
    (2832044 ;; 目には目を
     )
    (l k)
  (niha (search "には" k :from-end t))
  (:space niha)
  (:mod (1+ niha))
  (:space (+ 2 niha))
  (:space (- l 1)))

;; は in the middle
(def-simple-hint
    (2101500 ;; 無い袖は振れぬ
     2118440 ;; なにはなくとも
     2118430 ;; なにはともあれ
     2152710 ;; わたるせけんにおにはない
     2216540 ;; せにはらはかえられぬ
     2757560 ;; よいごしのぜにはもたない
     2424500 ;; もくてきのためにはしゅだんをえらばない
     2078950 ;; せにはらはかえられない
     2118120 ;; なにはさておき
     2108910 ;; かたいことはいいっこなし
     2403520 ;; どうということはない
     2411180 ;; ようじんするにこしたことはない
     2419710 ;; きくとみるとはおおちがい
     2585230 ;; ことはない
     1008970 ;; どうってことはない
     2418060 ;; いろごとはしあんのほか
     2418490 ;; せけんのくちにとはたてられぬ
     2761670 ;; さきのことはわからない
     2419270 ;; にどあることはさんどある
     2600530 ;; それとはなしに
     2670900 ;; おせじにもうまいとはいえない
     2135530 ;; ほどのことはない
     2136710 ;; ということはない
     2708230 ;; なんのことはない
     1188440 ;; なんとはなしに
     2741810 ;; いいあとはわるい
     2195810 ;; にこしたことはない
     2418280 ;; ひとはなさけ
     2419410 ;; ばかとはさみはつかいよう
     2416580 ;; わるいことはできぬもの
     2417180 ;; みるときくとはおおちがい
     2418220 ;; ひとのくちにとはたてられず
     2826812 ;; わるいことはいわない
     2420010 ;; ようじんにこしたことはない
     2792090 ;; しょうきとはおもえない
     1949380 ;; むりはない
     2209690 ;; おとこににごんはない
     2210960 ;; かちめはない
     2418900 ;; ただよりたかいものはない
     2420110 ;; れいがいのないきそくはない
     2419420 ;; ばかにつけるくすりはない
     2419970 ;; よのじしょにふかのうというもじはない
     2641030 ;; あいてにとってふそくはない
     2744840 ;; ざまはない
     2747300 ;; タダめしはない
     2418900 ;; タダよりたかいものはない
     2747300 ;; ただめしはない
     2798610 ;; おみきあがらぬかみはない
     1638390 ;; ようじんするにしくはない
     2089750 ;; なにかいいてはないか
     2442180 ;; いのちにべつじょうはない
     1193090 ;; かほうはねてまて
     1238480 ;; ごうにいってはごうにしたがえ
     1338260 ;; でるくぎはうたれる
     1394290 ;; ぜんはいそげ
     1471680 ;; ばかはしななきゃなおらない
     1566340 ;; へとかじはもとからさわぐ
     1855940 ;; あいさつはぬきで
     1900670 ;; ねはおとなしい
     1981600 ;; いっすんさきはやみ
     1982230 ;; それはそうと
     1985430 ;; でるくいはうたれる
     2593830 ;; それはそうとして
     2018320 ;; ないよりはまし
     2062980 ;; ありはしない
     2078930 ;; さいはなげられた
     2083430 ;; うりのつるになすびはならぬ
     2089520 ;; それはどうも
     2089620 ;; それはさておき
     2093380 ;; てきはほんのうじにあり
     2096340 ;; せんだんはふたばよりかんばしい
     2098150 ;; なくてはならない
     2109530 ;; けいぞくはちからなり
     2111700 ;; じじつはしょうせつよりきなり
     2113730 ;; えんはいなものあじなもの
     2113740 ;; えんはいなもの
     2115570 ;; それはどうかな
     2125770 ;; そのてはくわない
     2130410 ;; すまじきものはみやづかえ
     2134480 ;; こいはもうもく
     2140350 ;; ときはかねなり
     2140990 ;; あおはあいよりいでてあいよりあおし
     2141360 ;; きはこころ
     2152850 ;; やまいはきから
     2152960 ;; こうのくすりはおつのどく
     2159970 ;; こうきしんはねこをもころす
     2159990 ;; あまいものはべつばら
     2168350 ;; ペンはけんよりもつよし
     2168360 ;; たげいはむげい
     2173880 ;; いまはむかし
     2174570 ;; しくはなし
     2176450 ;; いまはこれまで
     2176630 ;; せんきんのきゅうはいっこのえきにあらず
     2177220 ;; たからさかっているときはさかってでる
     2177240 ;; たからはわきもの
     2200690 ;; こはかすがい
     2213470 ;; それはそれで
     2255320 ;; なくてはいけない
     2275900 ;; ものはいいよう
     2408680 ;; くるものはこばまず
     2416650 ;; ころもばかりでおしょうはできぬ
     2416680 ;; いっせんをわらうものはいっせんになく
     2416870 ;; きはもちよう
     2416930 ;; さるものはおわず
     2417040 ;; きんぎんはまわりもち
     2417140 ;; ちはみずよりもこい
     2417150 ;; ちはあらそえない
     2784400 ;; かにはこうらににせてあなをほる
     2418270 ;; ひとはみかけによらぬもの
     2219580 ;; ねごとはねてからいえ
     2418250 ;; ひとはパンのみにていくるものにあらず
     2417270 ;; あとはのとなれやまとなれ
     2178680 ;; だすことはしたをだすもきらい
     2417540 ;; きのうのともはきょうのてき
     2417580 ;; さんしょうはこつぶでもぴりりとからい
     2417750 ;; まかぬたねははえぬ
     2417760 ;; しかをおうものはやまをみず
     2417980 ;; しょうじはだいじ
     2418090 ;; ねるこはそだつ
     2418100 ;; ねるほどらくはなかりけり
     2418500 ;; せけんはひろいようでせまい
     2418600 ;; せいあるものはかならずしあり
     2418700 ;; せんだんはふたばよりかんばし
     2418720 ;; ぜんしゃのくつがえるはこうしゃのいましめ
     2418800 ;; そのときはそのとき
     2418840 ;; まつみはながい
     2418920 ;; たんきはそんき
     2418970 ;; しらぬはていしゅばかりなり
     2419030 ;; ちょうしょはたんしょ
     2419060 ;; つりおとしたさかなはおおきい
     2419190 ;; にがしたさかなはおおきい
     2419260 ;; にとをおうものはいっとをもえず
     2419350 ;; としはあらそえない
     2419390 ;; うまはうまづれ
     2419450 ;; いたごいちまいしたはじごく
     2419530 ;; ふうふげんかはいぬもくわない
     2419590 ;; ものはかんがえよう
     2419600 ;; ものはつかいよう
     2419610 ;; ものはためし
     2419620 ;; ものはそうだん
     2419690 ;; ぶんはひとなり
     2419730 ;; たよりのないのはよいたより
     2419800 ;; なのないほしはよいからでる
     2420120 ;; れきしはくりかえす
     2420180 ;; もちはもちや
     2420190 ;; おごるへいけはひさしからず
     2570900 ;; わらでたばねてもおとこはおとこ
     2580730 ;; ちはちからなり
     2582770 ;; きおうはとがめず
     2583560 ;; きってはきれない
     2593830 ;; それはそうとして
     2618920 ;; いきみはしにみ
     2618990 ;; むちはこうふく
     2694370 ;; ころんでもただはおきない
     2716900 ;; そうはいかない
     2737650 ;; ゆだんはきんもつ
     2738830 ;; おきゃくさまはかみさまです
     2758920 ;; たっているものはおやでもつかえ
     2776820 ;; りくつとこうやくはどこへでもつく
     2783090 ;; うたがわしきはばっせず
     2789240 ;; うそとぼうずのあたまはゆったことがない
     2797740 ;; いちえんをわらうものはいちえんになく
     2827424 ;; きはさらさらない
     2827754 ;; おとこはだまって
     2833986 ;; あくにつよいはぜんにもつよい
     2833961 ;; 梅は食うとも核食うな中に天神寝てござる
     2833957 ;; 老兵は死なず只消え去るのみ
     2833956 ;; 山より大きな猪は出ぬ
     2833597 ;; 君子は豹変す
     2831359 ;; 無くてはいけません
     2830029 ;; 明けない夜は無い
     2828308 ;; 吐いた唾は飲めぬ
     2419120 ;; 転んでもただは起きぬ
     2111710 ;; 事実は小説よりも奇なり
     )
    (l k)
  (ha (search "は" k :from-end t))
  (:space ha)
  (:mod ha)
  (:space (1+ ha)))

(def-simple-hint
    (2108440 ;; あやまちてはすなわちあらたむるにはばかることなかれ
     2417420 ;; ふりかかるひのこははらわねばならぬ
     )
    (l k)
  (ha (search "は" k))
  (:space ha)
  (:mod ha)
  (:space (1+ ha)))

;; ha twice in the middle
(def-simple-hint
    (2416600 ;; 悪人は畳の上では死ねない
     2767400 ;; おにはそとふくはうち
     2418260 ;; ひとはいちだいなはまつだい
     2828341 ;; はなはさくらぎひとはぶし
     2086560 ;; つるはせんねんかめはまんねん
     2152790 ;; らくはくのたねくはらくのたね
     2154700 ;; たびはみちづれよはなさけ
     2158840 ;; おとこはどきょうおんなはあいきょう
     2168380 ;; ちんもくはきんゆうべんはぎん
     2417120 ;; げいじゅつはながくじんせいはみじかし
     2417230 ;; 言うは易く行うは難し
     2417500 ;; 今日は人の身明日は我が身
     2417930 ;; 女は弱しされど母は強し
     2418180 ;; 親は無くても子は育つ
     2418550 ;; せいはかたくしはやすし
     2418630 ;; こえはすれどもすがたはみえず
     2418650 ;; 昔は昔今は今
     2418740 ;; 創業は易く守成は難し
     2419150 ;; 東は東西は西
     2419950 ;; 雄弁は銀沈黙は金
     2419960 ;; 夕焼けは晴れ朝焼けは雨
     2420080 ;; 旅は心世は情け
     2424520 ;; 去る者は追わず来たる者は拒まず
     2558710 ;; 遠きは花の香近きは糞の香
     2719710 ;; フグは食いたし命は惜しし
     2790690 ;; 弓は袋に太刀は鞘
     2828900 ;; 山中の賊を破るは易く心中の賊を破るは難し
     2833976 ;; 君子は周して比せず小人は比して周せず
     2833959 ;; 知る者は言わず言う者は知らず
     2833900 ;; 虎は死して皮を留め人は死して名を残す
     2570040 ;; 朝焼けは雨夕焼けは晴れ
     2424520 ;; 去る者は追わず、来たる者は拒まず
     2419570 ;; 腹が減っては戦は出来ぬ
     2255410 ;; 浜の真砂は尽きるとも世に盗人の種は尽きまじ
     )
    (l k)
  (ha1 (search "は" k :start2 1))
  (ha2 (search "は" k :from-end t))
  (:space ha1)
  (:mod ha1)
  (:space (1+ ha1))
  (:space ha2)
  (:mod ha2)
  (:space (1+ ha2)))

(def-simple-hint
    (2419720 ;; 聞くは一時の恥聞かぬは末代の恥
     2419910 ;; 問うは一旦の恥問わぬは末代の恥
     2757120 ;; 問うは一度の恥、問わぬは末代の恥
     )
    (l k)
  (ha1 (search "は" k))
  (nuha (search "ぬは" k :from-end t))
  (:space ha1)
  (:mod ha1)
  (:space (1+ ha1))
  (:space (1+ nuha))
  (:mod (1+ nuha))
  (:space (+ 2 nuha)))

(def-simple-hint
    (2153170 ;; めにはめをはにははを
     )
    (l k)
  (niha1 (search "には" k))
  (niha2 (search "には" k :from-end t))
  (wo1 (search "を" k))
  (wo2 (search "を" k :from-end t))
  (:space niha1)
  (:mod (1+ niha1))
  (:space (+ 2 niha1))
  (:space niha2)
  (:mod (1+ niha2))
  (:space (+ 2 niha2))
  (:space wo1)
  (:space (1+ wo1))
  (:space wo2))

(def-simple-hint
    (2422970 ;; 人には添うて見よ馬には乗って見よ
     2833500 ;; 馬には乗って見よ人には添うて見よ
     )
    (l k)
  (niha1 (search "には" k))
  (niha2 (search "には" k :from-end t))
  (yo1 (search "よ" k))
  (:space niha1)
  (:mod (1+ niha1))
  (:space (+ 2 niha1))
  (:space niha2)
  (:mod (1+ niha2))
  (:space (+ 2 niha2))
  (:space (1+ yo1)))

(def-simple-hint
    (1008660 ;; 隣の芝生は青い
     1204760 ;; 蛙の子は蛙
     2113380 ;; 金は天下の回り物
     2141020 ;; 秋の日は釣瓶落とし
     2144050 ;; 秋の鹿は笛に寄る
     2152870 ;; 柳の下にいつも泥鰌はおらぬ
     2152930 ;; 一年の計は元旦にあり
     2158240 ;; 若い時の苦労は買うてもせよ
     2202070 ;; 勝負は時の運
     2227110 ;; カエサルのものはカエサルに
     2417800 ;; 蛇の道は蛇
     2418170 ;; 親の光は七光り
     2420070 ;; 旅の恥は掻き捨て
     2420100 ;; 隣の花は赤い
     2582990 ;; 狐の子は頬白
     2697510 ;; 君父の讐は倶に天を戴かず
     2827732 ;; 若い時の苦労は買ってもせ
     )
    (l k)
  (no (search "の" k))
  (ha (search "は" k :from-end t))
  (:space no)
  (:space (1+ no))
  (:space ha)
  (:mod ha)
  (:space (1+ ha)))

(def-simple-hint
    (1487700 ;; 必要は発明の母
     1320150 ;; 失敗は成功のもと
     2081270 ;; 教うるは学ぶの半ばなり
     2126750 ;; 悪妻は百年の不作
     2141010 ;; 逢うは別れの始め
     2144040 ;; 商いは牛のよだれ
     2152780 ;; 苦は楽の種
     2211780 ;; 情けは人の為ならず
     2416720 ;; 嘘つきは泥棒の始まり
     2416970 ;; 教うるは学ぶの半ば
     2417350 ;; 口は禍の元
     2417610 ;; 子は三界の首枷
     2417810 ;; 弱き者よ汝の名は女也
     2418340 ;; 人間は万物の霊長
     2418470 ;; 据え膳食わぬは男の恥
     2418540 ;; 正直は一生の宝
     2418590 ;; 生兵法は大怪我のもと
     2419020 ;; 朝起きは三文の徳
     2420140 ;; 恋は思案の外
     2550210 ;; 幸運の女神は前髪しかない
     2591070 ;; 火事と喧嘩は江戸の華
     2716860 ;; そうはイカの金玉
     2796370 ;; 禍福は糾える縄のごとし
     2833968 ;; 人間は万物の尺度である
     2833958 ;; 言葉は身の文
     2832652 ;; 挨拶は時の氏神
     2111130 ;; 早起きは三文の徳
     2417830 ;; 酒は百薬の長
     )
    (l k)
  (no (search "の" k :from-end t))
  (ha (search "は" k))
  (:space no)
  (:space (1+ no))
  (:space ha)
  (:mod ha)
  (:space (1+ ha)))

(def-simple-hint
    (1213500 ;; 甘言は偶人を喜ばす
     1470130 ;; 能ある鷹は爪を隠す
     1929200 ;; 悪貨は良貨を駆逐する
     2077530 ;; 類は友を呼ぶ
     2079030 ;; 大は小を兼ねる
     2089460 ;; 鳴く猫は鼠を捕らぬ
     2102600 ;; おぼれる者はわらをもつかむ
     2168340 ;; 天は自ら助くる者を助く
     2416900 ;; 急いては事を仕損ずる
     2417110 ;; 芸は身を助く
     2419100 ;; 天は二物を与えず
     2419810 ;; 名は体を表す
     2520680 ;; 義を見てせざるは勇なきなり
     2627320 ;; 急いては事を仕損じる
     2686140 ;; 大人は赤子の心を失わず
     2833952 ;; 足るを知る者は富む
     2832631 ;; 井蛙は以って海を語る可からず
     2832604 ;; 良禽は木を択んで棲む
     2757650 ;; 日光を見ない中は結構と言うな
     2419440 ;; 敗軍の将は兵を語らず
     )
    (l k)
  (ha (search "は" k :start2 1))
  (wo (search "を" k :from-end t))
  (:space ha)
  (:mod ha)
  (:space (1+ ha))
  (:space wo)
  (:space (1+ wo)))

(def-simple-hint
    (2095170 ;; 天才と狂人は紙一重
     2237240 ;; 女房と畳は新しい方がいい
     )
    (l k)
  (to (search "と" k))
  (ha (search "は" k :from-end t))
  (:space to)
  (:space (1+ to))
  (:space ha)
  (:mod ha)
  (:space (1+ ha)))

(def-simple-hint
    (2124980 ;; そうは問屋が卸さない
     2395080 ;; すぎたるはおよばざるがごとし
     2395090 ;; すぎたるはなおおよばざるがごとし
     2417400 ;; 慌てる乞食はもらいが少ない
     2419860 ;; 明日は明日の風が吹く
     )
    (l k)
  (ha (search "は" k))
  (ga (search "が" k :from-end t))
  (:space ha)
  (:mod ha)
  (:space (1+ ha))
  (:space ga)
  (:space (1+ ga)))

(def-simple-hint
    (2138600 ;; 百聞は一見にしかず
     2153120 ;; 良薬は口に苦し
     2153130 ;; 目は口ほどに物を言う
     2168390 ;; 鉄は熱いうちに打て
     2171910 ;; 去る者は日々に疎し
     2416560 ;; ローマは一日にして成らず
     2416730 ;; 運は天に在り
     2416800 ;; 火のないところに煙は立たない
     2417160 ;; 健全なる精神は健全なる身体に宿る
     2417390 ;; 孝行をしたい時分に親は無し
     2418120 ;; 新しい酒は古い革袋に入れる
     2418130 ;; 深い川は静かに流れる
     2418450 ;; 水は低きに流る
     2418750 ;; すべての道はローマに通ず
     2419080 ;; 鉄は熱いうちに鍛えよ
     2420150 ;; 老いては子に従え
     2566010 ;; 秋茄子は嫁に食わすな
     2832573 ;; 巧詐は拙誠に如かず
     2704850 ;; 花泥棒は罪にならない
     )
    (l k)
  (ha (search "は" k))
  (ni (search "に" k :from-end t))
  (:space ha)
  (:mod ha)
  (:space (1+ ha))
  (:space ni)
  (:space (1+ ni)))

(def-simple-hint
    (2418150 ;; 親に似ぬ子は鬼子
     2418640 ;; 静かに流れる川は深い
     2419940 ;; 柳の下に何時も泥鰌は居ない
     2830412 ;; 他に方法は無い
     2666530 ;; 墓に布団は着せられぬ
     )
    (l k)
  (ha (search "は" k :from-end t))
  (ni (search "に" k))
  (:space ha)
  (:mod ha)
  (:space (1+ ha))
  (:space ni)
  (:space (1+ ni)))

(def-simple-hint
    (2832738 ;; 身体髪膚これを父母に受くあえて毀傷せざるは孝の始なり
     )
    (l k)
  (ruha (search "るは" k :from-end t))
  (no (search "の" k :from-end t))
  (wo (search "を" k))
  (ni (search "に" k))
  (:space wo)
  (:space (1+ wo))
  (:space ni)
  (:space (1+ ni))
  (:space (1+ ruha))
  (:mod (1+ ruha))
  (:space (+ 2 ruha))
  (:space no)
  (:space (1+ no))
  )
