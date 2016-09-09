(in-package :ichiran/dict)

(defun sql-eql-or-null (cell value)
  (if (equal value :null)
      `(:is-null ,cell)
      `(:= ,cell ,value)))

(defun find-conj (seq-from options)
  (destructuring-bind (conj-type pos neg fml) options
    (query 
     (sql-compile
      `(:select conj.id 
                :from (:as conjugation conj)
                :inner-join (:as conj-prop prop)
                :on (:= prop.conj-id conj.id)
                :where (:and (:= conj.from ,seq-from)
                             (:= prop.conj-type ,conj-type)
                             (:= prop.pos ,pos)
                             ,(sql-eql-or-null 'prop.neg neg)
                             ,(sql-eql-or-null 'prop.fml fml))))
     :column)))
  
(defun add-conj (seq-from options reading-map)
  (unless (find-conj seq-from options)
    (destructuring-bind (conj-type pos neg fml) options
      (let ((next-seq (1+ (query (:select (:max 'seq) :from 'entry) :single))))
        (make-dao 'entry :seq next-seq :content "")
        (loop with ord-r = 0 and ord-k = 0
           for (src-reading reading) in reading-map
           for is-kana = (test-word reading :kana)
           for table = (if is-kana 'kana-text 'kanji-text)
           for ord = (if is-kana ord-r ord-k)
           do (make-dao table :seq next-seq :text reading :ord ord :common :null)
             (if is-kana (incf ord-r) (incf ord-k)))
        (let ((conj (make-dao 'conjugation :seq next-seq :from seq-from)))
          (make-dao 'conj-prop :conj-id (id conj)
                    :pos pos :conj-type conj-type :neg neg :fml fml)
          (loop for (src-reading reading) in reading-map
               do (make-dao 'conj-source-reading :conj-id (id conj)
                            :text reading
                            :source-text src-reading)))))))

(defun add-reading (seq reading &key (common :null) (conjugate-p t))
  (let* ((is-kana (test-word reading :kana))
         (table (if is-kana 'kana-text 'kanji-text))
         (entry (get-dao 'entry seq)))
    (when (not (select-dao table (:and (:= 'seq seq) (:= 'text reading))))
      (let ((ord (1+ (query (:select (:max 'ord) :from table :where (:= 'seq seq)) :single))))
        (make-dao table :text reading :seq seq :ord ord :common common :conjugate-p conjugate-p)
        (if is-kana
            (incf (n-kana entry))
            (incf (n-kanji entry)))
        (update-dao entry)))
    entry))

(defun delete-reading (seq reading)
  (let* ((is-kana (test-word reading :kana))
         (table (if is-kana 'kana-text 'kanji-text))
         (entry (get-dao 'entry seq))
         (to-delete (select-dao table (:and (:= 'seq seq) (:= 'text reading))))
         (deleted 0))
    (when to-delete
      (dolist (obj to-delete)
        (delete-dao obj)
        (incf deleted))
      (if is-kana
          (decf (n-kana entry) deleted)
          (decf (n-kanji entry) deleted))
      (update-dao entry)
      (loop for obj in (select-dao table (:= 'seq seq) 'ord)
            for ord from 0
            do (setf (slot-value obj 'ord) ord) (update-dao obj)))))

(defun delete-senses (seq prop-test)
  (let* ((sense-props (remove-if-not prop-test (select-dao 'sense-prop (:= 'seq seq))))
         (sense-ids (remove-duplicates (mapcar #'sense-id sense-props))))
    (query (:delete-from 'sense-prop :where (:in 'sense-id (:set sense-ids))))
    (query (:delete-from 'gloss :where (:in 'sense-id (:set sense-ids))))
    (query (:delete-from 'sense :where (:in 'id (:set sense-ids))))))

(defun delete-sense-prop (seq tag text)
  (let ((props (select-dao 'sense-prop (:and (:= 'seq seq) (:= 'tag tag) (:= 'text text)))))
    (mapc #'delete-dao props)))

(defun add-sense-prop (seq sense-ord tag text)
  (let ((sense (car (select-dao 'sense (:and (:= 'seq seq) (:= 'ord sense-ord))))))
    (when sense
      (unless (select-dao 'sense-prop (:and (:= 'sense-id (id sense)) (:= 'tag tag) (:= 'text text)))
        (make-dao 'sense-prop :sense-id (id sense) :tag tag :text text :ord 0 :seq seq)))))

(defun add-sense (seq ord &rest glosses)
  (unless (select-dao 'sense (:and (:= 'seq seq) (:= 'ord ord)))
    (let ((sense-id (id (make-dao 'sense :seq seq :ord ord))))
      (loop for gord from 0
           for gloss in glosses
           do (make-dao 'gloss :sense-id sense-id :text gloss :ord gord)))))

(defun set-common (table seq text common)
  (loop for kt in (select-dao table (:and (:= 'seq seq) (:= 'text text)))
     do (setf (slot-value kt 'common) common)
       (update-dao kt)))

(defun add-deha-ja-readings ()
  (let ((deha-list (query (:select 'conj.seq 'kt.text :distinct
                                   :from (:as 'conjugation 'conj) (:as 'kana-text 'kt)
                                   :where (:and (:= 'conj.from 2089020)
                                                (:= 'kt.seq 'conj.seq)
                                                (:like 'kt.text "では%"))))))
    (loop for (seq deha) in deha-list
         for ja = (concatenate 'string "じゃ" (subseq deha 2))
         do (add-reading seq ja)))

  (let ((deha-src-reading (query (:select 'csr.conj-id 'csr.text 'csr.source-text
                                          :from (:as 'conjugation 'conj) (:as 'conj-source-reading 'csr)
                                          :where (:and (:= 'conj.from 2089020)
                                                       (:= 'csr.conj-id 'conj.id)
                                                       (:like 'csr.text "では%"))))))
    (loop for (conj-id text source-text) in deha-src-reading
         for ja = (concatenate 'string "じゃ" (subseq text 2))
         for jsr = (select-dao 'conj-source-reading (:and (:= 'conj-id conj-id) (:= 'text ja) (:= 'source-text source-text)))
         unless jsr
         do (make-dao 'conj-source-reading :conj-id conj-id
                      :text ja
                      :source-text (if (alexandria:starts-with-subseq "では" source-text)
                                       (concatenate 'string "じゃ" (subseq source-text 2))
                                       source-text)))))

(defun delete-conjugation (seq from &optional (via :null))
  (let ((conj (query-dao 'conjugation 
                         (sql-compile
                          `(:select * :from conjugation
                                    :where (:and (:= seq ,seq)
                                                 (:= from ,from)
                                                 ,(sql-eql-or-null 'via via))))))
        (entry (get-dao 'entry seq)))
    (when conj
      (let* ((conj-ids (mapcar #'id conj))
             (delete-entry (not (or (root-p entry)
                                    (select-dao 'conjugation (:and (:= 'seq seq)
                                                                   (:not (:in 'id (:set conj-ids))))))))
             )
        (query (:delete-from 'conj-prop :where (:in 'conj-id (:set conj-ids))))
        (query (:delete-from 'conj-source-reading :where (:in 'conj-id (:set conj-ids))))
        (query (:delete-from 'conjugation :where (:in 'id (:set conj-ids))))
        (when delete-entry
          (delete-dao entry))))))

(defun remove-hiragana-nokanji ()
  (loop with kts = (remove-if-not (lambda (kt) (test-word (text kt) :hiragana))
                                 (select-dao 'kana-text 'nokanji))
     for entry in (select-dao 'entry (:in 'seq (:set (mapcar #'seq kts))))
     do (setf (slot-value entry 'primary-nokanji) nil)
       (update-dao entry)
       (print entry)))

(defun set-primary-nokanji (seq value)
  (let ((entry (get-dao 'entry seq)))
    (setf (slot-value entry 'primary-nokanji) value)
    (update-dao entry)))

(defun rearrange-readings (seq table prefix)
  "Rearrange readings with prefix before the rest of them"
  (loop
     with offset = (query (:select (:count 'id) :from table
                                   :where (:and (:= 'seq seq) (:like 'text (:|| prefix "%"))))
                          :single)
     with cnt1 = -1 and cnt2 = (1- offset)
     for kt in (select-dao table (:= 'seq seq) 'ord)
     for new-ord = (if (alexandria:starts-with-subseq prefix (text kt))
                       (incf cnt1) (incf cnt2))
     do (setf (slot-value kt 'ord) new-ord) (update-dao kt)))

(defun rearrange-readings-conj (seq table prefix)
  (rearrange-readings seq table prefix)
  (dolist (seq (query (:select 'seq :distinct :from 'conjugation :where (:= 'from seq)) :column))
    (rearrange-readings seq table prefix)))

(defmacro do-readings (table seq text (rvar) &body body)
  "text can be nil for all readings from table"
  `(dolist (,rvar (select-dao ,table (:and (:= 'seq ,seq) ,@(when text `((:= 'text ,text))))))
     ,@body))

(defun add-primary-nokanji (seq reading)
  (set-primary-nokanji seq t)
  (do-readings 'kana-text seq reading (kt)
    (setf (slot-value kt 'nokanji) t)
    (update-dao kt)))

(defun add-errata ()
  (add-deha-ja-readings)
  (remove-hiragana-nokanji)

  (set-primary-nokanji 1538900 nil) ;; ただ
  (set-primary-nokanji 1580640 nil) ;; 人

  (add-primary-nokanji 1415510 "タカ")

  ;;; add sense for な 
  (add-sense 2029110 4 "(used with nouns) な-adjective")
  ;;; gozaimashita / gozaimashitara
  (add-conj 1612690 '(2 "exp" :null :null)
            '(("ございます" "ございました")))
  (add-conj 1612690 '(11 "exp" :null :null)
            '(("ございます" "ございましたら")))
  (add-conj 1612690 '(1 "exp" t :null)
            '(("ございます" "ございません")))

  ;; きみ / キミ
  (add-reading 1247250 "キミ")
  (add-reading 2015370 "ワシ")
  (add-reading 1202410 "カニ")
  (add-reading 1521960 "ボツ")
  (add-reading 2145800 "イラ")
  (add-reading 1517840 "ハチ")
   
  (add-reading 2029080 "ねぇ")
  (add-reading 2089020 "じゃ" :common 0)

  (delete-reading 2145800 "いら")

  (delete-reading 2067160 "たも")

  (delete-reading 2423450 "サシ")
  (delete-reading 2574600 "どうなん")
  
  ;;; delete sense-prop uk for 生る
  (delete-sense-prop 1611000 "misc" "uk")
  ;; 仕手 (して) 
  (delete-sense-prop 1305070 "misc" "uk")
  ;; 品 ( しな)
  (delete-sense-prop 1583470 "misc" "uk")
  ;; しな
  (delete-sense-prop 1446760 "misc" "uk")
  ;; だし
  (delete-sense-prop 1302910 "misc" "uk")
  ;; う
  (delete-sense-prop 2802220 "misc" "uk")
  ;; もち
  (delete-sense-prop 1535790 "misc" "uk")
  ;; なんだ
  (delete-sense-prop  2119750 "misc" "uk")
  ;; つ
  (delete-sense-prop 2220330 "misc" "uk")
  ;; かけ
  (delete-sense-prop 1207600 "misc" "uk")
  ;; かく
  (delete-sense-prop 1399970 "misc" "uk")
  ;; らい
  (delete-sense-prop 2094480 "misc" "uk")
  ;; いる
  (delete-sense-prop 2729170 "misc" "uk")
  ;; そば
  (delete-sense-prop 1238460 "misc" "uk")
  ;; 人
  (delete-sense-prop 1580640 "misc" "uk")
  ;; かし
  (delete-sense-prop 1569440 "misc" "uk") 
  ;; さし
  (delete-sense-prop 2423450 "misc" "uk")
  ;; 行く
  (delete-sense-prop 1578850 "misc" "uk")
  ;; 罹る
  (delete-sense-prop 1609500 "misc" "uk")
  ;; 吐く
  (delete-sense-prop 1444150 "misc" "uk")
  ;; 要る
  (delete-sense-prop 1546640 "misc" "uk")
  ;; ことなく
  (delete-sense-prop 1314490 "misc" "uk")
  ;; やす
  (delete-sense-prop 2643710 "misc" "uk")
  ;;  はねる
  (delete-sense-prop 1611260 "misc" "uk")
  ;; かける
  (delete-sense-prop 2208960 "misc" "uk")
  ;; もって
  (delete-sense-prop 1155020 "misc" "uk")
  ;; かっこ
  (delete-sense-prop 1208240 "misc" "uk")
  ;; かかる
  (delete-sense-prop 1207590 "misc" "uk")
  ;; かまう
  (delete-sense-prop 1279680 "misc" "uk")
  ;; ないし
  (delete-sense-prop 1469810 "misc" "uk")
  ;; むく
  ;; regrettably, but volitional form clashes with 向こう
  (delete-sense-prop 1474370 "misc" "uk")
  ;; うたう
  (delete-sense-prop 1609300 "misc" "uk")
  ;; ひく
  (delete-sense-prop 1612920 "misc" "uk")
  ;; まめ
  (delete-sense-prop 2827450 "misc" "uk")
  ;; たかる
  (delete-sense-prop 1333570 "misc" "uk")
  
  ;; こころ
  ;; (add-sense-prop 1360480 0 "misc" "uk")
  ;; そういう
  (add-sense-prop 1394680 0 "misc" "uk")
  ;; すごく
  (add-sense-prop 2272830 0 "misc" "uk")
  ;; ごめんなさい
  (add-sense-prop 1270680 0 "misc" "uk")
  ;; ありがたい
  (add-sense-prop 1541560 0 "misc" "uk")
  ;; わけない
  (add-sense-prop 1739410 1 "misc" "uk")
  ;; かける
  (add-sense-prop 1207610 0 "misc" "uk")
  ;; やつめ
  (add-sense-prop 2424410 0 "misc" "uk")
  
  ;; なの
  (add-sense-prop 2425930 0 "pos" "prt")
  ;; わね
  (add-sense-prop 2457930 0 "pos" "prt")
  
  ;; set/unset common flag for choice kana readings
  (set-common 'kana-text 1310920 "したい" :null)
  (set-common 'kana-text 1159430 "いたい" :null)
  (set-common 'kana-text 1523060 "ほんと" 2)
  (set-common 'kana-text 1577100 "なん" 2)
  (set-common 'kana-text 1012440 "めく" :null)
  (set-common 'kana-text 1005600 "しまった" :null)
  (set-common 'kana-text 2139720 "ん" 0)
  (set-common 'kana-text 1309910 "してい" 0)
  (set-common 'kana-text 1311320 "してい" 0)
  (set-common 'kana-text 1423310 "なか" 1)
  (set-common 'kanji-text 1245280 "空" 0)
  (set-common 'kana-text 1308640 "しない" 0)
  (set-common 'kana-text 1579130 "ことし" 0)
  (set-common 'kana-text 2084660 "いなくなった" 0)
  (set-common 'kana-text 1570850 "すね" :null)
  (set-common 'kana-text 1470740 "のうち" 0)
  (set-common 'kana-text 1156100 "いいん" 0)
  (set-common 'kana-text 1472520 "はいいん" :null)
  (set-common 'kana-text 1445000 "としん" 0)
  (set-common 'kana-text 1408100 "たよう" 0)
  (set-common 'kana-text 2409180 "ような" 0)
  (set-common 'kana-text 1524550 "まいそう" :null)
  (set-common 'kana-text 1925750 "そうする" :null)
  (set-common 'kana-text 1587780 "いる" :null)
  (set-common 'kana-text 1322180 "いる" :null)
  (set-common 'kana-text 1391500 "いる" :null)
  (set-common 'kanji-text 1606560 "分かる" 11)
  (set-common 'kana-text 1606560 "わかる" 11)
  (set-common 'kanji-text 1547720 "来る" 11)
  (set-common 'kana-text 1547720 "くる" 11)
  (set-common 'kana-text 2134680 "それは" 0)
  (set-common 'kana-text 2134680 "そりゃ" 0)
  (set-common 'kana-text 1409140 "からだ" 0)
  (set-common 'kana-text 1552120 "ながす" :null)
  (set-common 'kana-text 1516930 "ほう" 1)
  (set-common 'kana-text 1518220 "ほうが" :null)
  (set-common 'kana-text 1603340 "ほうが" :null)
  (set-common 'kana-text 1158400 "いどう" :null)
  (set-common 'kana-text 1157970 "いどう" :null)
  (set-common 'kana-text 1599900 "になう" :null)
  (set-common 'kana-text 1465590 "はいる" :null)
  (set-common 'kana-text 1535930 "とい" 0)
  (set-common 'kana-text 1472480 "はいらん" :null)
  (set-common 'kanji-text 2019640 "杯" 20)
  (set-common 'kana-text 1416220 "たち" 10)
  (set-common 'kana-text 1402900 "そうなん" :null)
  (set-common 'kana-text 1446980 "いたむ" :null)
  (set-common 'kana-text 1432710 "いたむ" :null)
  (set-common 'kana-text 1632670 "かむ" :null)
  (set-common 'kana-text 1224090 "きが" 40)
  (set-common 'kana-text 1534470 "もうこ" :null)
  (set-common 'kana-text 1739410 "わけない" 0)
  (set-common 'kanji-text 1416860 "誰も" 0)
  (set-common 'kana-text 2093030 "そっか" 0)
  (set-common 'kanji-text 1001840 "お兄ちゃん" 0)
  (set-common 'kanji-text 1341350 "旬" 0)
  (set-common 'kana-text 1188790 "いつか" 0)
  (set-common 'kana-text 1582900 "もす" :null)
  (set-common 'kana-text 1577270 "セリフ" 0)
  (set-common 'kana-text 1375650 "せいか" 0)
  (set-common 'kanji-text 1363540 "真逆" :null)
  (set-common 'kana-text 1632200 "どうか" 0)
  (set-common 'kanji-text 1920245 "何の" 0)
  (set-common 'kana-text 2733410 "だよね" 0)
  (set-common 'kana-text 1234260 "ともに" 0)
  (set-common 'kanji-text 2242840 "未" 0)
  (set-common 'kana-text 1246890 "リス" 0)
  (set-common 'kana-text 1257270 "やらしい" 0)
  (set-common 'kana-text 1343100 "とこ" 0)
  (set-common 'kana-text 1529930 "むこう" 14)
  (set-common 'kanji-text 1317910 "自重" 30)
  (set-common 'kana-text 1586420 "あったかい" 0)
  (set-common 'kana-text 1214190 "かんない" :null)
  (set-common 'kana-text 1614320 "かんない" :null)
  (set-common 'kana-text 1517220 "ほうがい" :null)
  (set-common 'kana-text 1380990 "せいなん" :null)
  (set-common 'kana-text 1280630 "こうなん" :null)
  (set-common 'kana-text 1289620 "こんなん" :null)

  (set-common 'kana-text 2827401 "ほうがいい" 0)
  (set-common 'kanji-text 2827401 "方がいい" 0)
  (set-common 'kana-text 2457920 "ですか" :null)
  (set-common 'kana-text 1228390 "すいもの" :null)
  (set-common 'kana-text 1423240 "きもの" 0)
  (set-common 'kana-text 1212110 "かんじ" 0)
  (set-common 'kana-text 1516160 "たから" 0)
  (set-common 'kana-text 1575510 "コマ" 0)
  (set-common 'kanji-text 1603990 "街" 0)
  (set-common 'kana-text 1548520 "からむ" :null)
  (set-common 'kana-text 2174250 "もしや" 0)
  (set-common 'kana-text 1595080 "のく" :null)
  (set-common 'kana-text 1309950 "しどう" 0)
  (set-common 'kana-text 1524860 "まくら" 9)
  (set-common 'kanji-text 1451770 "同じよう" 30)
  (set-common 'kana-text 1244210 "くない" 0)
  (set-common 'kana-text 1898260 "どうし" 11)
  (set-common 'kanji-text 1407980 "多分" 1)
  (set-common 'kana-text 1579630 "なのか" :null)
  (set-common 'kana-text 1371880 "すいてき" :null)
  (set-common 'kana-text 1008420 "でしょ" 0)
  (set-common 'kana-text 1928670 "だろ" 0)
  (set-common 'kanji-text 1000580 "彼" :null)
  
  ;; remove sense for なり and make it not root
  (delete-senses 2611370 (constantly t))
  (let ((entry (get-dao 'entry 2611370)))
    (setf (slot-value entry 'root-p) nil)
    (update-dao entry))
  (delete-reading 2611370 "為り")

  ;; 包む is read as tsutsumu
  (rearrange-readings-conj 1584060 'kana-text "つつ")
  (set-common 'kana-text 1584060 "つつむ" 6)

  ;; delete noun sense for と
  (delete-senses 1008490 (lambda (prop) (and (equal (text prop) "n") (equal (tag prop) "pos"))))

  ;; delete prt sense for たい
  (delete-senses 2017560 (lambda (prop) (and (equal (text prop) "prt") (equal (tag prop) "pos"))))

  ;; delete adj stem conjugation for ない
  (delete-conjugation 2029110 2257550)
  (delete-conjugation 2086640 2684620) ;; しい

  )

(defparameter *skip-words* '(2458040  ;; てもいい
                             2822120  ;; ても良い
                             2013800  ;; ちゃう
                             2108590  ;; とく
                             2029040  ;; ば
                             2428180  ;; い
                             2654250 ;; た
                             2084010 ;; になる
                             2561100 ;; うまいな
                             2210270 ;; ませんか
                             1006610 ;; そう
                             2257550 ;; ない
                             2210320 ;; ません
                             2017560 ;; たい
                             2394890 ;; とる
                             2194000 ;; であ
                             )
  "seq of words that aren't really words, like suffixes etc."
  )

(defparameter *final-prt* '(2017770 ;; かい
                            1008450 ;; では
                            2425930 ;; なの
                            ;; 2780660 ;; もの
                            2130430 ;; け っけ
                            )
  "Words that only have meaning when they're final")

(defparameter *semi-final-prt* (append *final-prt* 
                                       '(2029120 ;; さ
                                         2086640 ;; し
                                         2029110 ;; な
                                         2029080 ;; ね
                                         2029100 ;; わ
                                         ))
  "Particles that are final, but also have other uses")

(defparameter *non-final-prt*
  '(2139720 ;; ん
    )
  "Particles that don't get final bonus")

;; Additional conjugations

(defconstant +conj-adverbial+ 50)
(defconstant +conj-adjective-stem+ 51)

(defun errata-conj-description-hook (hash)
  (setf (gethash +conj-adverbial+ hash) "Adverbial")
  (setf (gethash +conj-adjective-stem+ hash) "Adjective Stem"))

(defun errata-conj-rules-hook (hash)
  (let* ((pos (get-pos-index "adj-i"))
         (rules (list (make-conjugation-rule pos +conj-adverbial+ nil nil 1 
                                             1 "く" "" "")
                      (make-conjugation-rule pos +conj-adjective-stem+ nil nil 1
                                             1 "" "" ""))))
    (dolist (rule rules)
      (push rule (gethash pos hash nil))))
  ;; fix non-past negative formal for v1 v1-s
  (let ((posi (mapcar 'get-pos-index '("v1" "v1-s"))))
    (loop for pos in posi
         do (loop for rule in (gethash pos hash)
                 when (and (= (cr-conj rule) 1) (cr-fml rule) (cr-neg rule))
               do (setf (cr-okuri rule) "ません"))))
  ;; remove potential forms of vs-s verbs
  (let ((pos (get-pos-index "vs-s")))
    (setf (gethash pos hash) (remove-if (lambda (r) (= (cr-conj r) 5)) (gethash pos hash))))
  )

(defparameter *weak-conj-types* (list +conj-adjective-stem+))

(defparameter *skip-conj-forms* ;; (type neg fml), :any matches whatever
  '((10 t :any)
    (3 t t)
    ("vs-s" 5 :any :any)
    ))

(defun skip-by-conj-data (conj-data)
  (flet ((matches (cd)
           (let* ((prop (conj-data-prop cd))
                  (prop-list (list (pos prop) (conj-type prop) (conj-neg prop) (conj-fml prop))))
             (some (lambda (sk)
                     (case (length sk)
                       (3 (every (lambda (l r) (or (eql r :any) (eql l r))) (cdr prop-list) sk))
                       (4 (and (equal (car prop-list) (car sk))
                               (every (lambda (l r) (or (eql r :any) (eql l r))) (cdr prop-list) (cdr sk))))))
                   *skip-conj-forms*))))
    (and conj-data (every #'matches conj-data))))
