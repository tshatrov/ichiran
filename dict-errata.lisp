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
      (let* ((maxord (query (:select (:max 'ord) :from table :where (:= 'seq seq)) :single))
             (ord (if (eql maxord :null) 0 (1+ maxord))))
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

(defun add-gloss (seq ord &rest texts)
  (let* ((sense-id (query (:select 'id :from 'sense :where (:and (:= 'seq seq) (:= 'ord ord))) :single))
         (glosses (select-dao 'gloss (:= 'sense-id sense-id) (:desc :ord)))
         (glosses-text (mapcar 'text glosses))
         (max-ord (if glosses (1+ (ord (car glosses))) 0)))
    (loop for new-text in texts
       unless (find new-text glosses-text :test 'equal)
       do (make-dao 'gloss :sense-id sense-id :text new-text :ord max-ord)
         (incf max-ord))))

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
  (set-primary-nokanji 1289030 nil) ;; いまいち

  (add-primary-nokanji 1415510 "タカ")

  ;;; gozaimashita / gozaimashitara
  (add-conj 1612690 '(2 "exp" :null :null)
            '(("ございます" "ございました")))
  (add-conj 1612690 '(11 "exp" :null :null)
            '(("ございます" "ございましたら")))
  (add-conj 1612690 '(1 "exp" t :null)
            '(("ございます" "ございません")))

  ;; きみ / キミ
  (delete-reading 1247250 "キミ")
  (add-reading 2015370 "ワシ")
  (add-reading 1202410 "カニ")
  (delete-reading 1521960 "ボツ")
  (add-reading 2145800 "イラ")
  (add-reading 1517840 "ハチ")
  (set-common 'kana-text 1517840 "ハチ" 34)

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
  ;; つける
  (delete-sense-prop 1610400 "misc" "uk")
  ;; つく
  (delete-sense-prop 2097190 "misc" "uk")

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
  ;; セミ
  (add-sense-prop 1387080 0 "misc" "uk")
  ;; くせ
  (add-sense-prop 1509350 0 "misc" "uk")
  ;; はやる
  (add-sense-prop 1637460 0 "misc" "uk")

  ;; なの
  (add-sense-prop 2425930 0 "pos" "prt")
  ;; わね
  (add-sense-prop 2457930 0 "pos" "prt")
  ;; とん
  (delete-sense-prop 2629920 "pos" "adv-to")

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
  (set-common 'kana-text 1204090 "がいまい" :null)
  (set-common 'kana-text 1459170 "ないほう" :null)

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
  (set-common 'kana-text 1546380 "ようと" 0)
  (set-common 'kana-text 2246510 "なさそう" 0)
  (set-common 'kanji-text 2246510 "無さそう" 0)
  (set-common 'kana-text 1579110 "きょう" 2)
  (set-common 'kana-text 1235870 "きょう" :null)
  (set-common 'kana-text 1587200 "いこう" 11)
  (set-common 'kana-text 1158240 "いこう" 0)
  (set-common 'kana-text 1534440 "もうまく" :null)
  (set-common 'kana-text 1459400 "ないよう" 0)
  (set-common 'kana-text 1590480 "カッコ" 0)
  (set-common 'kana-text 1208240 "カッコ" 0)
  (set-common 'kana-text 1495770 "つける" 11)
  (set-common 'kana-text 1610400 "つける" 12)
  (set-common 'kana-text 1495740 "つく" 11)
  (set-common 'kanji-text 1495740 "付く" 11)

  ;; remove sense for なり and make it not root
  (delete-senses 2611370 (constantly t))
  (let ((entry (get-dao 'entry 2611370)))
    (setf (slot-value entry 'root-p) nil)
    (update-dao entry))
  (delete-reading 2611370 "為り")

  ;; 包む is read as tsutsumu
  (rearrange-readings-conj 1584060 'kana-text "つつ")
  (set-common 'kana-text 1584060 "つつむ" 6)

  (rearrange-readings-conj 1602880 'kanji-text "増や")

  ;; delete noun sense for と
  (delete-senses 1008490 (lambda (prop) (and (equal (text prop) "n") (equal (tag prop) "pos"))))

  ;; delete prt sense for たい
  (delete-senses 2017560 (lambda (prop) (and (equal (text prop) "prt") (equal (tag prop) "pos"))))

  ;; delete adj stem conjugation for ない
  (delete-conjugation 2029110 2257550)
  (delete-conjugation 2086640 2684620) ;; しい

  (add-errata-feb17)
  (add-errata-jan18)
  (add-errata-mar18)
  (add-errata-counters)
  )

(defun add-errata-feb17 ()
  (set-common 'kana-text 2136890 "とする" :null)
  (set-common 'kana-text 2100900 "となる" :null)
  (set-common 'kana-text 1006200 "すべき" :null)
  (set-common 'kana-text 2683060 "なのです" :null)
  (set-common 'kana-text 2683060 "なんです" :null)
  (set-common 'kana-text 1001200 "おい" :null)
  (set-common 'kana-text 1001200 "おおい" :null)
  (set-common 'kanji-text 1441840 "伝い" 0)
  (set-common 'kanji-text 1409140 "身体" 0)
  (set-common 'kanji-text 2830705 "身体" :null)
  (set-common 'kana-text 1009040 "どきっと" 0)
  (set-common 'kana-text 2261300 "するべき" :null)
  (set-common 'kana-text 2215430 "には" :null)
  (set-common 'kana-text 2210140 "まい" :null)
  (set-common 'kana-text 2192950 "なさい" :null)
  (set-common 'kana-text 2143350 "かも" :null)
  (set-common 'kana-text 2106890 "そのよう" :null)
  (set-common 'kana-text 2084040 "すれば" :null)
  (set-common 'kana-text 2036080 "うつ" :null)
  (set-common 'kana-text 1922760 "という" :null)
  (set-common 'kana-text 1632520 "ふん" :null)
  (set-common 'kana-text 1631750 "がる" :null)
  (set-common 'kana-text 1394680 "そういう" :null)
  (set-common 'kana-text 1208840 "かつ" :null)
  (set-common 'kana-text 1011430 "べき" :null)
  (set-common 'kana-text 1009610 "にも" :null)
  (set-common 'kana-text 1008340 "である" :null)
  (set-common 'kana-text 1007960 "ちんちん" :null)
  (set-common 'kana-text 1301230 "さんなん" :null)
  (set-common 'kanji-text 1311010 "氏" 20)
  (set-common 'kana-text 1311010 "うじ" 20)
  (set-common 'kanji-text 2101130 "氏" 21)
  (set-common 'kana-text 1155180 "いない" 10)
  (set-common 'kanji-text 1609450 "思いきって" 0)
  (set-common 'kanji-text 1309320 "思いきる" 0)
  (set-common 'kana-text 1312880 "メス" 15)
  (set-common 'kana-text 1312880 "めす" :null)
  (set-common 'kana-text 2061540 "ぶっちゃける" 0)
  (set-common 'kana-text 2034520 "ですら" 0)
  (set-common 'kana-text 1566210 "いずれ" 9)

  (delete-sense-prop 2021030 "misc" "uk") ;; 摂る（とる）
  (delete-sense-prop 1586730 "misc" "uk") ;; 粗 (あら)
  (delete-sense-prop 1441400 "misc" "uk") ;; 点く （つく）

  (add-sense-prop 1569590 0 "misc" "uk") ;; 痙攣 けいれん
  (add-sense-prop 1590540 0 "misc" "uk") ;; 仮名 かな
  (add-sense-prop 1430200 0 "misc" "uk") ;; いただき

  (set-primary-nokanji 1374550 nil) ;; すごい
  (set-primary-nokanji 1591900 nil) ;; きれい
  (set-primary-nokanji 1000230 nil) ;; あかん
  (set-primary-nokanji 1517810 nil) ;; もやし
  (set-primary-nokanji 1585410 nil) ;; まま

  (add-reading 1029150 "えっち")
  (add-reading 1363740 "マネ")
  (set-common 'kana-text 1363740 "マネ" 9)

  (set-common 'kanji-text 1000420 "彼の" :null)
  (set-common 'kanji-text 2219590 "元" 10)
  (set-common 'kana-text 2219590 "もと" 10)
  (set-common 'kana-text 1394760 "さほど" 0)
  (set-common 'kana-text 1529560 "なし" 10)
  (set-common 'kana-text 1436830 "ていない" :null)
  (set-common 'kana-text 1057580 "さぼる" 0)
  (set-common 'kanji-text 1402420 "走り" :null)
  (set-common 'kana-text 1402420 "はしり" :null)
  (set-common 'kana-text 1209540 "かる" :null)
  (set-common 'kana-text 1244840 "かる" :null)
  (set-common 'kana-text 1280640 "こうは" 0)
  (set-common 'kana-text 1158960 "いほう" 0)

  (delete-sense-prop 2122310 "pos" "prt") ;; え

  (load-entry "
<entry>
<ent_seq>1613860</ent_seq>
<k_ele><keb>回戦</keb></k_ele>
<r_ele><reb>かいせん</reb></r_ele>
<sense>
<pos>n</pos><pos>ctr</pos>
<gloss xml:lang=\"eng\">match</gloss><gloss xml:lang=\"eng\">game</gloss>
</sense>
</entry>" :if-exists :skip)

  (load-entry "
<entry>
<ent_seq>99000000</ent_seq>
<k_ele><keb>お掛け</keb><ke_pri>spec1</ke_pri></k_ele>
<r_ele><reb>おかけ</reb><re_pri>spec1</re_pri></r_ele>
<sense>
<pos>vs</pos><misc>uk</misc><misc>hum</misc>
<gloss xml:lang=\"eng\">to cause</gloss>
</sense>
<sense>
<pos>vs</pos><misc>uk</misc><misc>hum</misc>
<gloss xml:lang=\"eng\">to sit</gloss>
</sense>
<sense>
<pos>vs</pos><misc>uk</misc><misc>hum</misc>
<gloss xml:lang=\"eng\">to spend (time)</gloss>
</sense>
</entry>" :if-exists :overwrite)

  (recalc-entry-stats 1029150 1363740 1613860 99000000)
  )

(defun add-errata-jan18 ()
  (set-common 'kanji-text 2067770 "等" :null)
  (set-common 'kana-text 2067770 "ら" :null)
  (set-common 'kanji-text 1242230 "近よる" 38)
  (set-common 'kanji-text 1315120 "字" 0)
  (set-common 'kana-text 1315120 "あざ" 0)
  (set-common 'kanji-text 1315130 "字" 5)
  (set-common 'kana-text 1315130 "じ" 0)
  (set-common 'kana-text 1005530 "しっくり" 0)
  (set-common 'kana-text 1554850 "りきむ" :null)
  (set-common 'kana-text 2812650 "ゲー" 0)
  (set-common 'kana-text 2083340 "やろう" 0)
  (set-common 'kana-text 2083340 "やろ" 0)
  (set-common 'kana-text 2122590 "てか" 0)
  (set-common 'kana-text 1008730 "とろ" :null)
  (set-common 'kana-text 1457840 "ないかい" :null)
  (set-common 'kana-text 2829697 "いかん" 0)
  (set-common 'kana-text 2157330 "おじゃま" 9)
  (set-common 'kana-text 1199800 "かいらん" :null)
  (set-common 'kana-text 2719580 "いらん" 0)
  (set-common 'kana-text 1808040 "めちゃ" 0)
  (set-common 'kana-text 1277450 "すき" 9)
  (set-common 'kana-text 1006460 "ズレる" 0)
  (set-common 'kanji-text 1522290 "本会議" 0)
  (set-common 'kana-text 1522290 "ほんかいぎ" 0)
  (set-common 'kana-text 1220570 "きたい" 10)
  (set-common 'kana-text 1221020 "きたい" 11)
  (set-common 'kana-text 2083990 "ならん" 0)
  (set-common 'kanji-text 2518850 "切れ" 0)
  (set-common 'kanji-text 1221900 "基地外" 0)
  (set-common 'kana-text 1379380 "せいと" 10)
  (set-common 'kanji-text 1203280 "外に" :null)
  (set-common 'kanji-text 1383690 "後継ぎ" 0)
  (set-common 'kana-text 2083600 "すまん" 0)

  (add-reading 1384840 "キレ" :common 0)

  (delete-sense-prop 1303400 "misc" "uk") ;; 撒く/まく
  (delete-sense-prop 1434020 "misc" "uk") ;; 吊る/つる
  (delete-sense-prop 1196520 "misc" "uk") ;; かすむ
  (delete-sense-prop 1414190 "misc" "uk") ;; 大人しい

  (add-sense-prop 1188380 0 "misc" "uk") ;; なんでもかんでも
  (add-sense-prop 1258330 0 "misc" "uk") ;; いぬ
  (add-sense-prop 2217330 0 "misc" "uk") ;; わい

  (set-primary-nokanji 1258330 nil) ;; いぬ
  (set-primary-nokanji 1588930 nil) ;; おかず

  (add-sense 1315920 2 "hours (period of)") ;; 時間
  (add-sense-prop 1315920 2 "pos" "ctr")

  (add-sense-prop 1445160 0 "pos" "ctr") ;; 度
  )

(defun add-errata-mar18 ()
  (set-common 'kana-text 1207610 "かける" 0)
  (set-common 'kanji-text 1236100 "強いる" :null)
  (set-common 'kana-text 1236100 "しいる" :null)
  (set-common 'kana-text 1451750 "おんなじ" 0)
  (set-common 'kanji-text 2068330 "事故る" 0)
  (set-common 'kana-text 1579260 "きのう" 2)
  (set-common 'kanji-text 2644980 "柔らかさ" 0)
  (set-common 'kana-text 2644980 "やわらかさ" 0)
  (set-common 'kana-text 2083610 "ベタ" 0)
  (set-common 'kana-text 2083610 "べた" 0)
  (set-common 'kana-text 1119610 "ベタ" :null)
  (set-common 'kana-text 1004840 "コロコロ" 0)
  (set-common 'kana-text 1257040 "ケンカ" 0)
  (set-common 'kana-text 1633840 "ごとき" 0)

  (add-sense-prop 1238460 0 "misc" "uk") ;; そば

  (delete-sense-prop 1896380 "misc" "uk") ;; 出
  (delete-sense-prop 1157000 "misc" "uk") ;; 易しい
  (delete-sense-prop 1576360 "misc" "uk") ;; 逸れる

  (add-sense-prop 1468900 0 "pos" "ctr") ;; 年生
  (add-sense-prop 1241380 0 "pos" "ctr") ;; 斤

  ;;; add sense for な
  (add-sense 2029110 5 "indicates な-adjective")

  (add-reading 2757120 "問うは一度の恥問わぬは末代の恥")
  )

(defun add-errata-counters ()
  (delete-reading 1299960 "さんかい")
  (mapc 'set-reading (select-dao 'kanji-text (:= 'seq 1299960)))

  (add-reading 2081610 "タテ")

  (add-sense-prop 1427420 0 "pos" "ctr") ;; 丁目
  (add-sense-prop 1397450 0 "pos" "ctr") ;; 組
  (add-sense-prop 1351270 0 "pos" "ctr") ;; 章
  (add-sense-prop 1351270 1 "pos" "n") ;; 章
  (add-sense-prop 1490430 0 "pos" "ctr") ;; 秒
  (add-sense-prop 2020680 0 "pos" "ctr") ;; 時
  (add-sense-prop 1502840 0 "pos" "ctr") ;; 分
  (add-sense-prop 1373990 0 "pos" "ctr") ;; 世紀
  (add-sense-prop 1281690 0 "pos" "ctr") ;; 行
  (add-sense-prop 1281690 1 "pos" "n")
  (add-sense-prop 1042610 1 "pos" "ctr") ;; キロ
  (add-sense-prop 1100610 0 "pos" "ctr") ;; パーセント
  (add-sense-prop 1100380 3 "pos" "ctr") ;; パー

  (add-sense 1215240 1 "counter for rooms") ;; 間
  (add-sense-prop 1215240 1 "pos" "ctr")
  (add-sense 1583470 3 "counter for dishes") ;; 品（しな）
  (add-sense-prop 1583470 3 "pos" "ctr")

  (add-sense-prop 1411070 0 "pos" "ctr") ;; 袋
  (add-sense-prop 1411070 1 "pos" "n")

  (add-sense-prop 1328810 0 "pos" "ctr") ;; 種

  (add-sense 1220540 5 "season (of a TV show)")
  (add-sense-prop 1220540 5 "pos" "n")
  (add-sense-prop 1220540 5 "pos" "ctr") ;; 期

  (add-sense-prop 1284220 0 "pos" "ctr") ;; 号
  (add-sense-prop 1284220 1 "pos" "n")
  (add-sense-prop 1284220 1 "pos" "n-suf")
  (add-sense-prop 1482360 0 "pos" "ctr") ;; 番地
  (add-sense-prop 1473230 1 "pos" "ctr") ;; 倍
  (add-sense-prop 2022640 0 "pos" "ctr") ;; 番
  (add-sense-prop 2022640 1 "pos" "n")
  (add-sense-prop 1175570 0 "pos" "ctr") ;; 円
  (add-sense-prop 1175570 1 "pos" "n")
  (add-sense-prop 1270910 1 "pos" "ctr") ;; 語
  (add-sense-prop 1270910 1 "pos" "n")
  (add-sense-prop 1270910 1 "pos" "n-suf")
  (add-sense-prop 1315130 0 "pos" "ctr") ;; 字
  (add-sense-prop 1315130 1 "pos" "n")
  (add-sense-prop 1199640 0 "pos" "ctr") ;; 回転

  (add-sense-prop 1047880 0 "pos" "ctr") ;; ケース
  (add-sense-prop 1047880 1 "pos" "n")

  (add-sense-prop 1214540 0 "pos" "ctr") ;; 缶
  (add-sense-prop 1244080 0 "pos" "ctr") ;; 区
  (add-sense-prop 1239700 0 "pos" "ctr") ;; 曲

  (add-sense-prop 1294940 0 "pos" "ctr") ;; 才 歳
  (add-sense-prop 1294940 1 "pos" "suf")

  (add-sense-prop 1575510 0 "pos" "ctr") ;; コマ
  (add-sense-prop 1575510 1 "pos" "n")

  (add-sense-prop 1505390 0 "pos" "ctr") ;; 文字

  (add-sense-prop 1101700 0 "pos" "ctr") ;; パック
  (add-sense-prop 1101700 1 "pos" "n")
  (add-sense-prop 1101700 1 "pos" "vs")

  (add-sense-prop 1120410 0 "pos" "ctr") ;; ページ
  (add-sense-prop 1138570 0 "pos" "ctr") ;; ラウンド
  (add-sense-prop 1956400 0 "pos" "ctr") ;; 集
  (add-sense-prop 1333450 0 "pos" "ctr") ;; 週
  (add-sense-prop 1480050 0 "pos" "ctr") ;; 反
  (add-sense-prop 1558330 0 "pos" "ctr") ;; 列

  (add-sense-prop 1956530 0 "pos" "ctr") ;; 寸
  (add-sense-prop 1324110 0 "pos" "ctr") ;; 尺
  (add-sense-prop 1324110 1 "pos" "n")
  (add-sense-prop 1382450 0 "pos" "ctr") ;; 石

  (add-sense-prop 1253800 1 "pos" "ctr") ;; 桁
  (add-sense-prop 1253800 1 "pos" "n")

  (add-sense-prop 1297240 0 "pos" "ctr") ;; 作

  (add-sense 2262420 4 "counter for strings") ;; 弦
  (add-sense-prop 2262420 4 "pos" "ctr")

  (add-sense-prop 1368480 0 "pos" "ctr") ;; 人前
  (add-gloss 1368480 0 "for N people")

  (add-sense-prop 1732510 1 "pos" "ctr") ;; 番手
  (add-sense-prop 2086480 1 "pos" "ctr") ;; 頭身
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
                             2210710 ;; ましょうか
                             2257550 ;; ない
                             2210320 ;; ません
                             2017560 ;; たい
                             2394890 ;; とる
                             2194000 ;; であ
                             2822130 ;; て良い
                             2568000 ;; れる/られる
                             2537250 ;; しようとする
                             2760890 ;; 三箱
                             2831062 ;; てる
                             2831063 ;; てく
                             2029030 ;; ものの
                             )
  "seq of words that aren't really words, like suffixes etc."
  )

(defparameter *final-prt* '(2017770 ;; かい
                            1008450 ;; では
                            2425930 ;; なの
                            ;; 2780660 ;; もの
                            2130430 ;; け っけ
                            2029130 ;; ぞ
                            2834812 ;; ぜ
                            2718360 ;; がな
                            2201380 ;; わい
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

(defparameter *no-kanji-break-penalty*
  '(1169870 ;; 飲む
    1198360 ;; 会議
    )
  "Words that get no kanji break penalty")

;; Additional conjugations

(defconstant +conj-adverbial+ 50)
(defconstant +conj-adjective-stem+ 51)
(defconstant +conj-negative-stem+ 52)

(defun errata-conj-description-hook (hash)
  (setf (gethash +conj-adverbial+ hash) "Adverbial")
  (setf (gethash +conj-adjective-stem+ hash) "Adjective Stem")
  (setf (gethash +conj-negative-stem+ hash) "Negative Stem"))

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
  ;; remove 2nd causative form (it's not v1 so it breaks conjugations)
  ;; and add conj-negative-stem for godan verbs
  (maphash
   (lambda (key value)
     (let ((val (remove-if (lambda (r) (and (= (cr-conj r) 7) (= (cr-onum r) 2))) value))
           (pos (get-pos key)))
       (when (alexandria:starts-with-subseq "v5" pos)
         (let* ((neg-rule (find-if (lambda (r) (and (= (cr-conj r) 1) (cr-neg r) (not (cr-fml r)))) val))
                (len (length (cr-okuri neg-rule))))
           (when (and neg-rule (> len 2))
             (let ((new-rule (copy-conjugation-rule neg-rule)))
               (setf (cr-conj new-rule) +conj-negative-stem+
                     (cr-okuri new-rule) (subseq (cr-okuri neg-rule) 0 (- len 2)))
               (push new-rule val)))))
       (setf (gethash key hash) val)))
   hash)
  )

(defparameter *skip-conj-forms* ;; (type neg fml), :any matches whatever
  '((10 t :any)
    (3 t t)
    ("vs-s" 5 :any :any)
    ))

(defparameter *weak-conj-forms*
  `((,+conj-adjective-stem+ :any :any)
    (,+conj-negative-stem+ :any :any)
    (9 t :any)))

(defun test-conj-prop (prop forms)
  (let ((prop-list (list (pos prop) (conj-type prop) (conj-neg prop) (conj-fml prop))))
    (some (lambda (sk)
            (case (length sk)
              (3 (every (lambda (l r) (or (eql r :any) (eql l r))) (cdr prop-list) sk))
              (4 (and (equal (car prop-list) (car sk))
                      (every (lambda (l r) (or (eql r :any) (eql l r))) (cdr prop-list) (cdr sk))))))
          forms)))

(defun skip-by-conj-data (conj-data)
  (flet ((matches (cd)
           (test-conj-prop (conj-data-prop cd) *skip-conj-forms*)))
    (and conj-data (every #'matches conj-data))))
