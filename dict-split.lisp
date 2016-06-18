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
  (alexandria:with-gensyms (offset parts)
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
                `(let ((pseq ,(if (listp part-seq)
                                  (if (and part-seq (stringp (car part-seq)))
                                      `(list (seq (car (find-word-conj-of ,@part-seq))))
                                      `',part-seq)
                                  `',(list part-seq)))
                       (part-length ,part-length-form))
                   (push (car (apply
                               ,(if conj-p
                                   ''find-word-conj-of
                                   ''find-word-seq)
                                (let ((part-txt (subseq ,text-var ,offset 
                                                       (and part-length (+ ,offset part-length)))))
                                  ,(if rendaku-p
                                      '(unrendaku part-txt)
                                      'part-txt))
                                pseq))
                         ,parts)
                   (incf ,offset part-length)))
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

(defmacro def-de-split (seq seq-a &key (score 15))
  (let ((name (intern (format nil "~a~a" :split-de- seq))))
    `(def-simple-split ,name ,seq ,score (len)
       (,seq-a (- len 1))
       (2028980 1))))

(def-de-split 1163700 1576150) ;; 一人で

(def-de-split 1611020 1577100) ;; 何で

(def-de-split 1004800 1628530) ;; これで

(def-de-split 1413430 1413420) ;; 大急ぎで

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

(def-de-split 1317210 1611820) ;; 耳元で

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

;; nakunaru split: because naku often attaches to previous word

(def-simple-split split-nakunaru 1529550 30 (len) ;; 無くなる
  (("無く" 1529520) 2)
  (1375610 (- len 2) t))

(def-simple-split split-nakunaru2 1518540 10 (len txt r) ;; 亡くなる
  (:test (eql (word-type r) :kana))
  (("亡く" 1518450) 2)
  (1375610 (- len 2) t))

;; tegakakaru split (kana form might conflict with other uses of kakaru verb)

(def-simple-split split-tegakakaru 2089710 10 (len) ;; 手が掛かる
  (1327190 1) ;; 手
  (2028930 1) ;; が
  (1207590 (- len 2) t))
  

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
  (1495740 (- len 2) t))

(def-simple-split split-janaika 2819990 20 (len) ;; じゃないか
  (("じゃない" 2089020) 4)
  (2028970 1))

(def-simple-split split-kaasan 1609470 50 (len txt r) ;; 母さん
  (:test (eql (word-type r) :kanji))
  (1514990 1)
  (1005340 2))

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
