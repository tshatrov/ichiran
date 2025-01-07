(in-package #:ichiran/characters)

(defparameter *sokuon-characters* '(:sokuon "っッ"))

(defparameter *iteration-characters* '(:iter "ゝヽ" :iter-v "ゞヾ"))

(defparameter *modifier-characters* '(:+a "ぁァ" :+i "ぃィ" :+u "ぅゥ" :+e "ぇェ" :+o "ぉォ"
                                      :+ya "ゃャ" :+yu "ゅュ" :+yo "ょョ" :+wa "ゎヮ"
                                      :long-vowel "ー"))

(defparameter *kana-characters*
  '(:a "あア"     :i "いイ"     :u "うウ"     :e "えエ"     :o "おオ"
    :ka "かカ"    :ki "きキ"    :ku "くク"    :ke "けケ"    :ko "こコ"
    :sa "さサ"    :shi "しシ"   :su "すス"    :se "せセ"    :so "そソ"
    :ta "たタ"    :chi "ちチ"   :tsu "つツ"   :te "てテ"    :to "とト"
    :na "なナ"    :ni "にニ"    :nu "ぬヌ"    :ne "ねネ"    :no "のノ"
    :ha "はハ"    :hi "ひヒ"    :fu "ふフ"    :he "へヘ"    :ho "ほホ"
    :ma "まマ"    :mi "みミ"    :mu "むム"    :me "めメ"    :mo "もモ"
    :ya "やヤ"                  :yu "ゆユ"                 :yo "よヨ"
    :ra "らラ"    :ri "りリ"    :ru "るル"    :re "れレ"    :ro "ろロ"
    :wa "わワ"    :wi "ゐヰ"                 :we "ゑヱ"    :wo "をヲ"
    :n "んン"

    :ga "がガ"    :gi "ぎギ"    :gu "ぐグ"    :ge "げゲ"    :go "ごゴ"
    :za "ざザ"    :ji "じジ"    :zu "ずズ"    :ze "ぜゼ"    :zo "ぞゾ"
    :da "だダ"    :dji "ぢヂ"   :dzu "づヅ"   :de "でデ"    :do "どド"
    :ba "ばバ"    :bi "びビ"    :bu "ぶブ"    :be "べベ"    :bo "ぼボ"
    :pa "ぱパ"    :pi "ぴピ"    :pu "ぷプ"    :pe "ぺペ"    :po "ぽポ"
    :vu "ゔヴ"
    ))

(defparameter *all-characters* (append *sokuon-characters*
                                       *iteration-characters*
                                       *modifier-characters*
                                       *kana-characters*))

(defparameter *char-class-hash*
  (let ((hash (make-hash-table)))
    (loop for (class chars) on *all-characters* by #'cddr
         do (loop for char across chars
               do (setf (gethash char hash) class)))
    hash))

(defun get-char-class (char)
  (gethash char *char-class-hash* char))

(defun long-vowel-modifier-p (modifier prev-char)
  (let ((vowel (getf '(:+a #\A :+i #\I :+u #\U :+e #\E :+o #\O) modifier)))
    (when vowel
      (let* ((char-class (get-char-class prev-char))
             (char-str (string char-class)))
        (and (keywordp char-class)
           (char= vowel (char char-str (1- (length char-str)))))))))

(defmacro hash-from-list (var list &key test)
  (alexandria:with-gensyms (hash key val)
    `(defparameter ,var
       (let ((,hash (make-hash-table ,@(when test `(:test ,test)))))
         (loop for (,key ,val) on ,list by #'cddr
              do (setf (gethash ,key ,hash) ,val))
         ,hash))))

(hash-from-list *dakuten-hash*
                '(:ka :ga :ki :gi :ku :gu :ke :ge :ko :go
                  :sa :za :shi :ji :su :zu :se :ze :so :zo
                  :ta :da :chi :dji :tsu :dzu :te :de :to :do
                  :ha :ba :hi :bi :fu :bu :he :be :ho :bo
                  :u :vu))

(hash-from-list *handakuten-hash*
                '(:ha :pa :hi :pi :fu :pu :he :pe :ho :po))

(hash-from-list *undakuten-hash*
                '(:ga :ka :gi :ki :gu :ku :ge :ke :go :ko
                  :za :sa :ji :shi :zu :su :ze :se :zo :so
                  :da :ta :dji :chi :dzu :tsu :de :te :do :to
                  :ba :ha :bi :hi :bu :fu :be :he :bo :ho
                  :pa :ha :pi :hi :pu :fu :pe :he :po :ho
                  :vu :u))

(defun voice-char (cc)
  "Returns a voiced form of character class, or the same character class"
  (gethash cc *dakuten-hash* cc))

(defparameter *punctuation-marks*
  '("【" " [" "】" "] "
    "、" ", " "，" ", "
    "。" ". " "・・・" "... " "・" " " "　" " "
    "「" " \"" "」" "\" " "゛" "\""
    "『" " «"  "』" "» "
    "〜" " - " "：" ": " "！" "! " "？" "? " "；" "; "))

(defun dakuten-join (dakuten-hash char)
  (loop for (cc . ccd) in (alexandria:hash-table-alist dakuten-hash)
     for kc = (getf *kana-characters* cc)
     for kcd = (getf *kana-characters* ccd)
     for offset = (- (length kc) (length kcd))
     if (> offset 0) do (setf kc (subseq kc offset))
     nconcing (loop for idx from 0 below (length kc)
                 nconcing (list (coerce (list (char kc idx) char) 'string)
                                (coerce (list (char kcd idx)) 'string)))))

(defparameter *dakuten-join*
  (append (dakuten-join *dakuten-hash* #\゛) (dakuten-join *handakuten-hash* #\゜)))

(defparameter *half-width-kana* "･ｦｧｨｩｪｫｬｭｮｯｰｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜﾝﾞﾟ")
(defparameter *full-width-kana* "・ヲァィゥェォャュョッーアイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワン゛゜")

(defparameter *abnormal-chars*
  (concatenate 'string
               "０１２３４５６７８９ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ＃＄％＆（）＊＋／〈＝〉？＠［］＾＿‘｛｜｝～"
               *half-width-kana*))

(defparameter *normal-chars*
  (concatenate 'string "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ#$%&()*+/<=>?@[]^_`{|}~"
               *full-width-kana*))

(defparameter *katakana-regex* "[ァ-ヺヽヾー]")
(defparameter *katakana-uniq-regex* "[ァ-ヺヽヾ]")
(defparameter *hiragana-regex* "[ぁ-ゔゝゞー]")
(defparameter *kanji-regex* "[々ヶ〆一-龯]")
(defparameter *kanji-char-regex* "[一-龯]")

(defparameter *nonword-regex* "[^々ヶ〆一-龯ァ-ヺヽヾぁ-ゔゝゞー〇]")
(defparameter *numeric-regex* "[0-9０-９〇一二三四五六七八九零壱弐参拾十百千万億兆京]")
(defparameter *num-word-regex* "[0-9０-９〇々ヶ〆一-龯ァ-ヺヽヾぁ-ゔゝゞー]")
(defparameter *word-regex* "[々ヶ〆一-龯ァ-ヺヽヾぁ-ゔゝゞー〇]")
(defparameter *digit-regex* "[0-9０-９〇]")
(defparameter *decimal-point-regex* "[.,]")

(defparameter *basic-split-regex*
  (format nil "((?:(?<!~a|~a)~a+|~a)~a*~a|~a)"
          *decimal-point-regex* *digit-regex* *digit-regex*
          *word-regex* *num-word-regex* *word-regex* *word-regex*))

(defparameter *char-class-regex-mapping*
  `((:katakana ,*katakana-regex*)
    (:katakana-uniq ,*katakana-uniq-regex*)
    (:hiragana ,*hiragana-regex*)
    (:kanji ,*kanji-regex*)
    (:kanji-char ,*kanji-char-regex*)
    (:kana ,(format nil "(~a|~a)" *katakana-regex* *hiragana-regex*))
    (:traditional ,(format nil "(~a|~a)" *hiragana-regex* *kanji-regex*))
    (:nonword ,*nonword-regex*)
    (:number ,*numeric-regex*)))

(deftype char-class () '(member :katakana :katakana-uniq
                         :hiragana :kanji :kanji-char
                         :kana :traditional :nonword :number))

(defparameter *char-scanners*
  (mapcar (lambda (pair) (cons (car pair) (ppcre:create-scanner (format nil "^~a+$" (cadr pair)))))
          *char-class-regex-mapping*))

(defparameter *char-scanners-inner*
  (mapcar (lambda (pair)
            (cons (car pair) (ppcre:create-scanner `(:greedy-repetition 1 nil (:regex ,@(cdr pair))))))
          *char-class-regex-mapping*))

(defun test-word (word char-class)
  (declare (type char-class char-class))
  (let ((regex (cdr (assoc char-class *char-scanners*))))
    (ppcre:scan regex word)))

(defun count-char-class (word char-class)
  (declare (type char-class char-class))
  (let ((cnt 0)
        (regex (cadr (assoc char-class *char-class-regex-mapping*))))
    (ppcre:do-matches (s e regex word cnt)
      (incf cnt))))

(defun collect-char-class (word char-class)
  (declare (type char-class char-class))
  (let ((regex (cadr (assoc char-class *char-class-regex-mapping*)))
        result)
    (ppcre:do-matches-as-strings (s regex word (nreverse result))
      (push s result))))

(defun sequential-kanji-positions (word &optional (offset 0))
  (let (positions)
    (ppcre:do-matches (s e "(?=[々一-龯][々一-龯])" word)
      (push (+ s 1 offset) positions))
    (nreverse positions)))

(defun kanji-mask (word)
  "SQL LIKE mask for word"
  (let ((regex (ppcre:create-scanner `(:greedy-repetition 1 nil (:regex ,*kanji-regex*)))))
    (ppcre:regex-replace-all regex word "%")))

(defun kanji-regex (word)
  (ppcre:create-scanner
   `(:sequence
     :start-anchor
     ,@(loop for char across (kanji-mask word)
          if (char= char #\%)
          collect '(:greedy-repetition 1 nil :everything)
          else collect char)
     :end-anchor)))

(defun kanji-match (word reading)
  (ppcre:scan (kanji-regex word) reading))

(defun kanji-cross-match (word reading new-word)
  (let* ((m (mismatch word new-word))
         (r-cut (+ m (- (length reading) (length word)))))
    (when (and (> m 0) (<= 0 r-cut (length reading)))
      (let ((reading-head (subseq reading 0 r-cut)))
        (concatenate 'string reading-head (subseq new-word m))))))

(defun simplify-ngrams (str map)
  (let* ((alist (loop for (from to) on map by #'cddr collect (cons from to)))
         (scanner (ppcre:create-scanner (cons :alternation (mapcar #'car alist)))))
    (ppcre:regex-replace-all scanner str
                             (lambda (match &rest rest)
                               (declare (ignore rest))
                               (cdr (assoc match alist :test #'equal)))
                             :simple-calls t)))

(defun to-normal-char (char &key context)
  (let ((pos (position char (if (eql context :kana) *half-width-kana* *abnormal-chars*))))
    (when pos
      (char (if (eql context :kana) *full-width-kana* *normal-chars*) pos))))

(defun normalize (str &key context)
  (loop for i from 0 below (length str)
       for char = (char str i)
       for normal-char = (to-normal-char char :context context)
       if normal-char do (setf (char str i) normal-char))
  (setf str (simplify-ngrams str
                             (if (eql context :kana)
                                 *dakuten-join*
                                 (append *punctuation-marks* *dakuten-join*)))))

(defun split-by-regex (regex str)
  (remove-if (lambda (seg) (= (length seg) 0))
             (ppcre:split regex str :with-registers-p t)))

(defun basic-split (str)
  "splits string into segments of japanese and misc characters"
  (let* ((split1 (split-by-regex *basic-split-regex* str)))
    (loop for segment in split1
         for misc = (test-word segment :nonword) then (not misc)
         collect (cons (if misc :misc :word) segment))))

(defun mora-length (str)
  "like length but doesn't count modifier characters"
  (count-if-not (lambda (char)
                  (find char "っッぁァぃィぅゥぇェぉォゃャゅュょョー"))
                str))

(defun as-hiragana (str)
  "convert katakana to hiragana"
  (map 'string
       (lambda (char)
         (let* ((char (or (to-normal-char char) char))
                (class (gethash char *char-class-hash*)))
           (if class
               (char (getf *all-characters* class) 0)
               char)))
       str))

(defun as-katakana (str)
  "convert hiragana to katakana"
  (map 'string
       (lambda (char)
         (let* ((char (or (to-normal-char char) char))
                (class (gethash char *char-class-hash*)))
           (if class
               (alexandria:last-elt (getf *all-characters* class))
               char)))
       str))

(defun consecutive-char-groups (char-class str &key (start 0) (end (length str)))
  (let ((regex (cdr (assoc char-class *char-scanners-inner*)))
        result)
    (ppcre:do-matches (s e regex str (nreverse result)
                         :start start :end end)
      (push (cons s e) result))))

(defun kanji-prefix (word)
  (or
   (let ((regex (format nil "^.*~a" *kanji-regex*)))
     (ppcre:scan-to-strings regex word))
   ""))

(defun unrendaku (txt &key fresh)
  (if fresh (setf txt (copy-seq txt)))
  (if (zerop (length txt)) txt
      (let* ((first-char (char txt 0))
             (cc (gethash first-char *char-class-hash*))
             (unvoiced (gethash cc *undakuten-hash*)))
        (unless unvoiced (return-from unrendaku txt))
        (let* ((pos (position first-char (getf *kana-characters* cc)))
               (new-char (char (getf *kana-characters* unvoiced) pos)))
          (setf (char txt 0) new-char)
          txt))))

(defun rendaku (txt &key fresh handakuten)
  (if fresh (setf txt (copy-seq txt)))
  (if (zerop (length txt)) txt
      (let* ((first-char (char txt 0))
             (cc (gethash first-char *char-class-hash*))
             (use-hash (if handakuten *handakuten-hash* *dakuten-hash*))
             (voiced (gethash cc use-hash)))
        (unless voiced (return-from rendaku txt))
        (let* ((pos (position first-char (getf *kana-characters* cc)))
               (new-char (char (getf *kana-characters* voiced) pos)))
          (setf (char txt 0) new-char)
          txt))))

(defun geminate (txt &key fresh)
  (if fresh (setf txt (copy-seq txt)))
  (if (zerop (length txt)) txt
      (progn (setf (char txt (1- (length txt))) #\っ) txt)))

(defun destem (word stem &optional (char-class :kana))
  "Remove `stem` characters of char-class + whatever else gets in the way from the end of `word`"
  (declare (type char-class char-class))
  (when (= stem 0) (return-from destem word))
  (let ((regex (cadr (assoc char-class *char-class-regex-mapping*)))
        pos)
    (ppcre:do-matches (s e regex word) (push s pos))
    (let ((tail (nthcdr (1- stem) pos)))
      (if tail (subseq word 0 (car tail)) ""))))

(defun match-diff (s1 s2 &aux (l1 (length s1)) (l2 (length s2)))
  "Match strings s1 and s2 optimally. Similar to ichiran/kanji:match-readings, but works with any strings."
  (cond
    ((zerop l1))
    ((zerop l2))
    (t (let ((m (mismatch s1 s2)))
         (cond
           ((not m) (values (list s1) l1))
           ((or (= l1 1) (= l2 1)) (values (list (list s1 s2)) 0))
           ((= m 0)
            (let ((best-match nil)
                  (best-match-value nil))
              (loop for c1 across s1
                 for i from 0
                 unless (zerop i)
                 do (loop for c2 across s2
                       for j from 0
                       if (and (not (zerop j)) (char= c1 c2))
                       do (multiple-value-bind (match value) (match-diff (subseq s1 i) (subseq s2 j))
                            (when (and match (or (not best-match-value) (> value best-match-value)))
                              (setf best-match (cons (list (subseq s1 0 i) (subseq s2 0 j)) match)
                                    best-match-value value)))))
              (when best-match
                (values best-match best-match-value))))
           ((= m l1)
            (values (list (subseq s1 0 (1- l1)) (list (subseq s1 (1- l1)) (subseq s2 (1- l1)))) (1- l1)))
           ((= m l2)
            (values (list (subseq s2 0 (1- l2)) (list (subseq s1 (1- l2)) (subseq s2 (1- l2)))) (1- l2)))
           (t
            (multiple-value-bind (match value) (match-diff (subseq s1 m) (subseq s2 m))
              (when match
                (values (cons (subseq s1 0 m) match) (+ value m))))))))))

(defun safe-subseq (sequence start &optional end)
  (let ((len (length sequence)))
    (when (and (<= 0 start len)
               (or (not end) (<= start end len)))
      (subseq sequence start end))))

(defun join (separator list &key key)
  (with-output-to-string (out)
    (loop for (obj . more) on list
       for string = (if key (funcall key obj) obj)
       do (princ string out)
       if more do (princ separator out))))
