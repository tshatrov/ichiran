(in-package #:ichiran/characters)

(defparameter *sokuon-characters* '(:sokuon "っッ"))

(defparameter *iteration-characters* '(:iter "ゝヽ" :iter-v "ゞヾ"))

(defparameter *modifier-characters* '(:+a "ぁァ" :+i "ぃィ" :+u "ぅゥ" :+e "ぇェ" :+o "ぉォ"
                                      :+ya "ゃャ" :+yu "ゅュ" :+yo "ょョ"
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


(defmacro hash-from-list (var list)
  (alexandria:with-gensyms (hash key val)
    `(defparameter ,var
       (let ((,hash (make-hash-table)))
         (loop for (,key ,val) on ,list
              do (setf (gethash ,key ,hash) ,val))
         ,hash))))
       
(hash-from-list *dakuten-hash* 
                '(:ka :ga :ki :gi :ku :gu :ke :ge :ko :go
                  :sa :za :shi :ji :su :zu :se :ze :so :zo
                  :ta :da :chi :dji :tsu :dzu :te :de :to :do
                  :ha :ba :hi :bi :fu :bu :he :be :ho :bo))

(defun voice-char (cc)
  "Returns a voiced form of character class, or the same character class"
  (gethash cc *dakuten-hash* cc))

(defparameter *punctuation-marks*
  '("【" " [" "】" "] "
    "、" ", " "，" ", "
    "。" ". " "・" " " "　" " "
    "「" " \"" "」" "\" " "゛" "\""
    "『" " «"  "』" "» "
    "〜" " - " "：" ": " "！" "! " "？" "? " "；" "; "))

(defparameter *full-width-chars* "０１２３４５６７８９ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ＃＄％＆（）＊＋／〈＝〉？＠［］＾＿‘｛｜｝～")

(defparameter *half-width-chars* "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ#$%&()*+/<=>?@[]^_`{|}~")


(defparameter *katakana-regex* "[ァ-ヺヽヾー]")

(defparameter *hiragana-regex* "[ぁ-ゔゝゞー]")

(defparameter *kanji-regex* "[々一-龯]")

(defparameter *nonword-regex* "[^々一-龯ァ-ヺヽヾぁ-ゔゝゞー]")

(defparameter *char-scanners*
  (mapcar (lambda (pair) (cons (car pair) (ppcre:create-scanner (format nil "^~a+$" (cadr pair)))))
          `((:katakana ,*katakana-regex*)
            (:hiragana ,*hiragana-regex*)
            (:kanji ,*kanji-regex*)
            (:kana ,(format nil "(~a|~a)" *katakana-regex* *hiragana-regex*))
            (:traditional ,(format nil "(~a|~a)" *hiragana-regex* *kanji-regex*))
            (:nonword ,*nonword-regex*))))

(defun test-word (word char-class)
  (declare (type (member :katakana :hiragana :kanji :kana :traditional :nonword) char-class))
  (let ((regex (cdr (assoc char-class *char-scanners*))))
    (ppcre:scan regex word)))
    
(defun simplify-ngrams (str map)
  (let* ((alist (loop for (from to) on map by #'cddr collect (cons from to)))
         (scanner (ppcre:create-scanner (cons :alternation (mapcar #'car alist)))))
    (ppcre:regex-replace-all scanner str 
                             (lambda (match &rest rest)
                               (declare (ignore rest))
                               (cdr (assoc match alist :test #'equal)))
                             :simple-calls t)))

(defun normalize (str)
  (loop for i from 0 below (length str)
       for char = (char str i)
       for pos = (position char *full-width-chars*)
       if pos do (setf (char str i) (char *half-width-chars* pos)))
  (setf str (simplify-ngrams str *punctuation-marks*)))
  
(defun split-by-regex (regex str)
  (remove-if (lambda (seg) (= (length seg) 0))
             (ppcre:split regex str :with-registers-p t)))

(defun basic-split (str)
  "splits string into segments of katakana, traditional and misc characters"
  (let ((split1 (split-by-regex (format nil "(~a+)" *nonword-regex*) str)))
    (loop for segment in split1
         for misc = (test-word segment :nonword) then (not misc)
         append (if misc
                    (list (cons :misc segment))
                    (mapcar (lambda (seg) (cons :word seg))
                            (split-by-regex "([ァ-ヺヽヾ][ァ-ヺヽヾー]*)" segment))))))
                      
  


