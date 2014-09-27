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
  
(defun add-conj (seq-from options
                 &rest readings)
  (unless (find-conj seq-from options)
    (destructuring-bind (conj-type pos neg fml) options
      (let ((next-seq (1+ (query (:select (:max 'seq) :from 'entry) :single))))
        (make-dao 'entry :seq next-seq :content "")
        (loop with ord-r = 0 and ord-k = 0
           for reading in readings
           for is-kana = (test-word reading :kana)
           for table = (if is-kana 'kana-text 'kanji-text)
           for ord = (if is-kana ord-r ord-k)
           do (make-dao table :seq next-seq :text reading :ord ord :common :null)
             (if is-kana (incf ord-r) (incf ord-k)))
        (let ((conj (make-dao 'conjugation :seq next-seq :from seq-from)))
          (make-dao 'conj-prop :conj-id (id conj)
                    :pos pos :conj-type conj-type :neg neg :fml fml))))))

(defun add-reading (seq reading &optional (common :null))
  (let* ((is-kana (test-word reading :kana))
         (table (if is-kana 'kana-text 'kanji-text))
         (entry (get-dao 'entry seq)))
    (when (not (select-dao table (:and (:= 'seq seq) (:= 'text reading))))
      (let ((ord (1+ (query (:select (:max 'ord) :from table :where (:= 'seq seq)) :single))))
        (make-dao table :text reading :seq seq :ord ord :common common)
        (if is-kana
            (incf (n-kana entry))
            (incf (n-kanji entry)))
        (update-dao entry)))))

(defun delete-sense-prop (seq tag text)
  (let ((props (select-dao 'sense-prop (:and (:= 'seq seq) (:= 'tag tag) (:= 'text text)))))
    (mapc #'delete-dao props)))

(defun add-errata ()
  ;;; gozaimashita / gozaimashitara
  (add-conj 1612690 '(2 "exp" :null :null)
            "ございました")
  (add-conj 1612690 '(11 "exp" :null :null)
            "ございましたら")
  ;;;  いる / る
  (add-reading 1577980 "る")
  ;; きみ / キミ
  (add-reading 1247250 "キミ")

  ;;; delete sense-prop uk for 生る
  (delete-sense-prop 1611000 "misc" "uk")
  )


;; Additional conjugations

(defconstant +conj-adverbial+ 50)

(defun errata-conj-description-hook (hash)
  (setf (gethash +conj-adverbial+ hash) "Adverbial"))

(defun errata-conj-rules-hook (hash)
  (let ((pos (get-pos-index "adj-i")))
    (push 
     (make-conjugation-rule
      pos +conj-adverbial+ nil nil 1 
      1 "く" "" "")
     (gethash pos hash nil))))
