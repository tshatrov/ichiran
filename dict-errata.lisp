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
        (update-dao entry)))))

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


(defun add-deha-ja-readings ()
  (let ((deha-list (query (:select 'conj.seq 'kt.text :distinct
                                   :from (:as 'conjugation 'conj) (:as 'kana-text 'kt)
                                   :where (:and (:= 'conj.from 2089020)
                                                (:= 'kt.seq 'conj.seq)
                                                (:like 'kt.text "では%"))))))
    (loop for (seq deha) in deha-list
         for ja = (concatenate 'string "じゃ" (subseq deha 2))
         do (add-reading seq ja))))

(defun add-errata ()
  (add-deha-ja-readings)

  ;;; add sense for な 
  (add-sense 2029110 4 "(used with nouns) な-adjective")
  ;;; gozaimashita / gozaimashitara
  (add-conj 1612690 '(2 "exp" :null :null)
            "ございました")
  (add-conj 1612690 '(11 "exp" :null :null)
            "ございましたら")
  ;; きみ / キミ
  (add-reading 1247250 "キミ")

  ;;; delete sense-prop uk for 生る
  (delete-sense-prop 1611000 "misc" "uk")
  ;; 仕手 (して) 
  (delete-sense-prop 1305070 "misc" "uk")
  ;; 品 ( しな)
  (delete-sense-prop 1583470 "misc" "uk")

  (add-sense-prop 1360480 0 "misc" "uk")

  ;; unset common flag for choice kana readings
  (loop for kt in (select-dao 'kana-text (:= 'seq 1310920))
       do (setf (slot-value kt 'common) :null)
       (update-dao kt))

  ;; delete noun sense for と
  (delete-senses 1008490 (lambda (prop) (and (equal (text prop) "n") (equal (tag prop) "pos"))))

  ;; delete prt sense for たい
  (delete-senses 2017560 (lambda (prop) (and (equal (text prop) "prt") (equal (tag prop) "pos"))))

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
