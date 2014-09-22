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

(defun add-errata ()
  ;;; gozaimashita / gozaimashitara
  (add-conj 1612690 '(2 "exp" :null :null)
            "ございました")
  (add-conj 1612690 '(11 "exp" :null :null)
            "ございましたら")
  )
