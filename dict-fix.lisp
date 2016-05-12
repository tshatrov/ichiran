;; one-time fixes that don't even need to be loaded

(in-package #:ichiran/dict)


(defun masen-fix ()
  "Fix erroneous conjugations for formal negative non-past forms of v1 and v1-s"
  (loop with max-seq = (query (:select (:max 'seq) :from 'entry) :single)
        with next-seq = (1+ max-seq)
     for prop in (select-dao 'conj-prop (:and (:in 'pos (:set "v1" "v1-s"))
                                                 (:= 'conj-type 1)
                                                 'neg 'fml))
       for conj = (get-dao 'conjugation (conj-id prop))
       for seq = (if (eql (seq-via conj) :null)
                     (seq-from conj)
                     (seq-via conj)) ;; via conj should always be null though
       for matrix = (conjugate-entry-inner seq :conj-types '(1) :as-posi (list (pos prop)))
       for mvals = (alexandria:hash-table-values matrix)
       for readings = (and mvals (aref (car mvals) 1 1))
       when readings
       do (print readings)
       (when (insert-conjugation readings :seq next-seq 
                                    :via (seq-via conj) :from (seq-from conj)
                                    :conj-type 1 :neg t :fml t :pos (pos prop))
            (incf next-seq))
          (delete-dao prop)
       ))


(defun load-sense-props (tag-list &aux (count 0))
  (loop for (seq content) in (query (:select 'seq 'content :from 'entry :where (:not (:= 'content ""))))
     do
    (when (some (lambda (tag) (search (format nil "<~a>" tag) content)) tag-list)
      (incf count)
      (let* ((parsed (cxml:parse content (cxml-dom:make-dom-builder)))
             (sense-nodes (dom:get-elements-by-tag-name parsed "sense")))
        (do-node-list-ord (ord node sense-nodes)
          (let ((sense (select-dao 'sense (:and (:= 'seq seq) (:= 'ord ord)))))
            (when sense
              (let ((sense-id (id (car sense))))
                (loop for tag in tag-list
                   do (insert-sense-traits node tag sense-id seq)))))))
      (when (zerop (mod count 500))
        (format t "~a entries processed~%" count)))))


(defun add-gozaimasu-source-readings ()
  (loop for conj in (select-dao 'conjugation (:= 'from 1612690))
     do (delete-conjugation (seq conj) (seq-from conj)))
  (add-conj 1612690 '(2 "exp" :null :null)
            '(("ございます" "ございました")))
  (add-conj 1612690 '(11 "exp" :null :null)
            '(("ございます" "ございましたら")))
  (add-conj 1612690 '(1 "exp" t :null)
            '(("ございます" "ございません"))))
