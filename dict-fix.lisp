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

(defun wanakatta-fix ()
  "Fix erroneous conjugations for negative conditional forms of v5u"
  (loop with max-seq = (query (:select (:max 'seq) :from 'entry) :single)
        with next-seq = (1+ max-seq)
     for prop in (select-dao 'conj-prop (:and (:= 'pos "v5u")
                                              (:= 'conj-type 11)
                                              'neg (:not 'fml)))
       for conj = (get-dao 'conjugation (conj-id prop))
       for seq = (if (eql (seq-via conj) :null)
                     (seq-from conj)
                     (seq-via conj)) ;; via conj should always be null though
       for matrix = (conjugate-entry-inner seq :conj-types '(11) :as-posi (list "v5u"))
       for mvals = (alexandria:hash-table-values matrix)
       for readings = (and mvals (aref (car mvals) 1 0))
       when readings
       do (print readings)
       (when (insert-conjugation readings :seq next-seq
                                    :via (seq-via conj) :from (seq-from conj)
                                    :conj-type 11 :neg t :fml nil :pos "v5u")
            (incf next-seq))
       (delete-dao prop)
       ))

(defun delete-empty-conjs ()
  (let* ((conjs (query (:select 'conj.id 'conj.seq
                                :from (:as 'conjugation 'conj)
                                :left-join (:as 'conj-prop 'cp)
                                :on (:= 'conj.id 'cp.conj-id) :where (:is-null 'cp.id))))
         (conj-ids (mapcar 'car conjs))
         (seqs (mapcar 'cadr conjs)))
    (query (:delete-from 'conj-source-reading :where (:in 'conj-id (:set conj-ids))))
    (query (:delete-from 'conjugation :where (:in 'id (:set conj-ids))))
    (let ((entries (query (:select 'entry.seq :from 'entry :left-join (:as 'conjugation 'conj)
                                   :on (:and (:= 'conj.seq 'entry.seq) (:not (:in 'conj.id (:set conj-ids))))
                                   :where (:and (:not 'entry.root-p) (:is-null 'conj.id) (:in 'entry.seq (:set seqs))))
                          :column)))
      (query (:delete-from 'entry :where (:in 'seq (:set entries)))))))

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


(defun add-kudasari-conjs ()
  (loop with max-seq = (query (:select (:max 'seq) :from 'entry) :single)
     with next-seq = (1+ max-seq)
     for prop in (select-dao 'conj-prop (:and (:= 'pos "v5aru") (:= 'conj-type 3) (:not 'neg) (:not 'fml)))
     for conj = (get-dao 'conjugation (conj-id prop))
     for seq = (if (eql (seq-via conj) :null)
                   (seq-from conj)
                   (seq-via conj)) ;; via conj should always be null though
     for matrix = (conjugate-entry-inner seq :conj-types '(3) :as-posi (list (pos prop)))
     for mvals = (alexandria:hash-table-values matrix)
     for readings = (remove-if-not (lambda (x) (= (fifth x) 2)) (and mvals (aref (car mvals) 0 0)))
     when readings do (print readings)
       (when (insert-conjugation readings :seq next-seq :via (seq-via conj) :from (seq-from conj)
                                 :conj-type 3 :neg nil :fml nil :pos (pos prop))
         (incf next-seq))))


(defun delete-forbidden-conjs ()
  (let ((seqs (query (:select 'conj.seq :from (:as 'conjugation 'conj)
                              :where (:in 'conj.from (:set *do-not-conjugate-seq*))) :column)))
    (query (:delete-from 'entry :where (:in 'seq (:set seqs))))))


(defun fix-da-conjs (&key (seq 2089020))
  (let ((seqs (query (:select 'conj.seq :from (:as 'conjugation 'conj)
                              :where (:in 'conj.from (:set (list seq)))) :column)))
    (query (:delete-from 'entry :where (:and (:not 'root-p) (:in 'seq (:set seqs))))))
  (loop for kt in (select-dao 'kana-text (:and (:= 'text "じゃ") (:= 'seq seq)))
     do (setf (slot-value kt 'conjugate-p) nil) (update-dao kt))
  (conjugate-entry-outer seq))

(defun fix-da-conjs-2 ()
  ;; fix for when conjugation だ -> で becomes the same seq as でる -> で
  (let ((conj-id (car (query (:select 'c1.id :from (:as 'conjugation 'c1) (:as 'conjugation 'c2) :where
                                      (:and (:= 'c1.from 2089020) (:= 'c2.from 2831062) (:= 'c1.seq 'c2.seq))) :column))))
    (when conj-id
      (let ((conj (get-dao 'conjugation conj-id)))
        (setf (slot-value conj 'seq) 2028980)
        (update-dao conj)))))


(defun fix-duplicate-cities ()
  (loop for (seq text) in (query (:select 'sense.seq 'gloss.text
                                  :from 'gloss :inner-join 'sense
                                  :on (:= 'gloss.sense-id 'sense.id)
                                  :where (:like 'gloss.text "% (city)%")
                                  :group-by 'sense.seq 'gloss.text
                                  :having (:> (:count 'gloss.id) 1)))
        for sense-ids = (query (:order-by
                                (:select 'sense.id :from 'gloss :inner-join 'sense
                                 :on (:= 'gloss.sense-id 'sense.id)
                                 :where (:and (:= 'sense.seq seq) (:= 'gloss.text text)))
                                'sense.ord)
                               :column)
        for to-delete = (subseq sense-ids 1)
        do (format t "Deleting extra senses ~a for '~a'~%" to-delete text)
           (query (:delete-from 'sense :where (:in 'id (:set to-delete))))))


(defun add-ki-conjs ()
  (let ((seqs (query (:select 'seq :distinct :from 'sense-prop
                                             :where (:and (:not (:in 'seq (:set *do-not-conjugate-seq*)))
                                             (:= 'tag "pos")
                                             (:= 'text "adj-i")))
                     :column)))
    (loop for cnt from 1
          for seq in seqs
          do (conjugate-entry-outer seq :conj-types (list +conj-adjective-literary+) :as-posi '("adj-i"))
          if (zerop (mod cnt 100)) do (format t "~a entries processed~%" cnt))))
