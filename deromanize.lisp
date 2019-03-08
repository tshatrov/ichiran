;; detransliteration module

(in-package #:ichiran)

(defstruct (rmap-item (:conc-name rmi-)) text kana next)

(csv-hash *romaji-kana* ("data/romaji-map.csv" :relative :ichiran)
          ((text kana next) text (make-rmap-item :text text :kana kana :next next))
          (val val))

(load-romaji-kana)

(defun has-successors (strings)
  (let ((hash (make-hash-table :test 'equal)))
    (loop for s in strings
       do (loop for end from 1 below (length s)
             for ss = (subseq s 0 end)
             do (setf (gethash ss hash) t)))
    hash))

(defparameter *romaji-kana-next* (has-successors (alexandria:hash-table-keys *romaji-kana*)))

(defstruct (kana-representation (:conc-name kr-)) (canonical "") (pattern "") (rest "") (branch 0))

(defun kr-concat (kr1 kr2)
  (make-kana-representation
   :canonical (concatenate 'string (kr-canonical kr1) (kr-canonical kr2))
   :pattern (concatenate 'string (kr-pattern kr1) (kr-pattern kr2))
   :rest (kr-rest kr2)
   :branch (kr-branch kr1)))

(defun possible-long-vowel-p (text)
  (unless (alexandria:emptyp text)
    (let ((ch (elt text (1- (length text)))))
      (find ch '(#\o #\u)))))

(defun apply-rmap-item (s rmi)
  (let ((kana (rmi-kana rmi)))
    (make-kana-representation
     :canonical kana
     :pattern (if (possible-long-vowel-p (rmi-text rmi)) (concatenate 'string kana "う?") kana)
     :rest (concatenate 'string
                        (or (rmi-next rmi) "")
                        (subseq s (length (rmi-text rmi)))))))

(defun romaji-next (s)
  (loop
     for end from 1 to (length s)
     for ss = (subseq s 0 end)
     for rmi = (get-romaji-kana ss)
     when rmi collect (apply-rmap-item s rmi)
     while (gethash ss *romaji-kana-next*)))


(defun join-branches (branches)
  (let* ((b0 (car branches))
         (tails (loop for b in branches
                   collect (subseq (kr-pattern b) (kr-branch b))))
         (head (subseq (kr-pattern b0) 0 (kr-branch b0)))
         (joined-kana (format nil "~a(~{~a~^|~})" head tails)))
    (make-kana-representation
     :canonical (reduce
                 (lambda (x y) (if (<= (length x) (length y)) x y))
                 (mapcar 'kr-canonical branches))
     :pattern joined-kana
     :rest (kr-rest b0)
     :branch (length joined-kana))))

(defun branches-next (branches)
  (flet ((key (b) (length (kr-rest b))))
    (let* ((kr (car branches))
           (new-branches
            (sort
             (nconc (loop for k in (romaji-next (kr-rest kr)) collect (kr-concat kr k)) (cdr branches))
             '>
             :key #'key))
           (new-len (length new-branches)))
      (if (= new-len 1)
          (setf (kr-branch (car new-branches)) (length (kr-pattern (car new-branches)))))
      (if (and (> new-len 1) (= (key (car new-branches)) (key (car (last new-branches)))))
          (list (join-branches new-branches))
          new-branches))))

(defun romaji-kana (s)
  (loop with branches = (list (make-kana-representation :rest (string-downcase s)))
     with finished = nil
     while branches
     do (setf branches (branches-next branches))
     when (and branches (alexandria:emptyp (kr-rest (car branches))))
     do (setf finished (car branches) branches nil)
     finally
       (when finished
         (return (values (kr-canonical finished) (format nil "^~a$" (kr-pattern finished)))))))

(defun romaji-suggest (s)
  (multiple-value-bind (canon pattern) (romaji-kana s)
    (when pattern
      (multiple-value-bind (pkanji pkana) (find-kanji-for-pattern pattern)
        (let ((hiragana (remove-duplicates (cons canon pkana) :test 'equal :from-end t)))
          (jsown:new-js
            ("hiragana" hiragana)
            ("katakana" (mapcar 'as-katakana hiragana))
            ("kanji" pkanji)))))))
