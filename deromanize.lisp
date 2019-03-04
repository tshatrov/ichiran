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

(defstruct (kana-representation (:conc-name kr-)) (canonical "") (pattern ""))

(defun kr-concat (kr1 kr2)
  (make-kana-representation
   :canonical (concatenate 'string (kr-canonical kr1) (kr-canonical kr2))
   :pattern (concatenate 'string (kr-pattern kr1) (kr-pattern kr2))))

(defun apply-rmap-item (s rmi)
  (let ((kana (rmi-kana rmi))
        (rest (concatenate 'string
                           (or (rmi-next rmi) "")
                           (subseq s (length (rmi-text rmi))))))
    (cons
     (make-kana-representation
      :canonical kana
      :pattern (if (alexandria:ends-with #\o (rmi-text rmi)) (concatenate 'string kana "う?") kana))
     rest)))

(defun romaji-next (s)
  (loop
     for end from 1 to (length s)
     for ss = (subseq s 0 end)
     for rmi = (get-romaji-kana ss)
     when rmi collect (apply-rmap-item s rmi)
     while (gethash ss *romaji-kana-next*)))

(defun romaji-kana (s)
  (loop with branches = (list (cons (make-kana-representation) s))
     with finished = nil
     while branches
     do (setf branches
              (loop for (kana . romaji) in branches
                 if (alexandria:emptyp romaji) do (push kana finished)
                 else nconc (loop for (k . r) in (romaji-next romaji)
                               collect (cons (kr-concat kana k) r))))
     finally (return (nreverse finished))))
