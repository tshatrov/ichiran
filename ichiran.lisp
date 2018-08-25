(defpackage :ichiran/maintenance
  (:nicknames :ichiran/mnt)
  (:use :cl :postmodern :ichiran/conn)
  (:import-from :ichiran/dict :load-jmdict :load-best-readings
                :add-errata :recalc-entry-stats
                :find-word :find-word-full :dict-segment :calc-score)
  (:import-from :ichiran/kanji :load-kanjidic :load-kanji-stats)
  (:export
   #:full-init
   #:load-jmdict
   #:load-best-readings
   #:load-kanjidic
   #:load-kanji-stats
   #:add-errata
   #:recalc-entry-stats
   #:custom-init
   #:compare-queries
   #:display-seq-set
   #:switch-connection))

(in-package :ichiran/maintenance)

(defun full-init ()
  (format t "Initializing ichiran/dict...~%")
  (load-jmdict)
  (format t "Calculating readings...~%")
  (load-best-readings :reset t)
  (format t "Initializing ichiran/kanji...~%")
  (load-kanjidic)
  (format t "Calculating kanji stats...~%")
  (load-kanji-stats)
  )

(defun custom-init (dict-connection &key jmdict-path jmdict-data kanjidic-path)
  (let ((ichiran/dict::*jmdict-path* (or jmdict-path ichiran/dict::*jmdict-path*))
        (ichiran/dict::*jmdict-data* (or jmdict-data ichiran/dict::*jmdict-data*))
        (ichiran/kanji::*kanjidic-path* (or kanjidic-path ichiran/kanji::*kanjidic-path*)))
    (let-db dict-connection
      (full-init))))

(defmacro compare-queries (conn1 conn2 &body query)
  (alexandria:with-gensyms (q1 q2)
    `(let ((,q1 (with-db ,conn1 (query ,@query)))
           (,q2 (with-db ,conn2 (query ,@query))))
       (values (set-difference ,q1 ,q2 :test 'equal)
               (set-difference ,q2 ,q1 :test 'equal)))))

;; example queries
;; find all [uk] tag changes
;; (:select 'seq :distinct :from 'sense-prop :where (:and (:= 'tag "misc") (:= 'text "uk")))
;; find common kana changes
;; (:select 'seq 'text :distinct :from 'kana-text :where (:not (:is-null 'common)))
;; find counters
;; (:select 'seq :distinct :from 'sense-prop :where (:and (:= 'tag "pos") (:= 'text "ctr")))
;; find expressions
;; (:select 'seq :distinct :from 'sense-prop :where (:and (:= 'tag "pos") (:= 'text "exp")))

(defmacro display-seq-set (seq-set entry-var test &key (conn 'ichiran/conn:*connection*))
  `(with-db ,conn
     (dolist (,entry-var (select-dao 'ichiran/dict::entry (:in 'seq (:set ,seq-set))))
       (when ,(or test t)
         (print (ichiran/dict::entry-digest ,entry-var))))))

;; example test
;; (and (not (primary-nokanji entry)) (<= (length (get-kana entry)) 3) (not (eql (common entry) :null)))


(defun switch-connection (conn &key reset)
  (with-db conn
    (ichiran/dict:init-suffixes t reset)
    (ichiran/dict:init-counters reset))
  (switch-conn-vars conn)
  (setf *database* (apply 'connect *connection*)))


(defun regex-file (filename regex)
  (let ((contents (uiop:read-file-string filename)))
    (ppcre:all-matches-as-strings regex contents)))


(defun get-hardcoded-constants ()
  (loop with regex = "\\b\\d{7,}\\b"
     for component in (asdf:component-children (asdf:find-system :ichiran))
     for name = (asdf:component-name component)
     for filename = (asdf:component-pathname component)
     unless (equal name "tests")
     nconcing (remove-duplicates (mapcar 'parse-integer (regex-file filename regex)))))


(defun collect-entries (seq-set &key (conn ichiran/conn:*connection*))
  (with-db conn
    (let ((hash (make-hash-table)))
      (loop for entry in (select-dao 'ichiran/dict::entry (:in 'seq (:set seq-set)))
         do (setf (gethash (ichiran/dict::seq entry) hash) entry))
      (values
       (loop for seq in seq-set unless (gethash seq hash) collect seq)
       hash))))
