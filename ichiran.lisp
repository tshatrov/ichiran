(defpackage :ichiran/maintenance
  (:nicknames :ichiran/mnt)
  (:use :cl :postmodern :ichiran/conn)
  (:import-from :ichiran/characters :split-by-regex)
  (:import-from :ichiran/dict :load-jmdict :load-best-readings
                :recalc-entry-stats
                :init-suffixes :init-suffixes-running-p
                :find-word :find-word-full :dict-segment :calc-score
                :entry-info-short :entry-info-long)
  (:import-from :ichiran/kanji :load-kanjidic :load-kanji-stats)
  (:export
   #:with-db
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
   #:switch-connection
   #:diff-content
   #:show-missing-constants
   #:get-seq-changes
   #:show-diffs))

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

;; showing results from above
;; (display-seq-set (car /) () :conn :dec23)
;; (display-seq-set (cadr //) () :conn :jan25)

(defmacro display-seq-set (seq-set (&optional entry-var test) &key (conn 'ichiran/conn:*connection*))
  (alexandria:with-gensyms (x)
    (unless entry-var (setf entry-var (gensym "EV")))
    `(with-db ,conn
       (dolist (,entry-var (select-dao 'ichiran/dict::entry
                                       (:in 'seq (:set (mapcar (lambda (,x) (if (listp ,x) (car ,x) ,x)) ,seq-set)))))
         (when ,(or test t)
           (print (ichiran/dict::entry-digest ,entry-var)))))))

;; example test
;; (and (not (primary-nokanji entry)) (<= (length (get-kana entry)) 3) (not (eql (common entry) :null)))


(defun switch-connection (conn &key reset)
  (with-db conn
    (init-all-caches reset)
    (ichiran/dict:init-suffixes t reset))
  (switch-conn-vars conn)
  (setf *database* (apply 'connect *connection*)))


(defun regex-file (filename regex)
  (let ((contents (uiop:read-file-string filename)))
    (ppcre:all-matches-as-strings regex contents)))


(defun get-hardcoded-constants (&key regex)
  (loop with regex = (or regex "\\b\\d{7,}\\b")
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


(defun show-missing-constants (old-conn new-conn)
  (display-seq-set (collect-entries (get-hardcoded-constants) :conn new-conn) () :conn old-conn))


;; (get-seq-changes old new :regex "(?<=\\(add-sense-prop )\\d{7,}") to find seq changes in sense-prop stuff

(defun get-seq-changes (old-conn new-conn &key regex seqs)
  (let ((seqs (or seqs (get-hardcoded-constants :regex regex)))
        (diffs (make-hash-table :test 'equal)))
    (multiple-value-bind (old new)
        (compare-queries old-conn new-conn
          (:select 'seq 'content :from 'entry :where (:in 'seq (:set seqs))))
      (loop for (seq content) in old
         do (setf (gethash seq diffs) (cons content nil)))
      (loop for (seq content) in new
         for diff = (gethash seq diffs)
         if diff do (setf (cdr (gethash seq diffs)) content)
         else do (setf (gethash seq diffs) (cons nil content))))
    diffs))

(defun diff-content (old new &key (short t))
  (let* ((re-newline "[\\r\\n]+")
         (old (and old (split-by-regex re-newline old)))
         (new (and new (split-by-regex re-newline new))))
    (cond ((and short (not new)) :gone)
          ((and short (not old)) :new)
          (t (with-output-to-string (s)
               (diff:render-diff (diff:generate-seq-diff 'diff:unified-diff old new) s))))))

(defun show-diffs (diffs &key (short t) (entry-info t))
  (loop for seq in (sort (alexandria:hash-table-keys diffs) '<)
     for (old . new) = (gethash seq diffs)
     collect (cons (if entry-info (entry-info-long seq) seq) (diff-content old new :short short))))

(defun add-errata (&optional conn)
  "Public-facing version of ichiran/dict::add-errata"
  (with-db conn
    (ichiran/dict::add-errata)))
