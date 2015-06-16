(defpackage :ichiran/maintenance
  (:nicknames :ichiran/mnt)
  (:use :cl)
  (:import-from :ichiran/dict :load-jmdict :load-best-readings
                :add-errata :recalc-entry-stats)
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
   ))

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

(defun custom-init (dict-connection &key jmdict-path jmdict-data kanjidic-path kanji-connection)
  (let ((ichiran/dict::*connection* dict-connection)
        (ichiran/kanji::*connection* (or kanji-connection dict-connection))
        (ichiran/dict::*jmdict-path* (or jmdict-path ichiran/dict::*jmdict-path*))
        (ichiran/dict::*jmdict-data* (or jmdict-data ichiran/dict::*jmdict-data*))
        (ichiran/kanji::*kanjidic-path* (or kanjidic-path ichiran/kanji::*kanjidic-path*)))
    (full-init)))
