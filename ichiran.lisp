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
   ))

(in-package :ichiran/maintenance)

(defun full-init ()
  (format t "Initializing ichiran/dict...~%")
  (load-jmdict)
  (format t "Calculating readings...~%")
  (load-best-readings)
  (format t "Initializing ichiran/kanji...~%")
  (load-kanjidic)
  (format t "Calculating kanji stats...~%")
  (load-kanji-stats)
  )
