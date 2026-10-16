(in-package #:ichiran/conn)

(defparameter *connection* '("jmdict" "postgres" "password" "pg"))

(defparameter *connections* '())

(in-package #:ichiran/dict)

(defparameter *jmdict-path* #p"ignore")

(defparameter *jmdict-data* #p"/root/jmdictdb/jmdictdb/data/")

(in-package #:ichiran/kanji)

(defparameter *kanjidic-path* #P"ignore")
