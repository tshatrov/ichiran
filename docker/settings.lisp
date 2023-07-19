(in-package #:ichiran/conn)

(setf postmodern:*default-use-ssl*
    (let* ((env-ssl (uiop:getenv "ICHIRAN_SSL")))
        (if env-ssl
            (cond
                ((string= env-ssl "no") :no)
                ((string= env-ssl "try") :try)
                ((string= env-ssl "require") :require)
                ((string= env-ssl "yes") :yes)
                ((string= env-ssl "full") :full)
                (t (error (format nil "Invalid environment variable ICHIRAN_SSL=~a. Expected no, try, require, yes or full." env-ssl))))
            :no)))

(defparameter *connection* '("jmdict0122" "postgres" "password" "pg"))

(defparameter *connections* '((:old "jmdict_old" "postgres" "password" "localhost")
                              (:test "jmdict_test" "postgres" "password" "localhost")))

(in-package #:ichiran/dict)

(defparameter *jmdict-path* #p"/home/you/dump/JMdict_e")

(defparameter *jmdict-data* #p"/root/jmdictdb/jmdictdb/data/")

(in-package #:ichiran/kanji)

(defparameter *kanjidic-path* #P"/home/you/dump/kanjidic2.xml")
