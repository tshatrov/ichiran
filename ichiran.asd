;;;; ichiran.asd

(in-package :asdf)

(defsystem #:ichiran
  :serial t
  :description "Ichiran means list in Japanese"
  :author "Timofei Shatrov <timofei.shatrov@example.com>"
  :license "MIT"
  :depends-on (#:cl-ppcre
               #:alexandria
               #:split-sequence
               #:postmodern
               #:cxml
               #:cl-unicode
               #:cl-csv
               #:lisp-unit
               #:bordeaux-threads
               #:jsown
               #:lparallel
               #:diff
               #:cl+ssl
               )
  :components ((:file "package")
               (:file "characters")
               (:file "numbers")
               (:file "conn")
               (:file "dict-errata")
               (:file "dict")
               (:file "dict-grammar")
               (:file "dict-split")
               (:file "dict-counters")
               (:file "dict-load")
               (:file "romanize")
               (:file "dict-custom")
               (:file "deromanize")
               (:file "kanji")
               (:file "ichiran")
               (:file "tests"))
  :perform (test-op
            (o s)
            (uiop:symbol-call :ichiran/test :run-all-tests)))


(defsystem #:ichiran/cli
  :serial t
  :description "Command line interface for Ichiran"
  :author "Timofei Shatrov <timofei.shatrov@example.com>"
  :license "MIT"
  :depends-on (#:ichiran
               #:unix-opts
               )
  :build-operation "program-op"
  :build-pathname "ichiran-cli"
  :entry-point "ichiran/cli::main"
  :components ((:file "cli")))


#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
