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
               )
  :components ((:file "package")
               (:file "characters")
               (:file "tokenize")
               (:file "romanize")
               (:file "dict-errata")
               (:file "dict")
               (:file "dict-grammar")
               (:file "dict-split")
               (:file "kanji")
               (:file "ichiran")
               (:file "tests"))
  :perform (test-op 
            (o s)
            (uiop:symbol-call :ichiran/test :run-all-tests)))

