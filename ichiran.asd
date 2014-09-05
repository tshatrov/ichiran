;;;; ichiran.asd

(asdf:defsystem #:ichiran
  :serial t
  :description "Ichiran means list in Japanese"
  :author "Timofei Shatrov <timofei.shatrov@example.com>"
  :license "MIT"
  :depends-on (#:cl-ppcre
               #:alexandria
               #:split-sequence
               )
  :components ((:file "package")
               (:file "characters")
               (:file "romanize")
               (:file "ichiran")))

