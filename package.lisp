;;;; package.lisp

(defpackage #:ichiran/characters
  (:use #:cl)
  (:export :*sokuon-characters* :*iteration-characters* :*modifier-characters*
           :*kana-characters* :*all-characters* :*char-class-hash*
           :*katakana-regex* :*hiragana-regex* :*kanji-regex* :test-word
           :hash-from-list :voice-char :simplify-ngrams :normalize
           :split-by-regex :basic-split
           ))

(defpackage #:ichiran/tokenize
  (:use #:cl)
  (:export :segment))

(defpackage #:ichiran/dict
  (:use #:cl #:postmodern #:ichiran/characters)
  (:export :simple-segment
           :word-info :word-info-type :word-info-text
           :word-info-kana :word-info-score
           ))

(defpackage #:ichiran
  (:use #:cl #:ichiran/characters #:ichiran/dict)
  (:export :romanize :romanize-word
           :generic-romanization :generic-hepburn :kana-table
           :simplified-hepburn :simplifications
           :*hepburn-basic* :*hepburn-simple* :*hepburn-passport*
           :*hepburn-traditional* :*hepburn-modified*
           :*default-romanization-method*))

