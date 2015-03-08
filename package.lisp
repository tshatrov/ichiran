;;;; package.lisp

(defpackage #:ichiran/characters
  (:use #:cl)
  (:export :*sokuon-characters* :*iteration-characters* :*modifier-characters*
           :*kana-characters* :*all-characters* :*char-class-hash*
           :*katakana-regex* :*hiragana-regex* :*kanji-regex* :test-word
           :hash-from-list :voice-char :simplify-ngrams :normalize
           :split-by-regex :basic-split :mora-length :count-char-class
           :as-hiragana :sequential-kanji-positions
           :*undakuten-hash* :*dakuten-hash*
           :kanji-prefix
           :unrendaku
           :rendaku
           :collect-char-class))

(defpackage #:ichiran/tokenize
  (:use #:cl)
  (:export :segment))

(defpackage #:ichiran/dict
  (:use #:cl #:postmodern #:ichiran/characters #:split-sequence)
  (:export :simple-segment :dict-segment
           :word-info :word-info-type :word-info-text
           :word-info-kana :word-info-score :map-word-info-kana
           :word-info-str :word-info-components :word-info-alternative
           :word-info-start :word-info-end :word-info-json
           :word-info-gloss-json
           :init-suffixes :init-suffixes-running-p
           :node-text
           :get-kanji-words))

(defpackage #:ichiran
  (:use #:cl #:ichiran/characters #:ichiran/dict)
  (:export :romanize :romanize-word :romanize* :romanize-word-info
           :generic-romanization :generic-hepburn :kana-table
           :simplified-hepburn :simplifications
           :*hepburn-basic* :*hepburn-simple* :*hepburn-passport*
           :*hepburn-traditional* :*hepburn-modified*
           :kunrei-siki :*kunrei-siki*
           :*default-romanization-method*))

(defpackage #:ichiran/kanji
  (:use #:cl #:postmodern #:ichiran #:ichiran/characters #:ichiran/dict)
  (:export
   #:kanji-info-json
   #:match-readings
   #:kanji-word-stats
   #:get-readings
   #:get-normal-readings))

(uiop:define-package #:ichiran/all
    (:use #:ichiran/characters #:ichiran/dict #:ichiran #:ichiran/kanji)
    (:reexport :ichiran :ichiran/dict :ichiran/characters :ichiran/kanji))

(defpackage #:ichiran/test
  (:use #:cl #:ichiran/all #:lisp-unit)
  (:export :run-all-tests))
  
