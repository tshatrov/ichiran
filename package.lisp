;;;; package.lisp

(defpackage :ichiran/characters
  (:use :cl)
  (:export :*sokuon-characters* :*iteration-characters* :*modifier-characters*
   :*kana-characters* :*all-characters* :*char-class-hash*
           :get-char-class :long-vowel-modifier-p
           :*katakana-regex* :*hiragana-regex* :*kanji-regex* :test-word
           :hash-from-list :voice-char :simplify-ngrams :normalize
           :split-by-regex :basic-split :mora-length :count-char-class
           :as-hiragana :as-katakana :sequential-kanji-positions
           :*undakuten-hash* :*dakuten-hash* :*handakuten-hash*
           :kanji-prefix :kanji-mask :kanji-regex :kanji-match :kanji-cross-match
           :unrendaku :rendaku :destem :geminate
           :collect-char-class :*kanji-char-regex* :consecutive-char-groups
           :match-diff :safe-subseq :join
           ))

(defpackage :ichiran/numbers
  (:use :cl :ichiran/characters)
  (:export
   #:*digit-kanji-default*
   #:*digit-kanji-legal*
   #:*power-kanji*
   #:number-to-kanji
   #:parse-number
   #:number-to-kana
   #:not-a-number))

(defpackage :ichiran/conn
  (:use :cl :postmodern)
  (:export :get-spec :with-db :let-db
           :*connection* :*connections* :*connection-env-var*
           :def-conn-var :switch-conn-vars
           :load-settings :with-log :load-connection-from-env
           :cache  :defcache :all-caches :init-all-caches
           :get-cache :init-cache :reset-cache :ensure
           :*debug* :dp
           ))

(defpackage :ichiran/dict
  (:use :cl :postmodern :split-sequence
        :ichiran/characters :ichiran/conn :ichiran/numbers)
  (:import-from :postmodern :drop-table)
  (:export :simple-segment :dict-segment :word-info-from-text
           :word-info :word-info-type :word-info-text
           :word-info-kana :word-info-score :map-word-info-kana
           :word-info-str :word-info-components :word-info-alternative
           :word-info-start :word-info-end :word-info-counter :word-info-skipped
           :word-info-json :word-info-gloss-json
           :init-suffixes :init-suffixes-running-p
           :node-text :get-kanji-words
           :find-word-info :find-word-info-json :simple-word-info
           :process-hints :strip-hints
           :find-kanji-for-pattern
           :match-glosses))

(defpackage :ichiran/custom
  (:use :cl :postmodern :split-sequence
        :ichiran/characters :ichiran/conn)
  (:export :load-custom-data :get-custom-data :slurp))

(uiop:define-package :ichiran
  (:use :cl :ichiran/characters :ichiran/conn :ichiran/dict)
  (:import-from :ichiran/dict :csv-hash)
  (:export :romanize :romanize* :romanize-word-info
           :romanize-word :romanize-word-geo
           :generic-romanization :generic-hepburn :kana-table
           :simplified-hepburn :simplifications
           :*hepburn-basic* :*hepburn-simple* :*hepburn-passport*
           :*hepburn-traditional* :*hepburn-modified*
           :kunrei-siki :*kunrei-siki*
           :*default-romanization-method*
           :romaji-kana :romaji-suggest))

(defpackage :ichiran/kanji
  (:use :cl :postmodern :ichiran/conn :ichiran :ichiran/characters :ichiran/dict)
  (:import-from :postmodern :drop-table)
  (:export
   :kanji-info-json
   :match-readings
   :kanji-word-stats
   :get-readings
   :get-normal-readings
   :match-readings-json
   :query-kanji-json))

(uiop:define-package :ichiran/all
    (:use :cl)
    (:use-reexport :ichiran/characters :ichiran/numbers
                   :ichiran/conn :ichiran/dict :ichiran :ichiran/kanji))

(defpackage :ichiran/test
  (:use :cl :ichiran/all :lisp-unit)
  (:export :run-all-tests :*test-thread-count* :run-parallel-tests))
