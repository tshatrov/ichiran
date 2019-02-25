;; detransliteration module

(in-package #:ichiran)

(defstruct (rmap-item (:conc-name rmi-)) text kana next)

(csv-hash *romaji-kana* ("data/romaji-map.csv" :relative :ichiran)
          ((text kana next) text (make-rmap-item :text text :kana kana :next next))
          (val val))
