(in-package #:ichiran/numbers)

(defparameter *digit-kanji-default* "〇一二三四五六七八九")

(defparameter *digit-kanji-legal* "〇壱弐参四五六七八九拾")

(defparameter *power-kanji* "一十百千万   億   兆   京")

(defun number-to-kanji (n &rest keys &key (digits *digit-kanji-default*) (powers *power-kanji*) (1sen nil))
  (assert (and (integerp n) (>= n 0)))
  (if (= n 0) (string (char digits 0))
      (loop with mp = 1 and mc
         for i from 0 below (length powers)
         for p = 1 then (* p 10)
         for c = (char powers i)
         while (<= p n)
         when (char-not-equal c #\Space) do (setf mp p mc c)
         finally
           (return
             (if (= mp 1)
                 (string (char digits n))
                 (multiple-value-bind (qt rem) (floor n mp)
                   (format nil "~a~c~a"
                           (if (and (= qt 1) (<= mp (if 1sen 100 1000))) ""
                               (apply #'number-to-kanji qt :1sen t keys))
                           mc
                           (if (= rem 0) "" (apply #'number-to-kanji rem keys)))))))))

