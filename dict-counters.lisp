(in-package :ichiran/dict)

(def-conn-var *counter-cache* nil)

(defgeneric counter-join (counter n number-kana counter-kana)
  (:documentation "Construct counter kana text")
  (:method (counter n number-kana counter-kana)
    (declare (ignore counter n))
    (concatenate 'string number-kana counter-kana)))

(defclass counter-text ()
  ((text :reader counter-text :initarg :text)
   (kana :reader counter-kana :initarg :kana)
   (number-text :reader number-text :initarg :number-text)
   (number :reader number-value)
   (source :reader source :initform nil :initarg :source)
   (ordinalp :reader ordinalp :initarg :ordinalp)
   ))

(defgeneric verify (counter unique)
  (:documentation "Verify if counter is valid")
  (:method (counter unique)
    (declare (ignore counter))
    unique))

(defmethod initialize-instance :after ((obj counter-text) &key)
  (setf (slot-value obj 'number) (parse-number (number-text obj))))

(defmethod print-object ((obj counter-text) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a[~a] ~a" (number-text obj) (number-value obj) (counter-text obj))))

(defmethod text ((obj counter-text))
  (concatenate 'string (number-text obj) (counter-text obj)))

(defmethod get-kanji ((obj counter-text))
  (concatenate 'string (number-to-kanji (number-value obj)) (counter-text obj)))

(defmethod get-kana ((obj counter-text))
  (let ((n (number-value obj)))
    (counter-join obj n (number-to-kana n :separator *kana-hint-space*)
                  (copy-seq (counter-kana obj)))))

(defmethod word-type ((obj counter-text))
  (if (> (count-char-class (text obj) :kanji-char) 0) :kanji :kana))

(defmethod common ((obj counter-text))
  (if (source obj) (common (source obj)) 0))

(defmethod seq ((obj counter-text))
  (and (source obj) (seq (source obj))))

(defmethod ord ((obj counter-text))
  (if (source obj) (ord (source obj)) 0))

(defmethod word-conjugations ((obj counter-text)) nil)

(defmethod word-conj-data ((obj counter-text)) nil)

(defmethod root-p ((obj counter-text)) t)

(defmethod counter-join ((obj counter-text) n number-kana counter-kana
                         &aux (digit (mod n 10))
                           (head (gethash (char counter-kana 0) *char-class-hash*)))                   
  (case digit
    (1 (case head
         ((:ka :ki :ku :ke :ko
           :sa :shi :su :se :so
           :ta :chi :tsu :te :to)
          (geminate number-kana))
         ((:ha :hi :fu :he :ho)
          (geminate number-kana)
          (rendaku counter-kana :handakuten t))))
    (3 (case head
         ((:ha :hi :fu :he :ho)
          (rendaku counter-kana))))
    (6 (case head
         ((:ka :ki :ku :ke :ko)
          (geminate number-kana))
         ((:ha :hi :fu :he :ho
           :pa :pi :pu :pe :po)
          (geminate number-kana)
          (rendaku counter-kana :handakuten t))))
    (8 (case head
         ((:ka :ki :ku :ke :ko
           :sa :shi :su :se :so
           :ta :chi :tsu :te :to
           :pa :pi :pu :pe :po)
          (geminate number-kana))
         ((:ha :hi :fu :he :ho)
          (geminate number-kana)
          (rendaku counter-kana :handakuten t))))
    (0 (unless (zerop (mod n 100))
         (case head
           ((:ka :ki :ku :ke :ko
             :sa :shi :su :se :so
             :ta :chi :tsu :te :to
             :pa :pi :pu :pe :po)
            (geminate number-kana))
           ((:ha :hi :fu :he :ho)
            (geminate number-kana)
            (rendaku counter-kana :handakuten t))))))
  (call-next-method))

(defclass number-text (counter-text)
  ((text :initform "")
   (kana :initform "")
   (ordinalp :initform nil)))

(defmethod get-kana ((obj number-text))
  (number-to-kana (number-value obj) :separator *kana-hint-space*))

(defun init-counters ()
  (setf *counter-cache* (make-hash-table :test 'equal))
  (labels ((add-args (text &rest args) (push args (gethash text *counter-cache* nil))))
    (add-args "" 'number-text)))

(defun find-counter (number counter &key (unique t))
  (let ((counter-args (gethash counter *counter-cache*)))
    (when counter-args
      (loop for args in counter-args
         for counter-obj = (handler-case
                               (apply 'make-instance `(,@args :number-text ,number))
                             (not-a-number () nil))
         when (and counter-obj (verify counter-obj unique))
           collect counter-obj))))
